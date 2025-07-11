library(tidyverse)
library(rlang)

validate_subclass <- function(x, subclass,
                              argname = to_lower_ascii(subclass),
                              x_arg = caller_arg(x),
                              env = parent.frame(),
                              call = caller_env()) {

  if (inherits(x, subclass)) {
    return(x)
  }
  if (!is_scalar_character(x)) {
    stop_input_type(x, as_cli("either a string or a {.cls {subclass}} object"), arg = x_arg)
  }

  # Try getting class object directly
  name <- paste0(subclass, camelize(x, first = TRUE))
  obj <- find_global(name, env = env)
  if (inherits(obj, subclass)) {
    return(obj)
  }

  # Try retrieving class via constructors
  name <- snakeize(name)
  obj <- find_global(name, env = env, mode = "function")
  if (is.function(obj)) {
    obj <- try_fetch(
      obj(),
      error = function(cnd) {
        # replace `obj()` call with name of actual constructor
        cnd$call <- call(name)
        cli::cli_abort(
          "Failed to retrieve a {.cls {subclass}} object from {.fn {name}}.",
          parent = cnd, call = call
        )
      })
  }
  # Position constructors return classes directly
  if (inherits(obj, subclass)) {
    return(obj)
  }
  # Try prying the class from a layer
  if (inherits(obj, "Layer")) {
    obj <- switch(
      subclass,
      Geom = obj$geom,
      Stat = obj$stat,
      NULL
    )
  }
  if (inherits(obj, subclass)) {
    return(obj)
  }
  cli::cli_abort("Can't find {argname} called {.val {x}}.", call = call)
}

# helper function to adjust the draw_key slot of a geom
# if a custom key glyph is requested
set_draw_key <- function(geom, draw_key = NULL) {
  if (is.null(draw_key)) {
    return(geom)
  }
  if (is.character(draw_key)) {
    draw_key <- paste0("draw_key_", draw_key)
  }
  draw_key <- match.fun(draw_key)

  ggproto(NULL, geom, draw_key = draw_key)
}

cleanup_mismatched_data <- function(data, n, fun) {
  if (vec_duplicate_any(names(data))) {
    data <- data[unique0(names(data))]
  }

  failed <- !lengths(data) %in% c(0, 1, n)
  if (!any(failed)) {
    return(data)
  }

  failed <- names(data)[failed]
  cli::cli_warn(
    "Failed to apply {.fn {fun}} for the following \\
    aesthetic{?s}: {.field {failed}}."
  )

  data[failed] <- NULL
  data
}

make_constructor <- function(x, ...) {
  UseMethod("make_constructor")
}

#' @export
#' @rdname make_constructor
make_constructor.Geom <- function(x, ..., checks = exprs(), omit = character(),
                                  env = caller_env()) {

  # Check that we can independently find the geom
  geom <- gsub("^geom_", "", ggplot2:::snake_class(x))
  validate_subclass(geom, "Geom", env = env)

  # Split additional arguments into required and extra ones
  args <- enexprs(...)
  fixed_fmls_names <- c("mapping", "data", "stat", "position", "...",
                        "na.rm", "show.legend", "inherit.aes")
  extra_args <- setdiff(names(args), fixed_fmls_names)
  if ("geom" %in% extra_args) {
    cli::cli_abort("{.arg geom} is a reserved argument.")
  }

  # Fill in values for parameters from draw functions
  known_params <-
    unique(c(names(args), fixed_fmls_names, "flipped_aes", x$aesthetics(), omit))
  missing_params <- setdiff(x$parameters(), known_params)
  if (length(missing_params) > 0) {
    draw_args <- ggplot2:::ggproto_formals(x$draw_panel)
    if ("..." %in% names(draw_args)) {
      draw_args <- ggplot2:::ggproto_formals(x$draw_group)
    }
    params <- intersect(missing_params, names(draw_args))
    extra_args <- c(extra_args, params)
    for (param in params) {
      if (!identical(draw_args[[param]], quote(expr = ))) {
        args[param] <- draw_args[param]
      }
    }
    extra_args <- intersect(extra_args, names(args))
    missing_params <- setdiff(missing_params, names(args))
    if (length(missing_params) > 0) {
      cli::cli_warn(
        "In {.fn geom_{geom}}: please consider providing default values for: \\
        {missing_params}."
      )
    }
  }

  # Build function formals
  fmls <- pairlist2(
    mapping  = args$mapping,
    data     = args$data,
    stat     = args$stat %||% "identity",
    position = args$position %||% "identity",
    `...` = missing_arg(),
    !!!args[extra_args],
    na.rm    = args$na.rm %||% FALSE,
    show.legend = args$show.legend %||% NA,
    inherit.aes = args$inherit.aes %||% TRUE
  )

  # Construct call for the 'layer(params)' argument
  params <- exprs(!!!syms(c("na.rm", extra_args)), .named = TRUE)
  params <- call2("list2", !!!params, quote(...))

  # Construct rest of 'layer()' call
  layer_args <- syms(setdiff(fixed_fmls_names, c("...", "na.rm")))
  layer_args <- append(layer_args, list(geom = geom), after = 2)
  layer_args <- exprs(!!!layer_args, params = !!params, .named = TRUE)
  body <- call2("layer", !!!layer_args)

  # Prepend any checks
  if (length(exprs) > 0) {
    lang <- vapply(checks, is_call, logical(1))
    if (!all(lang)) {
      cli::cli_abort(
        "{.arg checks} must be a list of calls, such as one constructed \\
        with {.fn rlang::exprs}."
      )
    }
  }
  body <- call2("{", !!!checks, body)

  # We encapsulate rlang::list2
  new_env <- new_environment(list(list2 = list2), env)

  new_function(fmls, body, new_env)
}

#' @export
#' @rdname make_constructor
make_constructor.Stat <- function(x, ..., checks = exprs(), omit = character(),
                                  env = caller_env()) {
  # Check that we can independently find the stat
  stat <- gsub("^stat_", "", ggplot2:::snake_class(x))
  validate_subclass(stat, "Stat", env = env)

  # Split additional arguments into required and extra ones
  args <- enexprs(...)
  fixed_fmls_names <- c("mapping", "data", "geom", "position", "...",
                        "na.rm", "show.legend", "inherit.aes")
  extra_args <- setdiff(names(args), fixed_fmls_names)
  if ("stat" %in% extra_args) {
    cli::cli_abort("{.arg stat} is a reversed argument.")
  }

  known_params <-
    unique(c(names(args), fixed_fmls_names, "flipped_aes", x$aesthetics(), omit))
  missing_params <- setdiff(x$parameters(), known_params)

  # Fill in missing parameters from the compute methods
  if (length(missing_params) > 0) {
    compute_args <- ggplot2:::ggproto_formals(x$compute_panel)
    if ("..." %in% names(compute_args)) {
      compute_args <- ggplot2:::ggproto_formals(x$compute_group)
    }
    params <- intersect(missing_params, names(compute_args))
    extra_args <- c(extra_args, params)
    for (param in params) {
      if (!identical(compute_args[[param]], missing_arg())) {
        args[param] <- compute_args[param]
      }
    }
    extra_args <- intersect(extra_args, names(args))
    missing_params <- setdiff(missing_params, names(args))
    if (length(missing_params) > 0) {
      cli::cli_warn(
        "In {.fn stat_{stat}}: please consider providing default values for: \\
        {missing_params}."
      )
    }
  }

  # Build function formals
  fmls <- pairlist2(
    mapping  = args$mapping,
    data     = args$data,
    geom     = args$geom %||% cli::cli_abort("{.arg geom} is required."),
    position = args$position %||% "identity",
    `...`    = missing_arg(),
    !!!args[extra_args],
    na.rm = args$na.rm %||% FALSE,
    show.legend = args$show.legend %||% NA,
    inherit.aes = args$inherit.aes %||% TRUE
  )

  # Construct params for the `layer(params)` argument
  params <- exprs(!!!syms(c("na.rm", extra_args)), .named = TRUE)
  params <- call2("list2", !!!params, quote(...))

  # Construct rest of `layer()` call
  layer_args <- syms(setdiff(fixed_fmls_names, c("...", "na.rm")))
  layer_args <- append(layer_args, list(stat = stat), after = 3)
  layer_args <- exprs(!!!layer_args, params = !!params, .named = TRUE)
  body <- call2("layer", !!!layer_args)

  # Prepend any checks
  if (length(exprs) > 0) {
    lang <- vapply(checks, is_call, logical(1))
    if (!all(lang)) {
      cli::cli_abort(
        "{.arg checks} must be a list of calls, such as one constructed \\
        with {.fn rlang::exprs}."
      )
    }
  }
  body <- call2("{", !!!checks, body)

  # We encapsulate rlang::list2
  new_env <- new_environment(list(list2 = list2), env)

  new_function(fmls, body, new_env)
}


StatManual <- ggproto(
  "StatManual", Stat,

  setup_params = function(data, params) {
    params[["fun"]] <- ggplot2:::allow_lambda(params[["fun"]])
    ggplot2:::check_function(params[["fun"]], arg = "fun")
    params
  },

  compute_group = function(data, scales, fun = identity, args = list()) {
    ggplot2:::as_gg_data_frame(inject(fun(data, !!!args)))
  }
)

stat_manual <- make_constructor(StatManual, geom = "point")

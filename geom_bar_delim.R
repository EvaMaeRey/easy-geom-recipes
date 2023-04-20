toy_survey <- data.frame(multiple_response = c('excitement;skepticism', 'skepticism;curiousity'))




compute_panel_bar_delim <- function(data,
                                     scales){

  data$x %>%
    paste(collapse = ";") %>%
    str_split_1(";") %>%
    data.frame(x = .) %>%
    mutate(y = 1)

    # add an additional column called label
    # the geom we inherit from requires the label aesthetic

}


compute_group_bar_delim <- function(data, scales){

  data %>%
    count(x) %>%
    rename(y = n)

}

toy_survey %>%
  rename(x = multiple_response) %>%
  compute_panel_bar_delim() %>%
  compute_group_bar_delim()


StatBardelim <- ggplot2::ggproto(
  `_class` = "StatBardelim",
  `_inherit` = ggplot2::Stat,
  required_aes = c("x"),
  compute_panel = compute_panel_bar_delim,
  compute_goup = compute_group_bar_delim
)

geom_bar_delim <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatBardelim,  # proto object from Step 2
    geom = ggplot2::GeomCol,  # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

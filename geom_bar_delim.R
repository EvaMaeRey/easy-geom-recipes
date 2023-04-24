toy_survey <- data.frame(q1 = c('excitement;skepticism', 'skepticism;curiousity'))


toy_survey %>%
  pull(q1) %>%
  paste(collapse = ";") %>%
  str_split_1(";") %>%
  data.frame(x = .) %>%
  count(x) %>%
  mutate(y = n) %>%
  ggplot() +
  aes(x = x, y = y) +
  geom_col()

layer_data(last_plot())



compute_panel_bar_delim <- function(data,
                                     scales){

  data$responses %>%
    paste(collapse = ";") %>%
    str_split_1(";") %>%
    data.frame(cats = .) %>%
    count(cats) %>%
    rename(num_responding = n)

    # add an additional column called label
    # the geom we inherit from requires the label aesthetic

}


# compute_group_bar_delim <- function(data, scales){
#
#   data %>%
#     count(x) %>%
#     rename(y = n)
#
# }

toy_survey %>%
  rename(responses = q1) %>%
  compute_panel_bar_delim() %>%
  compute_group_bar_delim()

setup_data_function <- function(data, params){

  if(data$group[1] == -1){
    nrows <- nrow(data)
    data$group <- seq_len(nrows)
  }

  data

}



StatBardelim <- ggplot2::ggproto(
  `_class` = "StatBardelim",
  `_inherit` = ggplot2::Stat,
  # required_aes = c("responses"),
  compute_panel = compute_panel_bar_delim,
  default_aes = ggplot2::aes(x = after_stat(cats), y = after_stat(num_responding), group = after_stat(cats)),
  # setup_data = setup_data_function
  # compute_goup = compute_group_bar_delim
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


toy_survey %>%
  ggplot() +
  aes(responses = q1) +
  geom_bar_delim()

#' Add a time-series plot to a plot list.
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for time-series plots
#' @param time bare name of the time-axis column
#' @param value bare name of the value column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_ts = function(.pl, data, time, value, group, ...) {
  group = rlang::enquo(group) 
  time = rlang::enquo(time) 
  value = rlang::enquo(value) 

  data = split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i = as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] = .pl[[i]] + geom_line(data = data[[i]], 
      aes_string(x=rlang::quo_text(time), y=rlang::quo_text(value)))
  }
  for (name in names(.pl)) {
    i = as.character(name)
    for (obj in list(...)) .pl[[i]] = .pl[[i]] + obj
  }
  return(.pl)
}

#' Add bounds of a time-series to a plot list
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for time-series plots
#' @param time bare name of the time-axis column
#' @param lb bare name of the lower bound column.
#' @param ub bare name of the upper bound column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_ts_bounds = function(.pl, data, time, lb, ub, group, ...) {
  group = rlang::enquo(group) 
  time = rlang::enquo(time) 
  lb = rlang::enquo(lb) 
  ub = rlang::enquo(ub) 

  data = split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i = as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] = .pl[[i]] + geom_ribbon(data = data[[i]], 
      aes_string(x=rlang::quo_text(time), 
        ymin=rlang::quo_text(lb), ymax=rlang::quo_text(ub)), 
      alpha = 0.3)
  }
  for (name in names(.pl)) {
    i = as.character(name)
    for (obj in list(...)) .pl[[i]] = .pl[[i]] + obj
  }
  return(.pl)
}


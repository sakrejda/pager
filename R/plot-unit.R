
#' Add point estimates for units to a plot list.
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for point estimate plots
#' @param unit bare name of the unit-axis column
#' @param value bare name of the value column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_unit = function(.pl, data, unit, value, group, ...) {
  group = rlang::enquo(group) 
  unit = rlang::enquo(unit) 
  value = rlang::enquo(value) 

  data = split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i = as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] = .pl[[i]] + geom_point(data = data[[i]], 
      aes_string(x=rlang::quo_text(value), y=rlang::quo_text(unit)))
  }
  for (name in names(.pl)) {
    i = as.character(name)
    for (obj in list(...)) .pl[[i]] = .pl[[i]] + obj
  }
  return(.pl)
}

#' Add bounds estimates for units to a plot list.
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for point estimate plots
#' @param unit bare name of the unit-axis column
#' @param value bare name of the value column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_unit_bounds = function(.pl, data, unit, lb, ub, group, ...) {
  group = rlang::enquo(group) 
  unit = rlang::enquo(unit) 
  lb = rlang::enquo(lb) 
  ub = rlang::enquo(ub) 

  data = split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i = as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] = .pl[[i]] + geom_errorbarh(data = data[[i]], 
      aes_string(x=rlang::quo_text(lb), y=rlang::quo_text(unit),
                 xmin = rlang::quo_text(lb), xmax = rlang::quo_text(ub)))
  }
  for (name in names(.pl)) {
    i = as.character(name)
    for (obj in list(...)) .pl[[i]] = .pl[[i]] + obj
  }
  return(.pl)
}



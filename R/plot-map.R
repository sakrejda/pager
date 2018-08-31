
#' Map point estimates for units in a plot list.
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for point estimate plots
#' @param map list of map df objects to merge data to
#' @param unit bare name of the unit column (in map and data)
#' @param value bare name of the value column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
map_unit = function(.pl, data, map, unit, value, group, ...) {
  group = rlang::enquo(group) 
  unit = rlang::enquo(unit) 
  value = rlang::enquo(value) 

  data = split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i = as.character(name)
    if (is.null(data[[i]]))
      next
    local_data = dplyr::left_join(map[[i]], data[[i]], by = rlang::quo_text(unit))
    .pl[[i]] = .pl[[i]] + geom_polygon(data = local_data,
      aes_string(x='longitude', y= 'latitude', fill = rlang::quo_text(value)), 
      colour = 'black') + coord_fixed()
  }
  for (name in names(.pl)) {
    i = as.character(name)
    for (obj in list(...)) .pl[[i]] = .pl[[i]] + obj
  }
  return(.pl)
}


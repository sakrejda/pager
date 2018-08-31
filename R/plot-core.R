
#' Create a list of named empty plots.  
#'
#' We often want to produce multi-page plots and ggplot offers
#' no straightfoward way to do this.  This is a minimal structure
#' to make that easy.  
#'
#' @param names, the names of each component plots in a vector.  These
#'               will always be used with 'as.character'.
#' @return a list of empty plots.
#' @export
plot_list = function(names) {
  .pl = list()
  for (name in names) {
    .pl[[as.character(name)]] = ggplot()
  }
  return(.pl)
}



#' Add titles to all plots
#'
#' For each plot in .pl, add a `ggtitle` with the provided strings. 
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param titles list, with names matching plot object names.
#' @return plot list with titles added
#' @export
plot_titles = function(.pl, titles) {
  for (name in names(.pl)) {
    i = as.character(name)
    .pl[[i]] = .pl[[i]] + ggtitle(titles[[i]])
  }
  return(.pl)
}




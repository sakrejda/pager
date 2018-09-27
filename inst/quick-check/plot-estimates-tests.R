library(ggplot2); library(dplyr)
output_dir = '/var/www/html/viz'

ar = function(ro, l) {
  o <- vector(mode = 'numeric', length = l)
  for (i in 1:(l-1)) {
    o[i+1] = o[i] * ro + rnorm(1, 0, .2)
  }
  return(o)
}


data = data.frame(
  YEAR = 1990:2014,
  signal = ar(.9,25),
  station = 1
)

for (i in 1:10) {
  data = rbind(data, 
    data.frame(YEAR = 1990:2014,
	       signal = ar(0.9/i, 25),
	       station = i + 1))
}

nrow(data)

bounds = data %>% dplyr::mutate(
  lowest_signal = signal - rnorm(25, 1, 0.02),
  biggest_signal = signal + rnorm(25, 1, 0.2)
)

pt_data = data.frame(
  YEAR = c(1991, 1998, 2002, 2011),
  signal_ub = c(0.5, 0.7, .6, 0.9),
  signal_est = c(0.02, 0.1, -.5, 0.8), 
  signal_lb = c(-0.1, 0.05, -1, -.6),
  station = rep(1, 4)
)

for (i in 1:10) {
  pt_data = rbind(pt_data, data.frame(
    YEAR = c(1991, 1998, 2002, 2011),
    signal_ub = rnorm(4, .5, .2),
    signal_est = rnorm(4, 0, .2),
    signal_lb = rnorm(4, -.3, 0.2),
    station = i + 1))
}

data[data$YEAR == 2011,'signal'] <- NA
bounds[bounds$YEAR == 2011,'lowest_signal'] <- NA

pl <- plot_list(1:11) %>% 
  plot_ts(data, YEAR, signal, station) %>%
  plot_ts_bounds(bounds, YEAR, lowest_signal, biggest_signal, 
    station, theme_minimal()) %>%
  plot_pt(pt_data, YEAR, signal_est, station) %>%
  plot_pt_bounds(pt_data, YEAR, signal_lb, signal_ub, station)

pdf(file.path(output_dir, "ts-examples.pdf"))
for (p in pl) print(p); dev.off()


countries <- c("US", "UK", "Ukraine")
unit_data = data.frame(unit = letters, cpr = plogis(rnorm(26)),
    country = sample(countries, size = 26, replace=TRUE)) %>%
  dplyr::mutate(lb = plogis(qlogis(cpr) - rexp(26, rate = 10)), 
		ub = plogis(qlogis(cpr) + rexp(26, rate = 10)))


pl_units <- plot_list(countries) %>% 
	plot_unit(unit_data, unit, cpr, country) %>%  
	plot_unit_bounds(unit_data, unit, lb, ub, country, 
	theme_minimal())

pdf(file.path(output_dir, "unit-examples.pdf"))
for (p in pl_units) print(p); dev.off()




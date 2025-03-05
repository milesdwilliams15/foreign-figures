### Creates my_mean_ci() ----
# This is a function useful for stat_summary() with {ggplot2}

my_mean_ci <- function(y, ci = .95) {
  ybar <- mean(y, na.rm = T)
  yboot <- replicate(
    n = 1000,
    expr = mean(sample(y, length(y), T), na.rm = T)
  )
  data.frame(
    y = ybar,
    ymin = quantile(yboot, .5 - ci / 2),
    ymax = quantile(yboot, .5 + ci / 2)
  )
}
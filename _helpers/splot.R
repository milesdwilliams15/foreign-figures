

# load custom ggplot theme/logo object from project root
source(here::here("my_theme.R"))

# create smart plot function
splot <- function(data, ...,
                  type = "scatter",
                  hjust = 0.5, vjust = 0.5, 
                  xangle = FALSE,
                  color = "black", fill = "gray",
                  shape = 19, linetype = 1,
                  size = 2, method = "gam", formula = y ~ s(x),
                  title = NULL, subtitle = NULL,
                  xlab = NULL, ylab = NULL) {
  
  ## base plot with user-supplied aesthetics
  p <- ggplot(data, aes(...))
  
  ## add layers conditionally based on selected type(s)
  if ("scatter" %in% type) p <- p + geom_point(color = color, shape = shape, size = size)
  if ("whisker" %in% type) p <- p + geom_pointrange(color = color, shape = shape, size = size, linetype = linetype) + geom_vline(xintercept = 0, lty = 2)
  if ("vwhisker" %in% type)p <- p + geom_pointrange(color = color, shape = shape, size = size, linetype = linetype) + geom_hline(yintercept = 0, lty = 2)
  if ("column" %in% type)  p <- p + geom_col(fill = fill) 
  if ("hist" %in% type)    p <- p + geom_histogram(fill = fill)
  if ("smooth" %in% type)  p <- p + stat_smooth(color = color, fill = fill, linetype = linetype, method = method, formula = formula)
  if ("line" %in% type)    p <- p + geom_line(color = color, linetype = linetype, linewidth = 1)
  if ("text" %in% type)    p <- p + geom_text(fontface = "bold", hjust = hjust, vjust = vjust, color = color)

  ## return plot with custom theme and labels
  p <- p + 
    my_theme() + 
    labs(title = title, subtitle = subtitle, x = xlab, y = ylab,
           caption = logo)
  
  ## update angle of x-axis?
  if (xangle) p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  suppressWarnings(print(p))
}
    
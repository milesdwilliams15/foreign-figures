

# load custom ggplot theme/logo object from project root
source(here::here("my_theme.R"))

# create smart plot function
splot <- function(data, ...,
                  type = "point",
                  hjust = 0.5, vjust = 0.5, 
                  pal = "qualitative",
                  aes = "color",
                  xangle = FALSE,
                  method = "gam", formula = y ~ s(x),
                  title = NULL, subtitle = NULL,
                  xlab = NULL, ylab = NULL) {
  
  ## set global default for all future geom_col and _histogram calls
  update_geom_defaults("col", list(fill = "gray"))
  update_geom_defaults("histogram", list(fill = "gray"))
  
  ## base plot with user-supplied aesthetics
  p <- ggplot(data, aes(...))
  
  ## add layers conditionally based on selected type(s)
  if ("point" %in% type) p <- p + geom_point()
  if ("whisker" %in% type) p <- p + geom_pointrange() + geom_vline(xintercept = 0, lty = 2)
  if ("vwhisker" %in% type)p <- p + geom_pointrange() + geom_hline(yintercept = 0, lty = 2)
  if ("column" %in% type)  p <- p + geom_col() 
  if ("hist" %in% type)    p <- p + geom_histogram()
  if ("smooth" %in% type)  p <- p + stat_smooth(method = method, formula = formula)
  if ("line" %in% type)    p <- p + geom_line(linewidth = 1)
  if ("text" %in% type)    p <- p + geom_text(fontface = "bold", hjust = hjust, vjust = vjust)

  ## return plot with custom theme and labels
  p <- p + 
    my_theme() + 
    labs(title = title, subtitle = subtitle, x = xlab, y = ylab,
           caption = logo) +
    ggpal(type = pal, aes = aes)
  
  ## update angle of x-axis?
  if (xangle) p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  suppressWarnings(print(p))
}


# load custom ggplot theme/logo object from project root
source(here::here("my_theme.R"))

# create smart plot function
splot <- function(data, ...,
                  type = "point",
                  geom_args = list(
                    color = "black",
                    fill = "gray",
                    shape = 19,
                    size = ifelse("point" %in% type, 1.5, 0.25 * 1.5),
                    linewidth = 1,
                    linetype = 1,
                    alpha = 1
                  ),
                  hline = NULL,
                  vline = NULL,
                  hjust = 0.5, vjust = 0.5, 
                  pal = "qualitative",
                  aes = "color",
                  xangle = FALSE,
                  method = "gam", formula = y ~ s(x),
                  se = TRUE,
                  facet = NULL,
                  scales = "fixed",
                  title = NULL, subtitle = NULL,
                  xlab = NULL, ylab = NULL) {
  
  ## set global default for all future geom calls
  for (i in 1:length(type)) update_geom_defaults(type[i], geom_args)
  
  ## base plot with user-supplied aesthetics
  p <- ggplot(data, aes(...))
  
  ## add layers conditionally based on selected type(s)
  if ("point" %in% type)      p <- p + geom_point()
  if ("pointrange" %in% type) p <- p + geom_pointrange() 
  if ("col" %in% type)        p <- p + geom_col() 
  if ("histogram" %in% type)  p <- p + geom_histogram()
  if ("smooth" %in% type)     p <- p + stat_smooth(alpha = 0.3, method = method, formula = formula, se = se)
  if ("line" %in% type)       p <- p + geom_line(linewidth = 1)
  if ("text" %in% type)       p <- p + geom_text(fontface = "bold", hjust = hjust, vjust = vjust, size = geom_args$size * 12)

  ## add vertical or horizontal lines
  if (!is.null(hline)) p <- p + geom_hline(yintercept = hline, linetype = 2)
  if (!is.null(vline)) p <- p + geom_vline(xintercept = vline, linetype = 2)
  
  ## facet the plot
  if (!is.null(facet)) p <- p + facet_wrap(facet, scales = scales)
  
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


# splot(mtcars, hp, mpg, type = c("point", "smooth"))
# mtcars |>
#   group_by(cyl) |>
#   summarize(
#     mean = mean(mpg),
#     lower = mean - 1.96 * sd(mpg) / sqrt(n()),
#     upper = mean + 1.96 * sd(mpg) / sqrt(n())
#   ) |>
#   splot(
#     mean, cyl,
#     xmin = lower, xmax = upper,
#     type = "pointrange",
#     vline = 0
#   )

splot(
  mtcars,
  hp, mpg, 
  type = c("point", "smooth"), 
  formula = y ~ x,
  facet = ~ cyl,
  scales = "free_x"
)

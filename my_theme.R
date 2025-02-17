
# -------------------------------------------------------------------------
# Create a my_theme() function
# -------------------------------------------------------------------------


## My plotting theme ----
my_theme <- function() {
  ggthemes::theme_fivethirtyeight(
    base_family = "mono"
  ) +
    theme(
      plot.title.position = "plot",
      panel.grid.major = element_line(
        linewidth = 0.1,
        color = "navy"
      ),
      plot.caption = ggtext::element_markdown(),
      axis.title.x = element_text(
        hjust = .5,
        size = 12
      ),
      axis.title.y = element_text(
        hjust = .5,
        size = 12
      )
    ) 
}

## My custom logo ----
logo <- paste0(
  "<img src='",
  here::here("ff_logo_wide.png"),
  "' height=15 />"
)
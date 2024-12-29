
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
      plot.caption = ggtext::element_markdown()
    ) 
}

## My custom logo ----
logo <- paste0(
  "<img src='",
  here::here("ff_logo_wide.png"),
  "' height=15 />"
)
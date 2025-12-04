
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
        color = "gray50"
      ),
      plot.caption = ggtext::element_markdown(),
      plot.caption.position = "plot",
      axis.title.x = element_text(
        hjust = .5,
        size = 12,
        face = "bold"
      ),
      axis.title.y = element_text(
        hjust = .5,
        size = 12,
        face = "bold"
      ),
      plot.subtitle = element_text(
        face = "bold"
      ),
      axis.text = element_text(
        face = "bold"
      ),
      legend.title.position = "top",
      legend.title = element_text(
        hjust = .5,
        face = "bold.italic"
      ),
      legend.text = element_text(
        face = "bold"
      )
    ) 
}

## My custom logo ----
logo <- paste0(
  "<img src='",
  here::here("ff_logo_wide.png"),
  "' height=15 />"
)
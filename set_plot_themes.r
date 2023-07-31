

# Define a theme for the 2021 global report

theme_gtbr_2021 <- function(base_size=14, base_family="", axis_text_size=8) {

  faint_gray <- "#CCCCCC"
  gray <- "#BCBCBC"
  charcoal <- "#222222"

  theme(

    #Text format:
    plot.margin = margin(30,5,30,5),

    #Legend format
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(family=base_family,
                               size=base_size,
                               colour=charcoal),

    #Axis format
    axis.title = element_text(family=base_family,
                              size=base_size,
                              colour=charcoal),

    axis.text = element_text(family=base_family,
                             size=axis_text_size,
                             colour=charcoal),

    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),

    axis.ticks.x = element_line(colour = gray),
    axis.ticks.y = element_blank(),

    axis.line.x = ggplot2::element_line(colour=gray, linewidth = 0.25),
    axis.line.y = element_blank(),

    #Grid lines
    panel.grid.minor = ggplot2::element_blank(),

    panel.grid.major.y = ggplot2::element_line(colour = faint_gray, linewidth = 0.1),
    panel.grid.major.x = ggplot2::element_blank(),

    # Have plain background in headings of facet sub-plots
    strip.background = element_rect(fill="white", colour=NA),
    strip.text = element_text(family=base_family,
                              size=base_size,
                              face="bold",
                              colour=charcoal),

    #Blank background
    panel.background = ggplot2::element_blank(),

  )
}



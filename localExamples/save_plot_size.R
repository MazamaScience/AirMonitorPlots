## save_plot_size.R
#
# A quick and dirty function to easily save a plot at different square sizes
# and resolutions for testing the third column in the `tarnayPlot()` legend
#
# NOTE: the defualt dir will need to be changed for different users


save_plot_size <- function(plot,
                           dir = "~/test_plots/",
                           prefix = "thirdcol",
                           dpi = 100,
                           sq = 8,
                           out = "png") {

  title <- paste0(
    prefix,
    "-sq", sq,
    "-dpi", dpi
  )

  plot <- plot + ggplot2::labs(title = title)

  ggplot2::ggsave(
    paste0(dir, title, ".", out),
    plot = plot,
    dpi = dpi,
    width = sq, height = sq,
    scale = 1
  )

}

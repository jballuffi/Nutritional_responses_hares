library(ggplot2)


themepoints <- theme(text = element_text(family = "serif"),
                     title = element_text(family = "serif"),
                     axis.title = element_text(size=14, family = "serif"),
                     axis.text = element_text(size=12, family = "serif"),
                     legend.position = "right",
                     legend.key = element_blank(),
                     #legend.title = element_blank(),
                     panel.background = element_blank(),
                     strip.background = element_blank(),
                     axis.line.x.top = element_blank(),
                     axis.line.y.right = element_blank(),
                     axis.line.x.bottom = element_line(size=.5),
                     axis.line.y.left = element_line(size=.5),
                     panel.border = element_blank(),
                     panel.grid.major = element_line(size = 0.5, color = "grey90"))

themepointstop <- theme(text = element_text(family = "serif"),
                     title = element_text(family = "serif"),
                     axis.title = element_text(size=14, family = "serif"),
                     axis.text = element_text(size=12, family = "serif"),
                     legend.position = "top",
                     legend.key = element_blank(),
                     #legend.title = element_blank(),
                     panel.background = element_blank(),
                     strip.background = element_blank(),
                     axis.line.x.top = element_blank(),
                     axis.line.y.right = element_blank(),
                     axis.line.x.bottom = element_line(size=.5),
                     axis.line.y.left = element_line(size=.5),
                     panel.border = element_blank(),
                     panel.grid.major = element_line(size = 0.5, color = "grey90"))




foodcols <- c("0" = "grey40", "1" = "red3")
phasecols <- c("increase" = "purple", "peak" = "green4", "decrease" = "orange", "low" = "black")

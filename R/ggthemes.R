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

themepoints_small <- theme(text = element_text(family = "serif"),
                     title = element_text(family = "serif"),
                     axis.title = element_text(size=13, family = "serif"),
                     axis.text = element_text(size=8, family = "serif"),
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


themepoints_top <- theme(text = element_text(family = "serif"),
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


themethesisright <-theme(text = element_text(family = "serif"),
                         title = element_text(family = "serif"),
                         axis.title = element_text(size=15, family = "serif"),
                         axis.text = element_text(size=10, family = "serif"),
                         legend.position = "right",
                         strip.background = element_blank(),
                         axis.line.x.top = element_blank(),
                         axis.line.y.right = element_blank(),
                         axis.line.x.bottom = element_line(size=.5),
                         axis.line.y.left = element_line(size=.5),
                         legend.key = element_blank(),
                         legend.text = element_text(size=13),
                         panel.background = element_blank())


themethesistop <-theme(text = element_text(family = "serif"),
                       title = element_text(family = "serif"),
                       axis.title = element_text(size=15, family = "serif"),
                       axis.text = element_text(size=10, family = "serif"),
                       legend.position = "top",
                       strip.background = element_blank(),
                       axis.line.x.top = element_blank(),
                       axis.line.y.right = element_blank(),
                       axis.line.x.bottom = element_line(size=.5),
                       axis.line.y.left = element_line(size=.5),
                       legend.key = element_blank(),
                       legend.text = element_text(size=13),
                       panel.background = element_blank())



gridcols <- c("Agnes" = "grey20", "Kloo" = "skyblue3", "Jo" = "orange")

foodcols <- c("Control" = "grey40", "Suppl." = "red3")

#phasecols <- c("increase" = "purple", "peak" = "green4", "decrease" = "orange", "low" = "black")

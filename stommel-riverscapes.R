
# stommel diagram ---------------------------------------------------------
library(ggplot2)
library(scales)
library(grid)

earth <- data.frame(
  space = c(0.01, 0.1, 1.0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000),
  time = c(0.01, 0.1, 1.0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
)

tlabels <- data.frame(
  lab = c("Second", "Minute", "Hour", "Day", "Week", "Month", "Year", "Decade", "Century", "1,000 Y", "10,000 Y"),
  point = c(1, 60, 3600, 86400, 604800, 2628288, 3.1536 * 10^7, 3.1536 * 10^8, 3.1536 * 10^9, 3.1556952 * 10^10, 3.1556952 * 10^11)
)

slabels <- data.frame(
  lab = c("1 cm", "1 m", "100 m", "1 km", "10 km", "100 km", "1,000 km", "10,000 km", "100,000 km"),
  point = c(0.01, 1, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)
)

rlabels <- data.frame(
  lab = c("micro-scale", "meso-scale", "macro-scale","riverscape-scale"),
  point = c(20,1000,35000,1000000)
)

ggplot(earth, aes(x = space, y = time)) +
  geom_line(alpha = 0) +
  labs(y = "Time (s)", x = "Space (m)") +
  theme_minimal() +
  theme(plot.margin = unit(c(2.5, 2.5, 1, 1), "cm")) -> p

p <- p + geom_text(data = tlabels, aes(y = point, label = paste("\U1F814", lab)), x = Inf, hjust = 0, vjust = 0)

p <- p + geom_text(data = slabels, aes(x = point, label = paste("\U1F814", lab), y = Inf), hjust = 0, vjust = 0, angle = 90, size = 3.5)

p <- p +
  geom_vline(xintercept = 20, alpha = 0.5, size = 25, color = "#f6eff7") +
  geom_vline(xintercept = 1000, alpha = 0.3, size = 16, color = "#bdc9e1") +
  geom_vline(xintercept = 35000, alpha = 0.3, size = 21, color = "#67a9cf") +
  geom_vline(xintercept = 1000000, alpha = 0.2, size = 14, color = "#02818a") +
  geom_text(data=rlabels, aes(x=point, y=.001, label=lab), size=4, angle=90, vjust=0, hjust=-.1, color = c("#dfd7e0","#bdc9e1","#67a9cf","#02818a"))

p <- p + 
  scale_y_log10(breaks = 10^(-1:12), labels = trans_format("log10", math_format(10^.x))) + 
  scale_x_log10(breaks = 10^(-1:8), labels = trans_format("log10", math_format(10^.x)))

p

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
# jpeg("stommel.jpg", width =540, height = 900)
tiff("stommel.tiff", units="in", width=6.1, height=9, res=300)
grid.draw(gt)
dev.off()





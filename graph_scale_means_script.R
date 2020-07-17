source("vars.R")
library(googlesheets4)
library(ggplot2)
gs4_deauth()


results <- read_sheet(ss = sheet_id, sheet = worksheet_name, range = range)

categories <- c("Excellent", "Good", "Above Average", "Below Average", "Bad")
color_scale <- c("#00cc00", "#66ff66", "#92d050", "#ffc000", "#ff0000")

results$Category <- factor(results$Category, levels = categories)

bar_chart2 <-
  ggplot(results, aes(x = Scale, y = Mean, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "",
                    values = color_scale,
                    drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(-1, 3)) +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 1),  colour="grey", linetype="dashed") +
  geom_hline(aes(yintercept = -1),  colour="grey", linetype="dashed") +
  geom_text(aes(label=round(Mean, 2)), vjust=1.5) +
  theme(axis.ticks.length.x = unit(0, "points"),
        panel.background = element_rect(fill = "white"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size=11),
        axis.text = element_text(size=11),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_blank())

bar_chart2
ggsave("live_means_bar_chart.pdf", width = 18, height = 12, units = "cm")

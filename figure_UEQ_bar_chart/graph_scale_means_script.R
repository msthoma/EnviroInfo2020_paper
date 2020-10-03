library(googlesheets4)
library(ggplot2)
gs4_deauth()

# import sensitive information such as Google spreadsheet id, range to import, etc.
source("figure_UEQ_bar_chart/vars.R")

# get results from Google spreadsheet
results <-
  read_sheet(ss = sheet_id, sheet = worksheet_name, range = scales_range)

categories <-
  c("Excellent", "Good", "Above Average", "Below Average", "Bad")
color_scale <-
  c("#00cc00", "#66ff66", "#92d050", "#ffc000", "#ff0000") # colors taken from Schrepp et al., 2014

results$Category <- factor(results$Category, levels = categories)

# create bar chart with imported results with ggplot2
bar_chart <-
  ggplot(results, aes(x = Scale, y = Mean, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "",
                    values = color_scale,
                    drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(-1, 3)) +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 1),  colour = "grey", linetype = "dashed") +
  geom_hline(aes(yintercept = -1),  colour = "grey", linetype = "dashed") +
  geom_text(aes(label = round(Mean, 2)), vjust = 1.5) +
  theme(
    axis.ticks.length.x = unit(0, "points"),
    panel.background = element_rect(fill = "white"),
    axis.line.y = element_line(color = "black"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13),
    axis.title.x = element_blank(),
    legend.key.size = unit(0.4, units = "cm"),
    legend.text = element_text(size = 8),
    legend.position = c(0.94, 0.92)
  )

bar_chart

bar_chart_thin <-
  ggplot(results, aes(x = Scale, y = Mean, fill = Category)) +
  geom_bar(stat = "identity") +
  ylab("Score") +
  # expand_limits(x = 0, y = 0) +
  scale_fill_manual(name = "",
                    values = color_scale,
                    drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(breaks = c(0,1,2), expand = expansion(mult = c(0, .05))) +
  geom_text(aes(label = round(Mean, 2)), vjust = 1.5) +
  theme(
    axis.ticks.length.x = unit(5, "points"),
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13),
    axis.title.x = element_blank(),
    legend.key.size = unit(0.4, units = "cm"),
    legend.text = element_text(size = 9)
  )

bar_chart_thin

# save chart as pdf
ggsave(
  "live_means_bar_chart.pdf",
  plot = bar_chart,
  width = 20,
  height = 8,
  units = "cm"
)

# script below exports live stats from google spreadsheet to .tex file, which can be referenced in the latex file for live updating
demographics <-
  read_sheet(ss = sheet_id, sheet = worksheet_name, range = dem_range)
output <- c(
  paste("\\newcommand\\TotalResponses{", demographics$total_responses, "}", sep = ""),
  paste("\\newcommand\\TotalFemale{", demographics$total_female, "}", sep = ""),
  paste("\\newcommand\\TotalMale{", demographics$total_male, "}", sep = "")
)
f <- file("stats.tex")
writeLines(output, f)
close(f)

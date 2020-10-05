library(googlesheets4)
library(ggplot2)
library(cowplot)
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


# Demographics graphs
genders <-
  read_sheet(ss = sheet_id, sheet = worksheet_name, range = genders)

p1 <- ggplot(data=genders, aes(x=Gender, y=Number)) + 
  geom_bar(stat="identity", fill="steelblue", width = 0.4) +
  scale_y_continuous(limits = c(0, 20), breaks = c(0,10,20)) +
  theme_bw() +
  ylab("# of volunteers") +
  geom_text(aes(label = Number), vjust = -1, size = 2)

p1

ages <-
  read_sheet(ss = sheet_id, sheet = worksheet_name, range = ages)

p2 <- ggplot(data=ages, aes(x=Age, y=Number)) + 
  geom_bar(stat="identity", fill="brown") + 
  scale_x_discrete(limits = ages$Age, labels = c("< 18","18-24","25-34","35-44","45-54",bquote("" >= .(55)))) +
  scale_y_continuous(limits = c(0, 20), breaks = c(0,10,20)) +
  theme_bw() +
  ylab("# of volunteers") +
  xlab("Age groups") +
  geom_text(aes(label = Number), vjust = -1, size = 2)

p2

walking_duration <-
  read_sheet(ss = sheet_id, sheet = worksheet_name, range = walking_duration)

p3 <- ggplot(data=walking_duration, aes(x=Walking_duration, y=Number)) + 
  geom_bar(stat="identity", fill="chartreuse4") +
  scale_x_discrete(limits = walking_duration$Walking_duration) +
  scale_y_continuous(limits = c(0, 20), breaks = c(0,10,20)) +
  theme_bw() +
  ylab("# of volunteers") +
  xlab("Average walking duration per day") +
  geom_text(aes(label = Number), vjust = -1, size = 2)


tech_familiarity <-
  read_sheet(ss = sheet_id, sheet = worksheet_name, range = tech_familiarity)

p4 <- ggplot(data=tech_familiarity, aes(x=Tech_familiarity, y=Number, fill=Tech_familiarity)) + 
  scale_fill_gradient(name="Scale",low="red",high="darkgreen",labels=c("1 - Not familiar at all",2,3,4,5,6,"7 - Very familiar")) +
  geom_bar(stat="identity") +
  scale_y_continuous(limits = c(0, 20), breaks = c(0,10,20)) +
  scale_x_continuous(limits = c(0.5, 7.5), breaks = c(1,2,3,4,5,6,7)) +
  theme_bw() +
  ylab("# of volunteers") +
  xlab("Familiarity with technology") +
  theme(legend.position = c(0.23, 0.65),
        legend.key.size = unit(0.35, units = "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=9)) +
  geom_text(aes(label = Number), vjust = -1, size = 2)

p4

dem_plots <- plot_grid(p1, p2, p3, p4, labels = "AUTO")

ggsave(
  "figure_UEQ_bar_chart/demographics_graphs.pdf",
  plot = dem_plots,
  width = 20,
  height = 11,
  units = "cm"
)

showCols <- function(cl=colors(), bg = "grey",
                     cex = 0.75, rot = 30) {
  m <- ceiling(sqrt(n <-length(cl)))
  length(cl) <- m*m; cm <- matrix(cl, m)
  require("grid")
  grid.newpage(); vp <- viewport(w = .92, h = .92)
  grid.rect(gp=gpar(fill=bg))
  grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
            vp=vp, gp=gpar(cex = cex, col = cm))
}

showCols(cl= colors(), bg="gray33", rot=30, cex=0.75)

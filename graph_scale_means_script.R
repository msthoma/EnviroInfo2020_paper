source("vars.R")
library(googlesheets4)
library(ggplot2)
gs4_deauth()

results <- read_sheet(ss = sheet_id, sheet = worksheet_name, range = range)

results$Category <- factor(results$Category, levels = c("Excellent", "Good", "Above Average", "Below Average", "Bad"))

bar_chart <- ggplot(results, aes(Scale)) + 
  geom_bar(aes(weight = Mean, fill = Category)) +
  scale_fill_discrete(drop=FALSE) + scale_x_discrete(drop=FALSE)
bar_chart

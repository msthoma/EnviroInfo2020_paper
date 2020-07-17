source("vars.R")
library(googlesheets4)
library(ggplot2)
gs4_deauth()

results <- read_sheet(ss = sheet_id, sheet = worksheet_name, range = range)

bar_chart <- ggplot(results, aes(Scale)) + geom_bar(aes(weight = Mean))
bar_chart

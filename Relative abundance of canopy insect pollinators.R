# Estimation of the relative abundance of major orders and functional groups identified as potential pollinators, as captured by PICT cameras on each of the seven canopy trees surveyed.
# Import necessary Libraries
library(ggplot2)
library(webr)
library(dplyr)
setwd("/Users/Oria/Documents/ULB/PhD/Article_pollination/Script")
# Read data file, with insects order, group and freq
dat = read.table("Pie_Poll.txt", header=T,sep="\t")
# Convert the data file into data frame
data = as.data.frame(dat)
PD = dat %>% group_by(Order, Group) %>% summarise(n = sum(Freq))
print(PD)
# Create a pie and donut combo chart 
PieDonut(PD, aes(Order, Group, count=n), pieLabelSize = 2, donutLabelSize = 2, r0 = getOption("PieDonut.r0", 0.4),r1 = getOption("PieDonut.r1", 1),showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),labelpositionThreshold = 0.5, labelposition = getOption("PieDonut.labelposition", 2), title = "Proportions of canopy insect pollinators")



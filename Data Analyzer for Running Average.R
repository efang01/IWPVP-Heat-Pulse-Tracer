##################################################
# 
#
#       Data Analyzer for Running Average
#       Written by Ellis Fangmann
#       March 18, 2025
#
#
##################################################

# Read the data, skipping possible metadata lines
#setwd("C:/Users/Ellis Fangmann/OneDrive - University of Kansas/3 Research/Collected Data/March 19")
data <- read.csv("CR1000_TempSensMar19.daT", skip = 1, header = TRUE)

# Peek at the first few rows
head(data)

# Convert TIMESTAMP to POSIXct datetime
data$TIMESTAMP <- as.POSIXct(data$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")

# Make sure HalfBR is numeric (in case it's read as character)
data$HalfBR <- as.numeric(data$HalfBR)

##

library(zoo)

# Let's say you want a 100-point running average
data$HalfBR_smooth <- rollmean(data$HalfBR, k = 60, fill = NA, align = "center")

# k = point window, fill = handles edges, align = where average is anchored

library(ggplot2)

ggplot(data, aes(x = TIMESTAMP)) +
  geom_point(aes(y = HalfBR), color = "gray", alpha = 0.5) +
  geom_line(aes(y = HalfBR_smooth), color = "blue", size = 1) +
  scale_y_continuous(limits = c(0.65,0.7)) +
  labs(title = "Smoothed HalfBR with Running Average", y = "HalfBR")
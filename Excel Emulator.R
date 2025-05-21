##################################################
# 
#
#       Excel Emulator
#       Written by Ellis Fangmann
#       April 11, 2025
#
#
##################################################

# Define model parameters
V <- 3             # Volume (mL)
Qin <- 6.6E-3      # Flow rate (mL/min)
Rin <- 3.0E-4        # (mass/min)
Tb <- 19     # Background temp (°C)
T0 <- Tb        # Initial temp (°C)
Kh <- 2            # Heat transfer coefficient
t_stop <- 90       # Time when heating stops (min)
t_stop2 <- 30      # Time when cooling stops (min)

# Time vector
time <- seq(0, 90, by = 1)

# Initialize output vector
Temp <- numeric(length(time))

# Define model terms
TermOne <- Rin / Qin
TermTwo <- Qin / ((1 + Kh) * V)

# Precompute temperature at the end of heating
#Temp_heating_end <- Tb + TermOne * (1 - exp(-TermTwo * t_stop))

# Loop to compute temperature profile
for (i in seq_along(time)) {
  t <- time[i]
  if (t <= t_stop) {
    # During heating
    Temp[i] <- Tb + TermOne * (1 - exp(-TermTwo * t))
  } else {
    # After heating — exponential decay from Temp_heating_end to Tb
    t <- t_stop + 1
  }
}

#plot(time, Temp, type = "l", col = "red", lwd = 2, main = "Breakthrough Curve",xlab = "Time (min)", ylab = "Temperature (°C)")

time2 <- seq(1, 30, by = 1)
Temp2 <- numeric(length(time2))
T02 <- Temp[91]

for (i in seq_along(time2)) {
  t2 <- time2[i]
  if (t2 <= t_stop2) {
    # During heating
    Temp2[i] <- Tb + (T02 - Tb) * exp(-TermTwo * t2)
  } else {
    # After heating — exponential decay from Temp_heating_end to Tb
    t2 <- t_stop2 + 2
  }
}

# Plot the breakthrough curve
# plot(time2, Temp2, type = "l", col = "blue", lwd = 2, main = "Breakthrough Curve", xlab = "Time (min)", ylab = "Temperature (°C)")

combinedplot <- as.data.frame(Temp)
combinedplot$time <- seq(0, 90, by = 1)


combinedplot2 <- as.data.frame(Temp2[2:31])
combinedplot2$time <- time2 + 90
colnames(combinedplot2)[1] <- "Temp"

combinedplot3 <- rbind(combinedplot,combinedplot2)

plot(combinedplot3$time, combinedplot3$Temp, type = "l", col = "darkgreen", lwd = 2,
     main = "Breakthrough Curve",
     xlab = "Time (min)", ylab = "Temperature (°C)")

# Mark time heating stops
abline(v = t_stop, col = "red", lty = 2)
text(t_stop + 2, max(Temp)*0.95, "Heating Stops", col = "red", pos = 4)

#data points

library(readr)
library(dplyr)
library(lubridate)
library(stringr)

#Load and clean measured data
data <- read_csv("CR1000_TempSensMar19.dat", skip = 1)

#Remove units line
data <- data %>%
  filter(str_detect(TIMESTAMP, "^\\d{4}-\\d{2}-\\d{2}"))

#Parse time and temperature columns
data <- data %>%
  mutate(
    TIMESTAMP = ymd_hms(TIMESTAMP),
    Time_min = as.numeric(difftime(TIMESTAMP, first(TIMESTAMP), units = "mins")),
    Temperature_C = as.numeric(Temperature_C)
  )

fixeddata <- as.data.frame(data$Temperature_C)
colnames(fixeddata)[1] <- "Temperature_C"
fixeddata$time <- (data$Time_min)

plot(combinedplot3$time, combinedplot3$Temp, type = "l", col = "darkgreen", lwd = 2,
     main = "Breakthrough Curve",
     xlab = "Time (min)", ylab = "Temperature (°C)")

points(fixeddata$time, fixeddata$Temperature_C, col = "darkgreen", pch = 17)

plot(data$Time_min, data$Temperature_C, type = "l", col = "blue", lwd = 2,
     ylim = range(c(Temp, data$Temperature_C), na.rm = TRUE),
     main = "Model vs. Measured Temperature",
     xlab = "Time (min)", ylab = "Temperature (°C)")

points(data$Time_min, data$Temperature_C, col = "yellow", pch = 17)


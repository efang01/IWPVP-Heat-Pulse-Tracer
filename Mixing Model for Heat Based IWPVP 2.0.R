##################################################
# 
#
#       Mixing Model for Heat Based IWPVP Ver. 2.0
#       Written by Ellis Fangmann
#       April 3, 2025
#
#
##################################################

# Define model parameters
V <- 3             # Volume (L)
Qin <- 6.6E-3        # Flow rate (mL/min)
Rin <- 3.0E-4       #(mass/min)
Tb <- 19        # Background temp (°C)
T0 <- Tb        # Initial temp (°C)
Kh <- 1            # Heat transfer coefficient
t_stop <- 90       # Time when heating stops (min)

# Time vector
time <- seq(0, 120, by = 1)

# Initialize output vector
Temp <- numeric(length(time))

# Define model terms
TermOne <- (V * Rin) / Qin
TermTwo <- Qin / ((1 + Kh) * V)

# Precompute temperature at the end of heating
Temp_heating_end <- Tb + TermOne * (1 - exp(-TermTwo * t_stop))
#Temp_heating_end <- 20

# Loop to compute temperature profile
i = 1

while (i < 122) {
  t <- time[i]
  if (t <= t_stop) {
    # During heating
    Temp[i] <- Tb + TermOne * (1 - exp(-TermTwo * t))
  } else {
    # After heating — exponential decay from Temp_heating_end to Tb
    delta_t <- t - t_stop
    Temp[i] <- Tb + (Temp_heating_end - Tb) * exp(-TermTwo * delta_t)
  }
  i <- i + 1 
}

# Plot the breakthrough curve
plot(time, Temp, type = "l", col = "blue", lwd = 2,
     main = "Breakthrough Curve",
     xlab = "Time (min)", ylab = "Temperature (°C)")

# Mark time heating stops
abline(v = t_stop, col = "red", lty = 2)
text(t_stop + 2, max(Temp)*0.95, "Heating Stops", col = "red", pos = 4)
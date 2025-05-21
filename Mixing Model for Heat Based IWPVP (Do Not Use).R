##################################################
# 
#
#       Mixing Model for Heat Based IWPVP
#       Written by Ellis Fangmann
#       March 21, 2025
#
#
##################################################
 
#Constants
WaterQin <- 0.05 #Qin (mL/hr)
Volume <- 0.5 #V (mL)
BackgroundT <- 0.66 #Tb (deg)
InflowTDischarge <- 0.03298 #
MaxTInject  <- 0.01
Volume <- 0.5 #mL
To <- 0.69
InjectedTDischarge <- 0.0001

#Create Dataframe
MixingModelData <- as.data.frame(seq(from = 0, to = 60, by = 1))
colnames(MixingModelData) <- "Time"

#Equation
#Function for Mixing Model
Time <- as.data.frame(MixingModelData$Time)
colnames(Time) <- "Time"

MixingModelFunction <- function(BackgroundT, InflowTDischarge, WaterQin, MaxTInject,
                                InjectedTDischarge, Volume, Time) {
  i = 1
  T <- Time
  while (i < 62) {
  if(Time [i, ] < 30) {
    T[i,] <- (InjectedTDischarge + InflowTDischarge)/(WaterQin) + 
      (BackgroundT - (InjectedTDischarge + InflowTDischarge)/(WaterQin)) * 
      exp(-1*WaterQin*Time[i,]/Volume)
  } else {
    T[i,] <- BackgroundT + (To - BackgroundT) *
      exp(-1*WaterQin*Time[i,]/Volume)
  }
    i = i+1
  }
  return(T)
}

T <- MixingModelFunction(BackgroundT, InflowTDischarge, WaterQin, MaxTInject,
                    InjectedTDischarge, Volume, Time)
Time$T <- T
colnames(Time[,2]) <- "T"

plot(Time)


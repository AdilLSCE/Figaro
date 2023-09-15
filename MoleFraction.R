#Written by Adil at the Laboratoire des Sciences du Climat et de l'Environnement in AK 138 - 140
#E-mail: adil.shah@lsce.ipsl.fr

#Clear any variables from the environment
rm(list = ls())

#Import functions
source("Functions/WaterAveraging.R")
library(caTools)

#List all of the loggers
SensorList <- c("TGS_2611C00_2", "TGS_2602", "TGS_2611C00_1A", "TGS_2611C00_1B", "TGS_2611E00")

#Set up empty data frame to add coefficients to and define the name of each row
Coefficients <- data.frame(matrix(ncol=length(SensorList), nrow=25))
rownames(Coefficients) <- c("BackgroundFromWind", "VariationThreshold_Ohms", "WindSpeedQualityThreshold_mPERs", "A_Ohms", "B", "C_PERK", "D_PERK", "c_ppm", "gamma", "Background_deg", "BackgroundSectorSize_deg", "WaterThreshold_PERmin", "MethaneBackground_ppm", "MethaneBackgroundFromLI7810", "cANDgammaFromLI7810", "BackgroundModelFitAttempted", "BackgroundModelFitSuccessful", "BackgroundModelRMSE_Ohms", "BackgroundModelR2", "A_averaging_level", "MethaneModelRMSE", "MethaneModelR2", "MethaneModelDataLimit", "WaterQualityControl", "CentralEpochTime")
colnames(Coefficients) <- SensorList
Coefficients["cANDgammaFromLI7810", ] <- "no"

#####

#Decide on the prefix of the file name
OutputFileName <- "FinalFinalFinal"

#Decide on a new output location
OutputLocation <- "C:/Users/ashah/Documents/Figaro/MergedFigaroData/"

#Decide on background from wind (1) or from resistance variability (0)
Coefficients["BackgroundFromWind", ] <- 1

#Decide on the background standard deviation resistance threshold in Ohms
Coefficients["VariationThreshold_Ohms", ] <- 100

#Decide on the wind speed quality threshold in Ohms
Coefficients["WindSpeedQualityThreshold_mPERs", ] <- 1

#Decide "yes" or "no" on whether or not to derive a methane background from the LICOR LI7810 (only select "yes" if you import LICOR LI7810 data)
Coefficients["MethaneBackgroundFromLI7810", ] <- "no"

#Decide on individual c methane coefficients in ppm if known (or starting parameters)
Coefficients["c_ppm", ] <- c(10, 618.887325886546, 9.06440223521794, 9.21563897961338, 17.6182646205324)

#Decide on the uncertainty in individual c methane coefficients in ppm if known (or NA if not known)
Coefficients["c_uncertainty_ppm", ] <- c(NA, 178.58275509603, 0.146566975862452, 0.247242358962543, 0.504299976506198)

#Decide on individual gamma methane coefficients if known (or starting parameters)
Coefficients["gamma", ] <- c(0.5, 0.126899158335021, 0.452205512328489, 0.430978567681635, 0.44198356210956)

#Decide on the uncertainty in individual gamma methane coefficients if known (or NA if not known)
Coefficients["gamma_uncertainty", ] <- c(NA, 0.0261267759827108, 0.00341409619476837, 0.00533278136071376, 0.00631392453543003)

#Decide on the direction of the background and the size of the background segment in degrees
Coefficients["Background_deg", ] <- c(65, 65, 65, 65, 65)
Coefficients["BackgroundSectorSize_deg", ] <- c(216, 216, 216, 216, 216)

#Decide on water threshold in per minute
Coefficients["WaterThreshold_PERmin", ] <- 0.003

#Decide on the default methane background in ppm (you may need to change this in 50 years)
Coefficients["MethaneBackground_ppm", ] <- 2

#Set up empty data frames to add data to
for (i in SensorList){
  assign(paste("Data", i, sep="_"), data.frame())
}
rm (i)
WindData <- data.frame()
LI7810Data <- data.frame()
Output <- data.frame()

#Function to import Figaro data, if it exists
ImportFigaroData <- function(Data, Location){
  if(file.exists(Location)){
    Data <- rbind(Data, read.table(Location, header = FALSE, skip = 1, blank.lines.skip = TRUE, fill = TRUE, sep = ","))
  } else {
    print(paste("this file does not exist:", Location))
  }
  return(Data)
}

##### SKIP IF THERE IS NO LICOR LI7810 DATA

#Decide on the location of LICOR LI7810 data
FileName <- "2022-04"

#Import the LI7810 data
LI7810Data <- rbind(LI7810Data, read.table(paste("C:/Users/ashah/Documents/Figaro/Data/LI-COR/Minute-Average_LICOR_LI-7810_Background_Data_", FileName, ".csv", sep=""), header = FALSE, skip = 1, blank.lines.skip = TRUE, fill = TRUE, sep = ","))

#####

#Decide on the location of wind data
FileName <- "2022-04"

#Import the wind data
WindData <- rbind(WindData, read.table(paste("C:/Users/ashah/Documents/Figaro/Data/MetPack/AmaillouxMetPak-", FileName, "-MinuteAverage.csv", sep=""), header = FALSE, skip = 1, blank.lines.skip = TRUE, fill = TRUE, sep = ","))

#####

#Decide on the Figaro input file name
FileName <- "2022-04"

#Import raw data
Data_TGS_2611C00_2 <- ImportFigaroData(Data_TGS_2611C00_2, paste("C:/Users/ashah/Documents/Figaro/Data/LSCE009/LSCE009-", FileName, "-combined.csv", sep=""))
Data_TGS_2602 <- ImportFigaroData(Data_TGS_2602, paste("C:/Users/ashah/Documents/Figaro/Data/Figaro Ultra Logger B/", FileName, "/TRACE-Figaro-Ultra-Logger-B-TGS_2602-B-", FileName, "-Minute-Averaged.csv", sep=""))
Data_TGS_2611C00_1A <- ImportFigaroData(Data_TGS_2611C00_1A, paste("C:/Users/ashah/Documents/Figaro/Data/Figaro Ultra Logger B/", FileName, "/TRACE-Figaro-Ultra-Logger-B-TGS_2611-C00-B-", FileName, "-Minute-Averaged.csv", sep=""))
Data_TGS_2611C00_1B <- ImportFigaroData(Data_TGS_2611C00_1B, paste("C:/Users/ashah/Documents/Figaro/Data/Figaro Ultra Logger B/", FileName, "/TRACE-Figaro-Ultra-Logger-B-TGS_2611-C00-B-OLD-", FileName, "-Minute-Averaged.csv", sep=""))
Data_TGS_2611E00 <- ImportFigaroData(Data_TGS_2611E00, paste("C:/Users/ashah/Documents/Figaro/Data/Figaro Ultra Logger B/", FileName, "/TRACE-Figaro-Ultra-Logger-B-TGS_2611-E00-B-", FileName, "-Minute-Averaged.csv", sep=""))

#####

#Remove any file name
rm(FileName)

#Function to plot time jumps
TimeJumps <- function(Time, Jumps, Title){
  print(paste("periods of discontinnuity for", Title))
  print(Jumps)
  xPositions <- as.POSIXct(pretty(Time/86400, 10)*86400, origin="1970-01-01", tz="GMT")
  par(mar = c(5.1, 2.1, 1.1, 1.1))
  plot(as.POSIXct(Time, origin="1970-01-01", tz="GMT"), rep(0, length(Time)), type="p", pch=16, cex=0.1, ylim=c(-1,1), font=6, font.main=6, font.lab=6, ylab="", xlab="", xaxt = "n", yaxt = "n", xaxs="i", yaxs="i")
  if (length(Jumps) > 0){
    lines(Time[sort(rep(Jumps, 2))], rep(c(-2, 2, 2, -2), ceiling(length(Jumps)/2))[1:(length(Jumps)*2)], col=rgb(1,0,0), lty=1, lwd=0.5)
  }
  axis(1, at=xPositions, labels=format(xPositions, "%d.%m.%Y"), font=6, font.main=6, font.lab=6, las=3)
  mtext(paste(Title, "time jumps"), side=2, line=1, font=6, font.main=6, col=rgb(1,0,0))
  rm(xPositions)
}

#Assign names to sensor data and list any indices of negative or stationary time to resolve
for (i in SensorList){
  if (nrow(get(paste("Data", i, sep="_"))) != 0){
    assign(paste("Data", i, sep="_"), data.frame(EpochTime_s = get(paste("Data", i, sep="_"))[,1], TimeString = get(paste("Data", i, sep="_"))[,2], Voltage_V = get(paste("Data", i, sep="_"))[,3], RawVoltage_V = get(paste("Data", i, sep="_"))[,4], SupplyVoltage_V = get(paste("Data", i, sep="_"))[,5], Temperature_K = get(paste("Data", i, sep="_"))[,6], RelativeHumidity_PERCENT = get(paste("Data", i, sep="_"))[,7], Resistance_Ohms = get(paste("Data", i, sep="_"))[,8], Pressure_Pa = get(paste("Data", i, sep="_"))[,9], WaterMoleFraction = get(paste("Data", i, sep="_"))[,10]))
    TimeJumps(get(paste("Data", i, sep="_"))$EpochTime_s, (1:nrow(get(paste("Data", i, sep="_"))))[get(paste("Data", i, sep="_"))$EpochTime_s[2:nrow(get(paste("Data", i, sep="_")))] - get(paste("Data", i, sep="_"))$EpochTime_s[1:nrow(get(paste("Data", i, sep="_")))-1] > 120], i)
  }
}

#Sort wind data in-case there are any overlapping data points and list any indices that are not separated by 60 seconds
WindData <- WindData[(1:nrow(WindData))[order(WindData$V1)][!duplicated(WindData$V1[order(WindData$V1)])], ]
TimeJumps(WindData$V1, (1:nrow(WindData))[WindData$V1[2:nrow(WindData)] - WindData$V1[1:nrow(WindData)-1] != 60], "Wind")

#Sort LI-COR in-case there are any overlapping data points and list any indices that are not separated by 60 seconds
if(nrow(LI7810Data) != 0){
  LI7810Data <- LI7810Data[(1:nrow(LI7810Data))[order(LI7810Data$V1)][!duplicated(LI7810Data$V1[order(LI7810Data$V1)])], ]
  TimeJumps(LI7810Data$V1, (1:nrow(LI7810Data))[LI7810Data$V1[2:nrow(LI7810Data)] - LI7810Data$V1[1:nrow(LI7810Data)-1] != 60], "LI-COR LI7810")
}

#####

#Produce output file names
if(!dir.exists(paste(OutputLocation, OutputFileName, "/", sep=""))){
  dir.create(paste(OutputLocation, OutputFileName, "/", sep=""))
}
for (i in 1:ncol(Coefficients)){
  Output[i,1] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_Processed_", colnames(Coefficients)[i], ".csv", sep="")
  Output$PlotsEnvironment[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_PlotEnvironment_", colnames(Coefficients)[i], ".png", sep="")
  Output$PlotsBaseline[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_PlotBaseline_", colnames(Coefficients)[i], ".png", sep="")
  Output$PlotsRatio[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_PlotRatio_", colnames(Coefficients)[i], ".png", sep="")
  Output$PlotsMoleFraction[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_PlotMoleFraction_", colnames(Coefficients)[i], ".png", sep="")
  Output$PlotsWind[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_PlotWind_", colnames(Coefficients)[i], ".png", sep="")
  Output$PlotsResistanceSD[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_PlotResistanceSD_", colnames(Coefficients)[i], ".png", sep="")
  Output$PlotsWater[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_PlotWater_", colnames(Coefficients)[i], ".png", sep="")
  Output$PlotsWaterGradients[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_PlotWaterGradient_", colnames(Coefficients)[i], ".png", sep="")
  Output$LI7810BackgroundComparison[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_LI7810BackgroundComparison_", colnames(Coefficients)[i], ".png", sep="")
  Output$LI7810MethaneModelFit[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_LI7810MethaneModelFit_", colnames(Coefficients)[i], ".png", sep="")
  Output$LI7810MethaneModelPerformance[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_LI7810MethaneModelPerformance_", colnames(Coefficients)[i], ".png", sep="")
  Output$LI7810MethaneModelComparison[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_LI7810MethaneModelComparison_", colnames(Coefficients)[i], ".png", sep="")
  Output$LI7810MethaneModelResiduals[i] <- paste(OutputLocation, OutputFileName, "/", OutputFileName, "_LI7810MethaneModelResiduals_", colnames(Coefficients)[i], ".png", sep="")
}
names(Output)[1] <- "Data"
rm(i)

#Calculate calculate standard deviation of resistances, calculate rounded Epoch time, assign winds to each sensor and calculate difference from background wind direction, with 0 corresponding to the expected location of the source
DataProcessingInitial <- function(Data, number, Coefficients, WindData, LI7810Data, WaterStatus, W1, W2, W3, W4, W5, W6, W7){
  Data$RoundedEpochTime_s <- (floor(Data$EpochTime_s/60))*60
  while(length((1:nrow(Data))[Data$RoundedEpochTime_s[1:nrow(Data) - 1] - Data$RoundedEpochTime_s[2:nrow(Data)] == 0]) != 0){
    Index <- (1:nrow(Data))[Data$RoundedEpochTime_s[1:nrow(Data) - 1] - Data$RoundedEpochTime_s[2:nrow(Data)] == 0][1]
    Data[Index, 1] <- mean(Data[c(Index, Index + 1), 1])
    Data[Index, c(3:10)] <- c(mean(Data[c(Index, Index + 1), 3]), mean(Data[c(Index, Index + 1), 4]), mean(Data[c(Index, Index + 1), 5]), mean(Data[c(Index, Index + 1), 6]), mean(Data[c(Index, Index + 1), 7]), mean(Data[c(Index, Index + 1), 8]), mean(Data[c(Index, Index + 1), 9]), mean(Data[c(Index, Index + 1), 10]))
    Data <- Data[-c(Index + 1),]
    rm(Index)
  }
  Data$WindU_mps <- WindData$V4[match(Data$RoundedEpochTime_s, WindData$V1)]
  Data$WindV_mps <- WindData$V6[match(Data$RoundedEpochTime_s, WindData$V1)]
  Data$WindPoints <- WindData$V5[match(Data$RoundedEpochTime_s, WindData$V1)]
  Data$WindSpeed_mps <- WindData$V8[match(Data$RoundedEpochTime_s, WindData$V1)]
  Data$WindDirection <- WindData$V9[match(Data$RoundedEpochTime_s, WindData$V1)]
  Data$WindDirection_degrees <- WindData$V10[match(Data$RoundedEpochTime_s, WindData$V1)]
  Data$WindQuality <- !is.na(Data$WindPoints) & Data$WindPoints >= 20 & Data$WindSpeed_mps > as.double(Coefficients["WindSpeedQualityThreshold_mPERs", number])
  Data$BackgroundAngle <- round(Data$WindDirection - (as.double(Coefficients["Background_deg", number])*pi/180), digits=6)
  Data$BackgroundAngle[!is.na(Data$BackgroundAngle) & Data$BackgroundAngle < -pi] <- Data$BackgroundAngle[!is.na(Data$BackgroundAngle) & Data$BackgroundAngle < -pi] + (2*pi)
  Data$BackgroundAngle[!is.na(Data$BackgroundAngle) & Data$BackgroundAngle > pi] <- Data$BackgroundAngle[!is.na(Data$BackgroundAngle) & Data$BackgroundAngle > pi] - (2*pi)
  if(nrow(Data) > 0){
    Data$BackgroundFromWindNoWater <- !is.na(Data$BackgroundAngle) & Data$WindQuality & Data$BackgroundAngle < (as.double(Coefficients["BackgroundSectorSize_deg", number])*pi/360) & Data$BackgroundAngle > -(as.double(Coefficients["BackgroundSectorSize_deg", number])*pi/360)
    Data <- WaterAveraging(Data, W1, W2, W3, as.double(Coefficients["WaterThreshold_PERmin", number]), W4, W5, W6, W7, WaterStatus)
    Data$ResistanceSD_Ohms <- round(runsd(Data$Resistance_Ohms, 5, endrule = "NA"), digits=2)
  } else {
    Data <- cbind(Data, data.frame(BackgroundFromWindNoWater=double(), WaterAverages=double(), WaterNumberOfPoints=double(), WaterTimeDiffference_s=double(), WaterAverageQuality=double(), WaterGradient_PERmin=double(), WaterAverageGradient_PERmin=double(), WaterAverageGradientNumberOfPoints=double(), WaterAverageGradientTimeDifference_s=double(), WaterAverageGradientQuality=double(), WaterQuality=double(), ResistanceSD_Ohms=double()))
  }
  Data$BackgroundFromResistanceNoWater <- !is.na(Data$ResistanceSD_Ohms) & Data$ResistanceSD_Ohms < as.double(Coefficients["VariationThreshold_Ohms", number])
  Data$BackgroundFromResistance <- Data$BackgroundFromResistanceNoWater & Data$WaterQuality
  Data$BackgroundFromWind <- Data$BackgroundFromWindNoWater & Data$WaterQuality
  if (nrow(Data) > 0 & nrow(LI7810Data) > 0){
    Data$LI7810MethaneMoleFraction_ppm <- LI7810Data$V3[match(Data$RoundedEpochTime_s, LI7810Data$V1)]
    Data$LI7810MethaneMoleFractionPoints <- LI7810Data$V4[match(Data$RoundedEpochTime_s, LI7810Data$V1)]
    Data$LI7810MethaneMoleFraction_ppm[Data$LI7810MethaneMoleFractionPoints < 59] <- NA
    Data$LI7810ProposedBackgroundMethaneMoleFraction_ppm <- Data$LI7810MethaneMoleFraction_ppm
    if (as.double(Coefficients["BackgroundFromWind", number]) == 1){
      Data$LI7810ProposedBackgroundMethaneMoleFraction_ppm[!Data$BackgroundFromWind] <- NA
    } else if (as.double(Coefficients["BackgroundFromWind", number]) == 0){
      Data$LI7810ProposedBackgroundMethaneMoleFraction_ppm[!Data$BackgroundFromResistance] <- NA
    }
    Data$LI7810FittingIndices <- FALSE
  }
  return(Data)
}

#####

#Conduct initial setup and calculate average water mole fraction for each hour (make the status "off" if water processing not needed)
Data_TGS_2611C00_2 <- DataProcessingInitial(Data_TGS_2611C00_2, 1, Coefficients, WindData, LI7810Data, "on", 900, 1800, 3600, 780, 14, 1680, 25)
Data_TGS_2602 <- DataProcessingInitial(Data_TGS_2602, 2, Coefficients, WindData, LI7810Data, "on", 900, 1800, 3600, 780, 14, 1680, 25)
Data_TGS_2611C00_1A <- DataProcessingInitial(Data_TGS_2611C00_1A, 3, Coefficients, WindData, LI7810Data, "off", 900, 1800, 3600, 780, 14, 1680, 25)
Data_TGS_2611C00_1B <- DataProcessingInitial(Data_TGS_2611C00_1B, 4, Coefficients, WindData, LI7810Data, "off", 900, 1800, 3600, 780, 14, 1680, 25)
Data_TGS_2611E00 <- DataProcessingInitial(Data_TGS_2611E00, 5, Coefficients, WindData, LI7810Data, "off", 900, 1800, 3600, 780, 14, 1680, 25)

#Copy water quality control from one sensor to all sensors in the same logger to save time
Data_TGS_2611C00_1A[,c(21:30,34)] <- Data_TGS_2602[,c(21:30,34)]
Data_TGS_2611C00_1B[,c(21:30,34)] <- Data_TGS_2602[,c(21:30,34)]
Data_TGS_2611E00[,c(21:30,34)] <- Data_TGS_2602[,c(21:30,34)]
Data_TGS_2611C00_1A$BackgroundFromResistance <- Data_TGS_2611C00_1A$BackgroundFromResistanceNoWater & Data_TGS_2611C00_1A$WaterQuality
Data_TGS_2611C00_1B$BackgroundFromResistance <- Data_TGS_2611C00_1B$BackgroundFromResistanceNoWater & Data_TGS_2611C00_1B$WaterQuality
Data_TGS_2611E00$BackgroundFromResistance <- Data_TGS_2611E00$BackgroundFromResistanceNoWater & Data_TGS_2611E00$WaterQuality

#Function to derive model parameters
BackgroundModel <- function(Coefficients, i, number, fit){
  if (fit == "on"){
    tryCatch(
      {
        print(paste("modelling data number", as.character(number)))
        tempBGLogical <- i$BackgroundFromResistance
        if (as.double(Coefficients["BackgroundFromWind", number]) == 1){
          tempBGLogical <- i$BackgroundFromWind
        }
        tempBG <- i$Resistance_Ohms[tempBGLogical]
        tempH2O <- i$WaterMoleFraction[tempBGLogical]
        tempT <- i$Temperature_K[tempBGLogical]
        model0 = nls(tempBG ~ A*(1 - (tempH2O*B)  + (tempH2O*tempT*C) - (tempT*D)), start = list(A=as.double(Coefficients["A_Ohms", number]), B=as.double(Coefficients["B", number]), C=as.double(Coefficients["C_PERK", number]), D=as.double(Coefficients["D_PERK", number])))
        RMSE <- sqrt(sum(residuals(model0)^2)/(length(residuals(model0))))
        R2 <- 1 - (sum(residuals(model0)^2)/sum((tempBG - mean(tempBG))^2))
        return(setNames(c(coef(model0)[1], coef(model0)[2], coef(model0)[3], coef(model0)[4], RMSE, R2, 1), c("A", "B", "C", "D", "RMSE", "R2", "model attempt")))
        print(paste("model number", as.character(number), "has worked successfully"))
      },
      error = function(e) {
        message(paste("there is an error with the model number", as.character(number)))
        print(e)
        return(setNames(c(NA, NA, NA, NA, NA, NA, 1), c("A", "B", "C", "D", "RMSE", "R2", "model attempt")))
      },
      warning = function(w) {
        message(paste("there is a warning with the model number", as.character(number)))
        print(w)
        return(setNames(c(NA, NA, NA, NA, NA, NA, 1), c("A", "B", "C", "D", "RMSE", "R2", "model attempt")))
      }
    )
    rm(tempBGLogical, tempBG, tempH2O, tempT, model0, RMSE, R2)
  } else if (fit == "off"){
    return(setNames(c(as.double(Coefficients["A_Ohms", number]), as.double(Coefficients["B", number]), as.double(Coefficients["C_PERK", number]), as.double(Coefficients["D_PERK", number]), NA, NA, 0), c("A", "B", "C", "D", "RMSE", "R2", "model attempt")))
  }
}

#Decide on individual A baseline correction factors in Ohms (or starting parameters)
Coefficients["A_Ohms",] <- c(10000, 100000, 100000, 100000, 100000)

#Decide on individual B baseline correction factors (or starting parameters)
Coefficients["B", ] <- c(50, 50, 50, 50, 50)

#Decide on individual C baseline correction factors in per Kelvin (or starting parameters)
Coefficients["C_PERK", ] <- c(0.05, 0.05, 0.05, 0.05, 0.05)

#Decide on individual D baseline correction factors in per Kelvin (or starting parameters)
Coefficients["D_PERK", ] <- c(0.001, 0.001, 0.001, 0.001, 0.001)

#####

#Derive model parameters for each background data set and collect background model coefficients (SET TO OFF OF NOT NEEDED AND DECIDE ON WHETHER TO INCLUDE A TIME FUNCTION)
BackgroundModels <- list()
BackgroundModels[[1]] <- BackgroundModel(Coefficients, get(paste("Data", SensorList[1], sep="_")), 1, "on")
BackgroundModels[[2]] <- BackgroundModel(Coefficients, get(paste("Data", SensorList[2], sep="_")), 2, "on")
BackgroundModels[[3]] <- BackgroundModel(Coefficients, get(paste("Data", SensorList[3], sep="_")), 3, "on")
BackgroundModels[[4]] <- BackgroundModel(Coefficients, get(paste("Data", SensorList[4], sep="_")), 4, "on")
BackgroundModels[[5]] <- BackgroundModel(Coefficients, get(paste("Data", SensorList[5], sep="_")), 5, "on")

#Model to produce a moving average A model, with recalculations of B, C and D
BackgroundModelRefinedA <- function(Coefficients, i, number, repetitions, BackgroundModels, averaginglevel=10000, lowestR2=1){
  tempBGLogical <- i$BackgroundFromResistance
  if (as.double(Coefficients["BackgroundFromWind", number]) == 1){
    tempBGLogical <- i$BackgroundFromWind
  }
  BGResistance <- i$Resistance_Ohms[tempBGLogical]
  BGH2O <- i$WaterMoleFraction[tempBGLogical]
  BGTemperature <- i$Temperature_K[tempBGLogical]
  BGTime <- i$RoundedEpochTime_s[tempBGLogical]
  if ((length(BGTime)-averaginglevel) < 2){
    print(paste("the averaging level is too high: the minimum level is", as.character(length(BGTime) - 2)))
    return(BackgroundModels)
  } else {
    h <- 0
    while (h < repetitions){
      h <- h + 1
      if (!exists("model1")){
        tempABank <- rep(as.double(Coefficients["A_Ohms", number]), (length(BGTime) - averaginglevel))
        BCD_0 <- c(as.double(Coefficients["B", number]), as.double(Coefficients["C_PERK", number]), as.double(Coefficients["D_PERK", number]))
        print("original background coefficients")
        print(BackgroundModels[[number]])
      } else {
        tempABank <- tempA
        BCD_0 <- c(as.double(coef(model2)[1]), as.double(coef(model2)[2]), as.double(coef(model2)[3]))
      }
      print(paste("starting refinement number", as.character(h)))
      tempTimeStamp <- c()
      tempA <- c()
      for (j in 1:(length(BGTime)-averaginglevel)){
        tempTimeStamp <- c(tempTimeStamp, mean(BGTime[j:(j+averaginglevel)]))
        BGResistance_reduced <- BGResistance[j:(j+averaginglevel)]
        BGH2O_reduced <- BGH2O[j:(j+averaginglevel)]
        BGTemperature_reduced <- BGTemperature[j:(j+averaginglevel)]
        model1 = nls(BGResistance_reduced ~ A*(1 - (BGH2O_reduced*BCD_0[1]) + (BGH2O_reduced*BGTemperature_reduced*BCD_0[2]) - (BGTemperature_reduced*BCD_0[3])), start = list(A=as.double(tempABank[j])))
        tempA <- c(tempA, as.double(coef(model1)[1]))
      }
      ANew <- round(unlist(approx(tempTimeStamp, tempA, xout=i$RoundedEpochTime_s, method='linear', rule=1, ties=mean)[2]), digits = 3)
      tempANew <- ANew[tempBGLogical]
      model2 = nls(BGResistance ~ tempANew*(1 - (BGH2O*B)  + (BGH2O*BGTemperature*C) - (BGTemperature*D)), start = list(B=BCD_0[1], C=BCD_0[2], D=BCD_0[3]))
      RMSE <- sqrt(sum(residuals(model2)^2)/(length(residuals(model2))))
      R2 <- 1 - (sum(residuals(model2)^2)/sum((BGResistance - mean(BGResistance))^2))
      print(paste("RMSE from round ", as.character(h), ": ", as.character(RMSE), "; R2 from round ", as.character(h), ": ", as.character(R2), "; average A from round ", as.character(h), ": ", as.character(mean(ANew, na.rm=TRUE)), "; coefficients from round ", as.character(h), ": ", sep=""))
      print(as.character(coef(model2)))
      if (lowestR2 == 1){
        if (h == 1){
          OUT_ANew <- ANew
          OUT_model2 <- model2
          OUT_RMSE <- RMSE
          OUT_R2 <- R2
          OUT_repetitions <- 1
        } else if (R2 > OUT_R2) {
          OUT_ANew <- ANew
          OUT_model2 <- model2
          OUT_RMSE <- RMSE
          OUT_R2 <- R2
          OUT_repetitions <- h
        } else {
          h <- repetitions
        }
        print(paste("best round so far:", as.character(OUT_repetitions)))
      } else {
        OUT_ANew <- ANew
        OUT_model2 <- model2
        OUT_RMSE <- RMSE
        OUT_R2 <- R2
        OUT_repetitions <- h
      }
    }
    BackgroundModels[[number]] <- c(BackgroundModels[[number]], averaginglevel, OUT_ANew)
    BackgroundModels[[number]][1] <- OUT_repetitions
    BackgroundModels[[number]][2] <- as.double(coef(OUT_model2)[1])
    BackgroundModels[[number]][3] <- as.double(coef(OUT_model2)[2])
    BackgroundModels[[number]][4] <- as.double(coef(OUT_model2)[3])
    BackgroundModels[[number]][5] <- as.double(OUT_RMSE)
    BackgroundModels[[number]][6] <- as.double(OUT_R2)
    BackgroundModels[[number]][7] <- 2
    return(BackgroundModels)
    rm(tempTimeStamp, BGResistance_reduced, BGH2O_reduced, BGTemperature_reduced, tempA, RMSE, R2, model1, model2, tempABank, ANew, tempANew, BCD_0)
  }
  rm(tempBGLogical, BGResistance, BGH2O, BGTemperature, BGTime)
}

#####

#Refine specific A values as a function of time
BackgroundModels <- BackgroundModelRefinedA(Coefficients, get(paste("Data", SensorList[1], sep="_")), 1, 100, BackgroundModels, 8000)
BackgroundModels <- BackgroundModelRefinedA(Coefficients, get(paste("Data", SensorList[2], sep="_")), 2, 100, BackgroundModels, 8000)
BackgroundModels <- BackgroundModelRefinedA(Coefficients, get(paste("Data", SensorList[3], sep="_")), 3, 100, BackgroundModels, 8000)
BackgroundModels <- BackgroundModelRefinedA(Coefficients, get(paste("Data", SensorList[4], sep="_")), 4, 100, BackgroundModels, 8000)
BackgroundModels <- BackgroundModelRefinedA(Coefficients, get(paste("Data", SensorList[5], sep="_")), 5, 100, BackgroundModels, 8000)

#####

#Output the central Epoch Time, whether the model fit has worked or not, whether a water correction has been applied or not, calculate a background value, if needed, calculate modelled background resistances and calculate resistance ratios
for (i in (1:length(SensorList))){
  if(nrow(get(paste("Data", SensorList[i], sep="_"))) >= 2){
    Coefficients["CentralEpochTime", i] <- min(get(paste("Data", SensorList[i], sep="_"))$RoundedEpochTime_s, na.rm=TRUE) + ((max(get(paste("Data", SensorList[i], sep="_"))$RoundedEpochTime_s, na.rm=TRUE) - min(get(paste("Data", SensorList[i], sep="_"))$RoundedEpochTime_s, na.rm=TRUE))/2)
  }
  if (nrow(get(paste("Data", SensorList[i], sep="_"))) > 0 & Coefficients["MethaneBackgroundFromLI7810", i] == "yes"){
    Coefficients["MethaneBackground_ppm", i] <- mean(Data$LI7810ProposedBackgroundMethaneMoleFraction_ppm, na.rm = TRUE)
  } else if (nrow(get(paste("Data", SensorList[i], sep="_"))) == 0 & Coefficients["MethaneBackgroundFromLI7810", i] == "yes"){
    Coefficients["MethaneBackground_ppm", i] <- NA
  }
  Coefficients["WaterQualityControl", i] <- "no"
  if (sum(get(paste("Data", SensorList[i], sep="_"))$WaterNumberOfPoints, na.rm=TRUE) != 0){
    Coefficients["WaterQualityControl", i] <- "yes"
  }
  Coefficients["BackgroundModelFitAttempted", i] <- "no"
  if(as.integer(unname(unlist(BackgroundModels[i]))[7]) == 1 | as.integer(unname(unlist(BackgroundModels[i]))[7]) == 2){
    Coefficients["BackgroundModelFitAttempted", i] <- "yes"
    if(!is.na(unname(unlist(BackgroundModels[i]))[1])){
      Coefficients["BackgroundModelFitSuccessful", i] <- "yes"
    } else {
      Coefficients["BackgroundModelFitSuccessful", i] <- "no"
    }
    if(as.integer(unname(unlist(BackgroundModels[i]))[7]) == 2){
      Coefficients["A_Ohms", i] <- paste("refined (", as.character(unname(unlist(BackgroundModels[i]))[1]), ")", sep="")
    } else {
      Coefficients["A_Ohms", i] <- unname(unlist(BackgroundModels[i]))[1]
    }
    Coefficients["B", i] <- unname(unlist(BackgroundModels[i]))[2]
    Coefficients["C_PERK", i] <- unname(unlist(BackgroundModels[i]))[3]
    Coefficients["D_PERK", i] <- unname(unlist(BackgroundModels[i]))[4]
    Coefficients["BackgroundModelRMSE_Ohms", i] <- unname(unlist(BackgroundModels[i]))[5]
    Coefficients["BackgroundModelR2", i] <- unname(unlist(BackgroundModels[i]))[6]
    Coefficients["A_averaging_level", i] <- unname(unlist(BackgroundModels[i]))[8]
  }
  Data <- get(paste("Data", SensorList[i], sep="_"))
  if (nrow(Data) > 0) {
    if (!is.na(Coefficients["A_Ohms", i]) && substr(Coefficients["A_Ohms", i], 1, 7) == "refined"){
      Data$A_Ohms <- as.double(BackgroundModels[[i]][9:(nrow(Data) + 8)])
    } else {
      Data$A_Ohms <- as.double(Coefficients["A_Ohms", i])
    }
    Data$BackgroundModelResistance_Ohms <- (Data$A_Ohms*(1 - (as.double(Coefficients["B", i])*Data$WaterMoleFraction) + (as.double(Coefficients["C_PERK", i])*Data$WaterMoleFraction*Data$Temperature_K) - (as.double(Coefficients["D_PERK", i])*Data$Temperature_K)))
    Data$Ratio <- round((Data$Resistance_Ohms/Data$BackgroundModelResistance_Ohms), digits=7)
    Data$MethaneMoleFraction_ppm <- (as.double(Coefficients["c_ppm", i])*((Data$Ratio^(-1/as.double(Coefficients["gamma", i]))) - 1)) + as.double(Coefficients["MethaneBackground_ppm", i])
    Data$MethaneMoleFractionUncertainty_ppm <- round(abs((Data$MethaneMoleFraction_ppm - as.double(Coefficients["MethaneBackground_ppm", i])))*sqrt(((1/(as.double(Coefficients["gamma", i])^4))*((1/(1 - ((Data$Ratio)^(1/as.double(Coefficients["gamma", i])))))^2)*((((as.double(Coefficients["gamma", i])*as.double(Coefficients["BackgroundModelRMSE_Ohms", i]))/Data$BackgroundModelResistance_Ohms)^2) + ((as.double(Coefficients["gamma_uncertainty", i])*log(1/Data$Ratio))^2))) + ((as.double(Coefficients["c_uncertainty_ppm", i])/as.double(Coefficients["c_ppm", i]))^2)), digits=9)
  }
  if (nrow(Data) > 0 & nrow(LI7810Data) > 0){
    Data$LI7810MethaneMoleFractionModelResiduals_ppm <- Data$LI7810MethaneMoleFraction_ppm - Data$MethaneMoleFraction_ppm
  }
  assign(paste("Data", SensorList[i], sep="_"), Data)
}
rm(i, Data)

#Function to derive c and gamma from LI7810 data
LI7810Modelling <- function(Data, number, Coefficients, cANDgammalimit="none", threshold=-1000, side="middle"){
  tryCatch(
    {
      FullEnhancement <- (Data$LI7810MethaneMoleFraction_ppm - as.double(Coefficients["MethaneBackground_ppm", number]))
      Data$LI7810FittingIndices <- !is.na(FullEnhancement) & FullEnhancement > threshold & Data$WaterQuality & !Data$BackgroundFromResistance & !is.na(Data$BackgroundModelResistance_Ohms)
      if (as.double(Coefficients["BackgroundFromWind", number]) == 1){
        Data$LI7810FittingIndices <- !is.na(FullEnhancement) & FullEnhancement > threshold & Data$WaterQuality & !Data$BackgroundFromWind & !is.na(Data$BackgroundModelResistance_Ohms)
      }
      if (cANDgammalimit != "none"){
        if (side == "start"){
          Data$LI7810FittingIndices[(((1:length(Data$LI7810FittingIndices))[Data$LI7810FittingIndices][cANDgammalimit]) + 1):length(Data$LI7810FittingIndices)] <- FALSE
        } else if (side == "end") {
          Data$LI7810FittingIndices[1:(1:length(Data$LI7810FittingIndices))[Data$LI7810FittingIndices][length((1:length(Data$LI7810FittingIndices))[Data$LI7810FittingIndices]) - cANDgammalimit]] <- FALSE
        } else {
          Data$LI7810FittingIndices[c((1:(1:length(Data$LI7810FittingIndices))[Data$LI7810FittingIndices][(length((1:length(Data$LI7810FittingIndices))[Data$LI7810FittingIndices]) - cANDgammalimit)/2]), ((1:length(Data$LI7810FittingIndices))[Data$LI7810FittingIndices][((length((1:length(Data$LI7810FittingIndices))[Data$LI7810FittingIndices]) + cANDgammalimit)/2) + 1]:length(Data$LI7810FittingIndices)))] <- FALSE
        }
      }
      tempRatio <- Data$Ratio[Data$LI7810FittingIndices]
      tempEnhancement <- FullEnhancement[Data$LI7810FittingIndices]
      plot(tempEnhancement, tempRatio, xlim=c(0, (max(tempEnhancement, na.rm=TRUE))), ylim=c(0, max(tempRatio)), pch=20, cex=0.1, col=rgb(0,0,0), type='p', font=6, font.main=6, font.lab=6, main=paste(colnames(Coefficients)[number], "Methane Model Fit"), xlab='LI7810 Water Quality-Reduced Methane Mole Fraction Enhancement (ppm)', ylab='Water Quality-Reduced Resistance Ratio')
      MethaneModel = nls(tempRatio ~ (1 + (tempEnhancement/a))^(-gamma), start = list(a=as.double(Coefficients["c_ppm", number]), gamma=as.double(Coefficients["gamma", number])))
      print(coef(MethaneModel))
      tempPlotEnhancement <- seq(as.double(Coefficients["MethaneBackground_ppm", number]), (max(tempEnhancement, na.rm=TRUE)*1.5), by=((max(tempEnhancement, na.rm=TRUE)*1.5/1000))) - as.double(Coefficients["MethaneBackground_ppm", number])
      lines(tempPlotEnhancement, (1 + (tempPlotEnhancement/as.double(coef(MethaneModel)[1])))^(-as.double(coef(MethaneModel)[2])), col=rgb(0,0,0.5), type='l')
      Coefficients["c_ppm", number] <- as.double(coef(MethaneModel)[1])
      Coefficients["c_uncertainty_ppm", number] <- summary(MethaneModel)$coefficients[1, "Std. Error"]
      Coefficients["gamma", number] <- as.double(coef(MethaneModel)[2])
      Coefficients["gamma_uncertainty", number] <- summary(MethaneModel)$coefficients[2, "Std. Error"]
      Coefficients["MethaneModelRMSE", number] <- sqrt(sum(residuals(MethaneModel)^2)/(length(residuals(MethaneModel))))
      Coefficients["MethaneModelR2", number] <- 1 - (sum(residuals(MethaneModel)^2)/sum((tempRatio - mean(tempRatio))^2))
      Coefficients["cANDgammaFromLI7810", number] <- "yes"
      Coefficients["MethaneModelDataLimit", number] <- cANDgammalimit
      Data$MethaneMoleFraction_ppm <- (as.double(Coefficients["c_ppm", number])*((Data$Ratio^(-1/as.double(Coefficients["gamma", number]))) - 1)) + as.double(Coefficients["MethaneBackground_ppm", number])
      Data$MethaneMoleFractionUncertainty_ppm <- round(abs((Data$MethaneMoleFraction_ppm - as.double(Coefficients["MethaneBackground_ppm", number])))*sqrt(((1/(as.double(Coefficients["gamma", number])^4))*((1/(1 - ((Data$Ratio)^(1/as.double(Coefficients["gamma", number])))))^2)*((((as.double(Coefficients["gamma", number])*as.double(Coefficients["BackgroundModelRMSE_Ohms", number]))/Data$BackgroundModelResistance_Ohms)^2) + ((as.double(Coefficients["gamma_uncertainty", number])*log(1/Data$Ratio))^2))) + ((as.double(Coefficients["c_uncertainty_ppm", number])/as.double(Coefficients["c_ppm", number]))^2)), digits=9)
      Data$LI7810MethaneMoleFractionModelResiduals_ppm <- Data$LI7810MethaneMoleFraction_ppm - Data$MethaneMoleFraction_ppm
      rm(tempRatio, tempEnhancement, FullEnhancement, MethaneModel, tempPlotEnhancement)
      return(list(Data, Coefficients))
    }
    ,
    error=function(e) {
      message(paste("there is an error with the model fit - try changing the c and gamma coeffeicients and try again"))
      print(e)
      return(list(Data, Coefficients))
    }
  )
}

#####

#Decide which data frames to apply a LI7810 model to (only apply the tempOutput if the model has constrained)
tempOutput <- LI7810Modelling(Data_TGS_2611C00_2, 1, Coefficients, 8000, 1)
Data_TGS_2611C00_2 <- tempOutput[[1]]
Coefficients <- tempOutput[[2]]
rm(tempOutput)

#####

#Output data
for(i in (1:length(SensorList))){
  OutputData(get(paste("Data", SensorList[i], sep="_")), i, Coefficients, Output)
}
rm(i)
write.table(Coefficients, paste(OutputLocation, OutputFileName, "/", OutputFileName, "_Coefficients.csv", sep=""), row.names = TRUE, col.names = NA, sep = ",")

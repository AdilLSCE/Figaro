#Written by Adil at the Laboratoire des Sciences du Climat et de l'Environnement in AK 138 - 140
#E-mail: adil.shah@lsce.ipsl.fr

#Calculate hourly average water mole fraction estimates containing at least 30 individual measurements in each hour
WaterAveraging <- function(Data, AveragingTime=3600, GradientAveragingTime=1800, QualityAdditionalDelayTime=1800, WaterGradientThreshold=0.00001, AveragingTimeThreshold=3480, AveragingNumberThreshold=59, GradientAveragingTimeThreshold=1680, GradientAveragingNumberThreshold=28, status="on"){
  if (status == "on"){
    print("water averaging status on")

  #Find running average water mole fraction, where average is taken either side of central time value
    Average <- rep(NA, nrow(Data))
    NumberOfPoints <- rep(NA, nrow(Data))
    TimeDifference <- rep(NA, nrow(Data))
    for (i in 1:nrow(Data)){
      Indices <- Data$RoundedEpochTime_s > (Data$RoundedEpochTime_s[i]-(AveragingTime*0.5)) & Data$RoundedEpochTime_s <= (Data$RoundedEpochTime_s[i]+(AveragingTime*0.5))
      NumberOfPoints[i] <- length(Data$RoundedEpochTime_s[Indices])
      TimeDifference[i] <- max(Data$RoundedEpochTime_s[Indices]) - min(Data$RoundedEpochTime_s[Indices])
      Average[i] <- mean(Data$WaterMoleFraction[Indices])
    }
  #Check that the time difference and number of points in each average is sufficient
    AverageQuality <- TimeDifference >= AveragingTimeThreshold & NumberOfPoints >= AveragingNumberThreshold
    
  #Calculate normalised the difference in running average water mole fraction between two points, provided that they are 60 seconds apart and both points are of good quality
    WaterGradients <- rep(NA, (nrow(Data) - 1))
    Indices <- (round(Data$RoundedEpochTime_s[2:nrow(Data)]) - round(Data$RoundedEpochTime_s[1:(nrow(Data) - 1)])) == 60 & AverageQuality[2:nrow(Data)] & AverageQuality[1:(nrow(Data) - 1)]
    WaterGradients[Indices] <- ((Average[2:nrow(Data)] - Average[1:(nrow(Data) - 1)])/((Average[2:nrow(Data)] + Average[1:(nrow(Data) - 1)])/2))[Indices]
    WaterGradients <- c(NA, WaterGradients)

  #Find the average absolute gradient from each point up to a period in the future of that point
    AverageGradient <- rep(NA, nrow(Data))
    GradientNumberOfPoints <- rep(NA, nrow(Data))
    GradientTimeDifference <- rep(NA, nrow(Data))
    for (i in 1:nrow(Data)){
      Indices <- Data$RoundedEpochTime_s >= Data$RoundedEpochTime_s[i] & Data$RoundedEpochTime_s < (Data$RoundedEpochTime_s[i] + GradientAveragingTime) & AverageQuality
      GradientNumberOfPoints[i] <- length(na.omit(WaterGradients[Indices]))
      GradientTimeDifference[i] <- max(Data$RoundedEpochTime_s[Indices]) - min(Data$RoundedEpochTime_s[Indices])
      AverageGradient[i] <- mean(abs(na.omit(WaterGradients[Indices])))
    }
    
  #Check that the time difference and number of points in each average gradient is sufficient
    AverageGradientQuality <- GradientTimeDifference >= GradientAveragingTimeThreshold & GradientNumberOfPoints >= GradientAveragingNumberThreshold
    
  #Ensure that the average change in water between average points is less than a threshold, based on the true values in the quality of the average gradient
    WaterQuality <- AverageGradientQuality & AverageGradient < WaterGradientThreshold & AverageGradient > -WaterGradientThreshold
    
  #If there is bad data, ensure that there is a sufficient delay to the quality assignment, after each bad data point
    for (i in (nrow(Data):1)){
      if (WaterQuality[i] == FALSE){
        WaterQuality[Data$RoundedEpochTime_s > Data$RoundedEpochTime_s[i] & Data$RoundedEpochTime_s < (Data$RoundedEpochTime_s[i] + QualityAdditionalDelayTime)] <- FALSE
      }
    }
    rm(Indices, i)
    
  #Collect water average data and quality values
  ### This is the running average water mole fraction taking all available data points from the current time +/- half the averaging time, as defined in the function
    Data$WaterAverages <- round(Average, digits=9)
  ### This is the number of points used in each running average water mole fraction
    Data$WaterNumberOfPoints <- NumberOfPoints
  ### This is the time difference from the first to the last data point used to derive a running average water mole fraction
    Data$WaterTimeDiffference_s <- round(TimeDifference, digits=3)
  ### A logical based on whether the number of points used to derive each running average water mole fraction and the time difference surpass a threshold, as defined in the function
    Data$WaterAverageQuality <- AverageQuality
  ### Finds the normalised difference in running average water mole fraction between the current point and the previous point, provided that it is one minute in the past, and that both the current point and previous point are of good quality according to "AverageQuality"; this value is otherwise NA if these conditions are not satisfied
    Data$WaterGradient_PERmin <- round(WaterGradients, digits=9)
  ### The average "WaterGraideints" from the current point up to a set time in the future (as defined in the function), only including points of good quality according to "AverageQuality"
    Data$WaterAverageGradient_PERmin <- round(AverageGradient, digits=9)
  ### This is the number of points used in each running average "WaterGradients"
    Data$WaterAverageGradientNumberOfPoints <- GradientNumberOfPoints
  ### This is the time difference from the first to the last data point used to derive a running average "WaterGradients"
    Data$WaterAverageGradientTimeDifference_s <- round(GradientTimeDifference, digits=3)
  ### A logical based on whether the number of points used to derive each running average "WaterGradient" and the time difference surpass a threshold, as defined in the function
    Data$WaterAverageGradientQuality <- AverageGradientQuality
  ### This builds on the FALSEs in "AverageGradientQuality" and adds FALSEs where the "AverageGradient" exceeds a threshold as defined in the function; this also adds more FALSEs after existing FALSEs, for a certain pre-defined delay as defined in the function
    Data$WaterQuality <- WaterQuality
    
  #Set all water output to NA, except for water quality which is set to true, if the water status is off
  } else if (status == "off"){
    print("water averaging status off")
  ### This is the running average water mole fraction taking all available data points from the current time +/- half the averaging time, as defined in the function
    Data$WaterAverages <- NA
  ### This is the number of points used in each running average water mole fraction
    Data$WaterNumberOfPoints <- NA
  ### This is the time difference from the first to the last data point used to derive a running average water mole fraction
    Data$WaterTimeDiffference_s <- NA
  ### A logical based on whether the number of points used to derive each running average water mole fraction and the time difference surpass a threshold, as defined in the function
    Data$WaterAverageQuality <- NA
  ### Finds the difference in running average water mole fraction between the current point and the previous point, provided that it is one minute in the past, and that both the current point and previous point are of good quality according to "AverageQuality"; this value is otherwise NA if these conditions are not satisfied
    Data$WaterGradient_PERmin <- NA
  ### The average "WaterGraideints" from the current point up to a set time in the future (as defined in the function), only including points of good quality according to "AverageQuality"
    Data$WaterAverageGradient_PERmin <- NA
  ### This is the number of points used in each running average "WaterGradients"
    Data$WaterAverageGradientNumberOfPoints <- NA
  ### This is the time difference from the first to the last data point used to derive a running average "WaterGradients"
    Data$WaterAverageGradientTimeDifference_s <- NA
  ### A logical based on whether the number of points used to derive each running average "WaterGradient" and the time difference surpass a threshold, as defined in the function
    Data$WaterAverageGradientQuality <- NA
  ### This builds on the FALSEs in "AverageGradientQuality" and adds FALSEs where the "AverageGradient" exceeds a threshold as defined in the function; this also adds more FALSEs after existing FALSEs, for a certain pre-defined delay as defined in the function
    Data$WaterQuality <- TRUE
  
  #Return the water quality information in the original data frame
  }
  return(Data)
}

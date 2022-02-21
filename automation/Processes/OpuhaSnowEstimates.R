#R functions needed to generate Snow Storage estimates
#
#Tim Kerr
#Aqualinc
#July 2015

#January 2018 Tim Kerr Added functions to access ECS data


#*********************************************
# Function to read the Opuha Water Ltd Data, provided by ECS Ltd. add it to an R data file that holds it all, and convert into a zoo object
#*********************************************
ReformatECSData<-function(ECSFile="G:\\ARL Projects\\RD Projects\\RD18004_Lake Opuha Further Work\\Data\\ToAqualinc.zip",CompleteECSDataFile="CompleteECSData.RDS")
  
{
  if (!require(XML)) install.packages("XML"); library(XML)    #Package to handle the xml format and structure
  if (!require(zoo)) install.packages("zoo"); library(zoo)    #Package to handle time series
  if (!require(xts)) install.packages("xts"); library(xts)    #Package to handle time series
  #browser()
  #Read the data files
  xmlfile=xmlParse(unzip(ECSFile))
  #browser()
  CompleteECSData <- readRDS(CompleteECSDataFile)
  
  #Get all the site names
  SiteNumbers <- xpathSApply(xmlfile,"/Hilltop/Measurement/@SiteName")
  SiteNames <-xpathSApply(xmlfile,"/Hilltop/Measurement/DataSource/@Name")
  
  #Build up a time series for each site
  ListsOfTimeSeries <- lapply(SiteNumbers, function(SiteNumber){
    DateTimes <- xpathSApply(xmlfile,paste0("/Hilltop/Measurement[@SiteName='",SiteNumber,"']/Data/E/T"),xmlValue)
    #Convert to a POSix Time Series
    DateTimeFormatted <- as.POSIXct(DateTimes,format="%Y-%m-%dT%H:%M:%S",tz="Etc/GMT-12")
    DataValues <- as.numeric(xpathSApply(xmlfile,paste0("/Hilltop/Measurement[@SiteName='",SiteNumber,"']/Data/E/I1"),xmlValue))
    DataZoo <- zoo(DataValues, order.by = DateTimeFormatted )
    return(DataZoo)
  })
  names(ListsOfTimeSeries) <- SiteNames
  
  #Merge the ECS data into the "complete" data, and save it for next time
  # MergeTo1 <- function(FirstZoo,SecondZoo){
  #   #browser()
  #   BothZoos <- merge.zoo(FirstZoo,SecondZoo)
  #   BothZoos$OutZoo   <- ifelse(is.na(BothZoos$FirstZoo),BothZoos$SecondZoo,BothZoos$FirstZoo)
  #   return(BothZoos$OutZoo)
  # }
  
  #Function to merge to the first zoo, the zoo in the second list which has the same name.
  #This replaces the previous method which simply assumed all data were available so there was a 1:1 match.
  MergeTo1 <- function(ListOfCompleteDataZoo,ListOfNewDataZoos){

    #Work through the name of each zoo in the complete data list
    MergedZooList <- lapply(names(ListOfCompleteDataZoo), function(NameOfInterest){
 
      #Check whether each name exists in the new data list
      NewZooListIndex <- which(names(ListOfNewDataZoos) == NameOfInterest)
      #If it does, merge it
      FirstZoo <- ListOfCompleteDataZoo[[NameOfInterest]]
      if(length(NewZooListIndex) == 1) {
        
        SecondZoo <- ListOfNewDataZoos[[NameOfInterest]]
        BothZoos <- merge.zoo(FirstZoo,SecondZoo)
        BothZoos$OutZoo   <- ifelse(is.na(BothZoos$FirstZoo),BothZoos$SecondZoo,BothZoos$FirstZoo)
        OutZoo <- BothZoos$OutZoo
        #If it doesn't just leave  it as is
      } else {OutZoo <- FirstZoo}
      return(OutZoo)
    })
    names(MergedZooList) <- names(ListOfCompleteDataZoo)
    return(MergedZooList)
  }

  
  CompleteECSData                 <- MergeTo1(CompleteECSData, ListsOfTimeSeries)
  #CompleteECSData                 <- 
  saveRDS(CompleteECSData,CompleteECSDataFile) 
  
  #Extract the Lake Opuha Rain data and aggregate it to daily totals
  LakeOpuhaDailyRain               <- apply.daily(CompleteECSData[['Opuha Rain']],sum)
  #index(LakeOpuhaDailyRain)        <- as.Date(index(LakeOpuhaDailyRain),tz="Etc/GMT-12")                      #Ditch the hours from the Date time index
  
  index(LakeOpuhaDailyRain)        <- as.Date(index(LakeOpuhaDailyRain),tz="Pacific/Auckland")                      #Ditch the hours from the Date time index
  
  
  #Extract the Fox Peak temperatures and aggregate to daily extremes
  FoxPeakDailyMaxTemperature       <- apply.daily(CompleteECSData[['Fox Air']],max)
  #index(FoxPeakDailyMaxTemperature)<- as.Date(index(FoxPeakDailyMaxTemperature),tz="Etc/GMT-12")
  index(FoxPeakDailyMaxTemperature)<- as.Date(index(FoxPeakDailyMaxTemperature),tz="Pacific/Auckland")
  FoxPeakDailyMinTemperature       <- apply.daily(CompleteECSData[['Fox Air']],min)
  #index(FoxPeakDailyMinTemperature)<- as.Date(index(FoxPeakDailyMinTemperature),tz="Etc/GMT-12")
  index(FoxPeakDailyMinTemperature)<- as.Date(index(FoxPeakDailyMinTemperature),tz="Pacific/Auckland")
  
  #Get Dobson temperature data as well
  DobsonDailyMaxTemperature       <- apply.daily(CompleteECSData[['Dobson Air']],max)
  index(DobsonDailyMaxTemperature)<- as.Date(index(DobsonDailyMaxTemperature),tz="Pacific/Auckland")
  DobsonDailyMinTemperature       <- apply.daily(CompleteECSData[['Dobson Air']],min)
  index(DobsonDailyMinTemperature)<- as.Date(index(DobsonDailyMinTemperature),tz="Pacific/Auckland")

  #If Fox temperatures are missing, use Dobson Temperatures but add 1.74 for max temps, and 0.74 for min temps (derived from comparison).
  MergedMaxTemperature <- merge(Fox=FoxPeakDailyMaxTemperature,DobsonOffset=DobsonDailyMaxTemperature+1.74)
  MergedMaxTemperature$Primary <- MergedMaxTemperature$Fox
  MergedMaxTemperature$Primary[is.na(MergedMaxTemperature$Primary)] <- MergedMaxTemperature$DobsonOffset[is.na(MergedMaxTemperature$Primary)]
  
  MergedMinTemperature <- merge(Fox=FoxPeakDailyMinTemperature,DobsonOffset=DobsonDailyMinTemperature+0.74)
  MergedMinTemperature$Primary <- MergedMinTemperature$Fox
  MergedMinTemperature$Primary[is.na(MergedMinTemperature$Primary)] <- MergedMinTemperature$DobsonOffset[is.na(MergedMinTemperature$Primary)]
   
  #Combine the temperature and rain data
  ClimateData                      <- merge(LakeOpuhaDailyRain,MergedMaxTemperature$Primary,MergedMinTemperature$Primary)
  
  #Rename the data columns
  names(ClimateData)               <- c("Precip","MaxT","MinT")
  return(ClimateData)
} #end of the ReformatECSData function

#**********************************************
# Function to merge the recent climate data with the primary climate data file
# and save it to an updated version of the primary file
#**********************************************

UpdateClimateData <- function(CompleteDataFileName="\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaClimateDataComplete.csv",RecentData)
#CompleteDataFileName="\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaClimateDataComplete.csv"
{
  
  #PrimaryData  <-  read.zoo(CompleteDataFileName,colClasses=c("character","double","double","double"),
 #                           index.column=1,sep=",",format="%y/%m/%d",header=TRUE,regular=FALSE)
  PrimaryData  <-  read.zoo(CompleteDataFileName,colClasses=c("character","double","double","double"),
                            index.column=1,sep=",",format="%Y/%m/%d",header=TRUE,regular=FALSE)
  Both <- merge(PrimaryData,RecentData, all = TRUE)
  Both$MaxT<- ifelse(is.na(Both$MaxT.RecentData),Both$MaxT.PrimaryData,Both$MaxT.RecentData)
  Both$MinT<- ifelse(is.na(Both$MinT.RecentData),Both$MinT.PrimaryData,Both$MinT.RecentData)
  Both$Precip<- ifelse(is.na(Both$Precip.RecentData),Both$Precip.PrimaryData,Both$Precip.RecentData) #Temporary fix while Fairlie rainfall is in error. Simply default to the primary data, which needs to updated manually each day.
  #Both$Precip<- ifelse(is.na(Both$Precip.RecentData),Both$Precip.PrimaryData,Both$Precip.RecentData)
  #UpdatedData<-data.frame(Date=format(index(Both), "%y/%m/%d"),Both[,c("MaxT","MinT","Precip")],check.names=FALSE,row.names=NULL)
  #write.csv(UpdatedData,file = CompleteDataFileName,quote=FALSE,row.names=FALSE)
  UpdatedData<-data.frame(Date=format(index(Both), "%Y/%m/%d"),Both[,c("MaxT","MinT","Precip")],check.names=FALSE,row.names=NULL)
  write.csv(UpdatedData,file = CompleteDataFileName,quote=FALSE,row.names=FALSE)
}


#******************************************************
#Function to convert a timeseries of climate data into snow storage estimates
#******************************************************
# This function takes daily temperature and precipiation
# data from the Fairlie climate station and
# spatially interpolates it, then estimates snow
# storage.
# The snow accumulates wherever it is precipitating
# and the temperature is below a temperature threshold.
# The snow melts wherever the temperature is above
# another temperature threshold.
# The climate data input file is a four column comma sepated
# variable file. The columns are, and are labelled:
# Date, MinT, MaxT, Precip
# The date is in yy/mm/dd format
# MinT is the minimum temperature for each day in degrees C
# MaxT is the maximum temperature for each day in degrees C
# Precip is the observed precipitation in millimetres
# Note the order of the columns is not important

## the to do list
## add function arguments of elevation, mask and mean annual precipitation
## describe the requirements of the input data

SnowSim_Opuha<-function(start_year=2017,end_year=2017,InputWorkingDirectory="\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input")
{
  
  ## Load required packages
  if (!require(maptools)) install.packages("maptools"); library(maptools)   
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)     
  if (!require(raster)) install.packages("raster"); library(raster)    

  
  ##Assign constants
  original_directory          <- getwd()
  setwd(InputWorkingDirectory)		                  #Set the working directory
  #start_year			            <- 2015	#first year to be processed
  #end_year			              <- 2015	#last year to be processed
  temperature_snowrain_threshold 	<- 2.5  	                                                #in deg C.
  temperature_melt_threshold 	<- 0   	                                                      #in deg C.
  temperature_melt_factor 	  <- 5 	                                                        # in mm/oC/day
  
  ## Load data
  OpuhaWeather			          <- read.csv("OpuhaClimateDataComplete.csv", header=T)			#Climate data
  Opuha_catchment_mask 	      <- raster(readGDAL("OpuhaCatchment.tif"))	#Spatial mask
  precip_interpolation_grid 	<- raster(readGDAL("OpuhaPrecipitationGrid.tif"))	#Interpolation grid for precipiation
  temperature_interpolation_grid 	<- raster(readGDAL("OpuhaTemperatureGrid.tif")) #Interpolation grid for temperature
  
  OpuhaWeather$AverageT       <- ( OpuhaWeather$MaxT + OpuhaWeather$MinT ) / 2	#this is used as the daily temperature 
  #OpuhaWeather$Date           <- as.Date(OpuhaWeather$Date,"%y/%m/%d")		#transforms the text dates into R dates. Obsolete as changed the date format to include 4 digit year
  OpuhaWeather$Date           <- as.Date(OpuhaWeather$Date,"%Y/%m/%d")		#transforms the text dates into R dates
  multiyear_snow_storage      <- data.frame(Date= as.Date(character()),SWE= numeric(),Loss=numeric())   #pre allocate the output dataframe
  
  for (year in start_year : end_year) {						#loop through each year
    snow_storage               <- data.frame(Date= as.Date(rep("01/01/01",365)),SWE= numeric(365),Loss=numeric(365)) #pre allocate the output dataframe
    accumulated_snow_latest    <- precip_interpolation_grid * 0 
    
    for (day in 1 : 365){								#loop through each day of the year
      accumulated_snow_yesterday  <- accumulated_snow_latest
      current_date                <- as.Date(paste(year,"/03/31",sep=""),"%Y/%m/%d") + day  	#Use the Southern Hemisphere hydrological year
      month                       <- format(current_date, "%m")
      day_of_month                <- format(current_date, "%d")
      print(paste("Processing:",current_date))
      current_julian_day          <- strftime(current_date, format = "%j")			#Get the current julian day number
      current_index               <- match(current_date,OpuhaWeather$Date)			#Find the array index for the current date
      daily_temperature           <- OpuhaWeather$AverageT[current_index] + temperature_interpolation_grid #daily spatial temperature 
      daily_precipitation         <- OpuhaWeather$Precip[current_index] * precip_interpolation_grid 	 #daily spatial precipiation
      snowfall_potential          <- daily_temperature < temperature_snowrain_threshold	#Areas where precipiation falls as snow
      daily_snow_gain             <- daily_precipitation * snowfall_potential 	
      daily_liquid_precip         <- daily_precipitation * !snowfall_potential
      accumulated_snow            <- (accumulated_snow_yesterday + daily_snow_gain)
      melt_areas                  <- daily_temperature > temperature_melt_threshold
      daily_temperature_loss      <- (daily_temperature - temperature_melt_threshold) * melt_areas * temperature_melt_factor 
      daily_potential_snow_loss   <- daily_temperature_loss
      accumulated_snow_latest     <- (accumulated_snow - daily_potential_snow_loss) * ((accumulated_snow - daily_potential_snow_loss) > 0)
      daily_snow_loss             <- (accumulated_snow - accumulated_snow_latest) * ((accumulated_snow - accumulated_snow_latest) > 0 )
      print(paste("SWE:",cellStats(accumulated_snow,sum)))    
      snow_storage$Loss[day]      <- cellStats((daily_liquid_precip + daily_snow_loss) * Opuha_catchment_mask,sum)
      snow_storage$SWE[day]       <- cellStats(accumulated_snow_latest * Opuha_catchment_mask,sum)
      snow_storage$Date[day]      <- current_date
    }   #end the day loop
    multiyear_snow_storage      <- rbind(multiyear_snow_storage,snow_storage)
  }     #end the year loop
  write.csv(multiyear_snow_storage,file = "OpuhaSnowDataRecent.csv",row.names=FALSE)
  setwd(original_directory)
}   #end of snow-storage estimation function

SnowSim_OpuhaV5<-function(start_year=1981,end_year=2016,
                          DataDirectory="\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input",
                          #StartDay=1,EndDay=365,
                          #OutSnowGrid=FALSE,
                          #StartSnowGrid=NULL,
                          temperature_snowrain_threshold 	= 2.5,  	                                                #in deg C.
                          temperature_melt_threshold 	= 0,   	                                                      #in deg C.
                          temperature_melt_factor 	  = 5, 	                                                        # in mm/oC/day
                          PrecipMultiplier          = 1,                                                           # unitless
                          MaximumMeltFactor         = 8,                                                          # in mm/oC/day
                          MinimumMeltFactorDayNumber = 81,                                   #Days since April 1st. The default is mid June.
                          MaximumMeltFactorDayNumber = 214,                                  #Days since April 1st. This default is early November.
                          WeatherFile = "OpuhaClimateDataComplete.csv",
                          CatchmentMaskFile = "OpuhaCatchment.tif",
                          PrecipInterpFile = "OpuhaPrecipitationGrid.tif",
                          TemperatureInterpFile = "OpuhaTemperatureGrid.tif"
)
  {
  
  ## Load required packages
  if (!require(maptools)) install.packages("maptools"); library(maptools)   
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)     
  if (!require(raster)) install.packages("raster"); library(raster)    
  
  
  ##Assign constants
  original_directory          <- getwd()
  setwd(InputWorkingDirectory)		                  #Set the working directory
  #start_year			            <- 2015	#first year to be processed
  #end_year			              <- 2015	#last year to be processed
  #temperature_snowrain_threshold 	<- 2.5  	                                                #in deg C.
  #temperature_melt_threshold 	<- 0   	                                                      #in deg C.
  #temperature_melt_factor 	  <- 5 	                                                        # in mm/oC/day
  
  ## Load data
  OpuhaWeather			          <- read.csv(file.path(DataDirectory,WeatherFile), header=T)			#Climate data
  Opuha_catchment_mask 	      <- raster(readGDAL(file.path(DataDirectory,CatchmentMaskFile)))	#Spatial mask
  precip_interpolation_grid 	<- raster(readGDAL(file.path(DataDirectory,PrecipInterpFile)))	#Interpolation grid for precipiation
  temperature_interpolation_grid 	<- raster(readGDAL(file.path(DataDirectory,TemperatureInterpFile))) #Interpolation grid for temperature
  
  OpuhaWeather$AverageT       <- ( OpuhaWeather$MaxT + OpuhaWeather$MinT ) / 2	#this is used as the daily temperature 
  #OpuhaWeather$Date           <- as.Date(OpuhaWeather$Date,"%y/%m/%d")		#transforms the text dates into R dates. Obsolete as changed the date format to include 4 digit year
  OpuhaWeather$Date           <- as.Date(OpuhaWeather$Date,"%Y/%m/%d")		#transforms the text dates into R dates
  multiyear_snow_storage      <- data.frame(Date= as.Date(character()),SWE= numeric(),Loss=numeric())   #pre allocate the output dataframe
  
  #Seasonlly varying melt factor
  MFMidDay <- mean(c(MinimumMeltFactorDayNumber,MaximumMeltFactorDayNumber))
  MaximumDaysPerYear <- 366
  MaximumFirst <- MaximumMeltFactorDayNumber < MinimumMeltFactorDayNumber
  M_Central <- (-pi/2-(2*pi*(MinimumMeltFactorDayNumber-MFMidDay)/MaximumDaysPerYear))/(MinimumMeltFactorDayNumber -MFMidDay)
  M_Start <- ifelse(MaximumFirst,(-pi/2+(2*pi*(MinimumMeltFactorDayNumber-MFMidDay)/MaximumDaysPerYear))/(0.5*MaximumDaysPerYear-abs(MinimumMeltFactorDayNumber-MFMidDay)),
                    (-pi/2-(2*pi*(MinimumMeltFactorDayNumber-MFMidDay)/MaximumDaysPerYear))/(0.5*MaximumDaysPerYear-abs(MinimumMeltFactorDayNumber-MFMidDay)))
  C_Start <- ifelse(MaximumFirst,(pi/2 -2*pi*(MaximumMeltFactorDayNumber - MFMidDay)/MaximumDaysPerYear )-M_Start*(MaximumMeltFactorDayNumber-MFMidDay),
                    (-pi/2 -2*pi*(MinimumMeltFactorDayNumber - MFMidDay)/MaximumDaysPerYear )-M_Start*(MinimumMeltFactorDayNumber-MFMidDay))
  M_Ends <- ifelse(MaximumFirst,(-pi/2+(2*pi*(MinimumMeltFactorDayNumber-MFMidDay)/MaximumDaysPerYear))/(0.5*MaximumDaysPerYear-abs(MinimumMeltFactorDayNumber-MFMidDay)),
                   (-pi/2-(2*pi*(MinimumMeltFactorDayNumber-MFMidDay)/MaximumDaysPerYear))/(0.5*MaximumDaysPerYear-abs(MinimumMeltFactorDayNumber-MFMidDay)))
  C_End  <- ifelse(MaximumFirst,(-pi/2 -2*pi*(MinimumMeltFactorDayNumber - MFMidDay)/MaximumDaysPerYear )-M_Ends*(MinimumMeltFactorDayNumber-MFMidDay),
                   (pi/2 -2*pi*(MaximumMeltFactorDayNumber - MFMidDay)/MaximumDaysPerYear )-M_Ends*(MaximumMeltFactorDayNumber-MFMidDay))
  
  Days <- seq(1:MaximumDaysPerYear)
  Offsets <- Days - MFMidDay
  FractionOfPeriod <- Offsets / MaximumDaysPerYear
  FirstPart <- Days < min(MaximumMeltFactorDayNumber,MinimumMeltFactorDayNumber)
  MidPart   <- (Days >= min(MaximumMeltFactorDayNumber,MinimumMeltFactorDayNumber)) & (Days < max(MaximumMeltFactorDayNumber,MinimumMeltFactorDayNumber))
  LastPart <- (Days >= max(MaximumMeltFactorDayNumber,MinimumMeltFactorDayNumber))
  PhiStart <- M_Start * Offsets + C_Start
  PhiMid   <- M_Central * Offsets
  PhiEnd   <- M_Ends * Offsets + C_End
  Start    <- FirstPart * sin(2 * pi * FractionOfPeriod + PhiStart)
  Middle   <- MidPart * sin(2 * pi * FractionOfPeriod + PhiMid)
  End      <- LastPart*sin(2*pi*FractionOfPeriod+PhiEnd)
  MeltFactorVariation <- (Start + Middle + End + 1 )/ 2
  
  for (year in start_year : end_year) {						#loop through each year
    snow_storage               <- data.frame(Date= as.Date(rep("01/01/01",365)),SWE= numeric(365),Loss=numeric(365)) #pre allocate the output dataframe
    accumulated_snow_latest    <- precip_interpolation_grid * 0 
    
    for (day in 1 : 365){								#loop through each day of the year
      accumulated_snow_yesterday  <- accumulated_snow_latest
      current_date                <- as.Date(paste(year,"/03/31",sep=""),"%Y/%m/%d") + day  	#Use the Southern Hemisphere hydrological year
      month                       <- format(current_date, "%m")
      day_of_month                <- format(current_date, "%d")
      print(paste("Processing:",current_date))
      current_julian_day          <- strftime(current_date, format = "%j")			#Get the current julian day number
      MinimumMeltFactor           <- temperature_melt_factor
      melt_factor                 <- MinimumMeltFactor + (MaximumMeltFactor - MinimumMeltFactor) * MeltFactorVariation
      current_index               <- match(current_date,OpuhaWeather$Date)			#Find the array index for the current date
      daily_temperature           <- OpuhaWeather$AverageT[current_index] + temperature_interpolation_grid #daily spatial temperature 
      daily_precipitation         <- OpuhaWeather$Precip[current_index] * precip_interpolation_grid 	 #daily spatial precipiation
      snowfall_potential          <- daily_temperature < temperature_snowrain_threshold	#Areas where precipiation falls as snow
      daily_snow_gain             <- daily_precipitation * snowfall_potential 	
      daily_liquid_precip         <- daily_precipitation * !snowfall_potential
      accumulated_snow            <- (accumulated_snow_yesterday + daily_snow_gain)
      melt_areas                  <- daily_temperature > temperature_melt_threshold
      daily_temperature_loss      <- (daily_temperature - temperature_melt_threshold) * melt_areas * melt_factor[day] 
      daily_potential_snow_loss   <- daily_temperature_loss
      accumulated_snow_latest     <- (accumulated_snow - daily_potential_snow_loss) * ((accumulated_snow - daily_potential_snow_loss) > 0)
      daily_snow_loss             <- (accumulated_snow - accumulated_snow_latest) * ((accumulated_snow - accumulated_snow_latest) > 0 )
      print(paste("SWE:",cellStats(accumulated_snow,sum)))    
      snow_storage$Loss[day]      <- cellStats((daily_liquid_precip + daily_snow_loss) * Opuha_catchment_mask,sum)
      snow_storage$SWE[day]       <- cellStats(accumulated_snow_latest * Opuha_catchment_mask,sum)
      snow_storage$Date[day]      <- current_date
    }   #end the day loop
    multiyear_snow_storage      <- rbind(multiyear_snow_storage,snow_storage)
  }     #end the year loop
  write.csv(multiyear_snow_storage,file = file.path(DataDirectory,"OpuhaSnowDataRecent.csv"),row.names=FALSE)
  setwd(original_directory)
}   #end of snow-storage estimation function


#**********************************************
# Function to merge the latest snow estimate data with the primary snow data file
# and save it to an updated version of the primary file
#**********************************************

UpdateSnowData <- function(CompleteDataFileName="\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaSnowDataComplete.csv",
                           RecentDataFileName="\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaSnowDataRecent.csv")
#  CompleteDataFileName="\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaSnowDataComplete.csv"
#  RecentDataFileName="\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaSnowDataRecent.csv"
{
  
  InputDirectory       <-  dirname(CompleteDataFileName)
  OutputFileName       <-  CompleteDataFileName
  PrimaryData  <-  read.zoo(CompleteDataFileName,colClasses=c("character","double","double"),
                            index.column=1,sep=",",format="%Y-%m-%d",header=TRUE,regular=FALSE)
  RecentData  <-  read.zoo(RecentDataFileName,colClasses=c("character","double","double"),
                            index.column=1,sep=",",format="%Y-%m-%d",header=TRUE,regular=FALSE)
  Both <- merge(PrimaryData,RecentData, all = TRUE)
  Both$SWE<- ifelse(is.na(Both$SWE.RecentData),Both$SWE.PrimaryData,Both$SWE.RecentData)
  Both$Loss<- ifelse(is.na(Both$Loss.RecentData),Both$Loss.PrimaryData,Both$Loss.RecentData)
  UpdatedData<-data.frame(Date=format(index(Both), "%Y-%m-%d"),Both[,c("SWE","Loss")],check.names=FALSE,row.names=NULL)
  write.csv(UpdatedData,file = OutputFileName,quote=FALSE,row.names=FALSE)
}

#**************************************
#  Function to find the maximum of the long-term annual daily median
#*************************************
MedianMaximum <- function(InputFileName = "\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaSnowDataComplete.csv"){
 
  #load libraries
  if (!require(zoo)) install.packages("zoo"); library(zoo)    
  if (!require(xts)) install.packages("xts"); library(xts)    
  if (!require(plyr)) install.packages("plyr"); library(plyr)    
  if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)    
  if (!require(scales)) install.packages("scales"); library(scales)    
  if (!require(reshape2)) install.packages("reshape2"); library(reshape2)    
  if (!require(grid)) install.packages("grid"); library(grid)    
  if (!require(plotly)) install.packages("plotly"); library(plotly)    
  if (!require(devtools)) install.packages("devtools"); library(devtools)    

  
  #Load in the snow time series data
  SnowWaterEquivalent  <-  read.zoo(InputFileName,index.column="Date",sep=",",format="%Y-%m-%d",header=TRUE,regular=FALSE,colClasses=c("character","double","double")) 
  
  #****Remove THE MISSING YEARS*******
  missing_data<-seq(as.Date("1989-01-01"),as.Date("1992-12-31"),by="day")
  SnowWaterEquivalent<-SnowWaterEquivalent[!time(SnowWaterEquivalent) %in% missing_data]
  
  #build a vector of day of year
  DayOfYear   <-  strptime(index(SnowWaterEquivalent),"%Y-%m-%d")$yday+1
  Percentiles <-  aggregate(SnowWaterEquivalent$SWE, DayOfYear, function(x) quantile(x, c(0.05,0.25,0.5,0.75,0.95)) )
  Percentiles<- apply(Percentiles,2, function(x) loess(x ~ as.numeric(index(Percentiles)),span=0.3)$fitted )   #Smooth the percentiles
  Percentiles <- as.data.frame(Percentiles)       #Convert back into a data frame (from the matrix that apply generates on the previous line)
  Percentiles[Percentiles< 0.001]<- 0                   #set the small number to 0 for tidiness
  MedianMaximum <- max(Percentiles[,3])
  return(MedianMaximum)
}




#*************************************
#  Function to plot the data with the Y axis scaled to cubic metres of water, and to prepare graph data for use in DyGraphs
#*************************************
#  Remove the data for future dates


GraphOpuhaSnowM3Y<-function(InputFileName = "\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaSnowDataComplete.csv",CurrentYear= 2017)
{
  browser()
  #  InputFileName        <-  "\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaSnowDataComplete.csv"
  #  CurrentYear          <-  2015
  
  InputDirectory       <-  dirname(InputFileName)
  OpuhaWeather  		   <- read.csv(file.path(InputDirectory,"OpuhaClimateDataComplete.csv"), header=T,stringsAsFactors = FALSE)			#Climate data  
  
  OutputFileName       <-  file.path(InputDirectory,"OpuhaGraphData.csv")
  OutputPlotFileName   <-  file.path(InputDirectory,"OpuhaSnowGraphM3.pdf")
  
  
  #load libraries
  if (!require(zoo)) install.packages("zoo"); library(zoo)    
  if (!require(xts)) install.packages("xts"); library(xts)    
  if (!require(plyr)) install.packages("plyr"); library(plyr)    
  if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)    
  if (!require(scales)) install.packages("scales"); library(scales)    
  if (!require(reshape2)) install.packages("reshape2"); library(reshape2)    
  if (!require(grid)) install.packages("grid"); library(grid)    
  if (!require(plotly)) install.packages("plotly"); library(plotly)    
  if (!require(devtools)) install.packages("devtools"); library(devtools)

  #Load in the snow time series data
  SnowWaterEquivalent  <-  read.zoo(InputFileName,index.column="Date",sep=",",format="%Y-%m-%d",header=TRUE,regular=FALSE,colClasses=c("character","double","double")) 
  #SnowWaterEquivalent  <-  SnowWaterEquivalent / 43356988   #turns the number into a value relative to the maximum of the median. This was last updated 23/8/2017. This should be calculated dynamically.
  MedianMaximum <- MedianMaximum(InputFileName = InputFileName)
  SnowWaterEquivalent  <-  SnowWaterEquivalent / MedianMaximum   #turns the number into a value relative to the maximum of the median.
  
  ScaleBackToRaw <- MedianMaximum
  #ScaleRawToWaterBalanceM3 <- 0.8 * 25 * 25 / 1000
  ScaleRawToWaterBalanceM3 <- 0.84 * 25 * 25 / 1000   #The 0.84 comes from water balance estimate of mean annual rainfall, and correction for the precipitation mulltiplier determined from model calibration
  
  #****Remove THE MISSING YEARS*******
  missing_data<-seq(as.Date("1989-01-01"),as.Date("1992-12-31"),by="day")
  SnowWaterEquivalent<-SnowWaterEquivalent[!time(SnowWaterEquivalent) %in% missing_data]
  
  #build a vector of day of year
  DayOfYear   <-  strptime(index(SnowWaterEquivalent),format="%Y-%m-%d")$yday+1
  Percentiles <-  aggregate(SnowWaterEquivalent$SWE, DayOfYear, function(x) quantile(x, c(0.05,0.25,0.5,0.75,0.95)) )
  Percentiles<- apply(Percentiles,2, function(x) loess(x ~ as.numeric(index(Percentiles)),span=0.3)$fitted )   #Smooth the percentiles
  Percentiles <- as.data.frame(Percentiles)       #Convert back into a data frame (from the matrix that apply generates on the previous line)
  Percentiles[Percentiles< 0.001]<- 0                   #set the small number to 0 for tidiness

  #get the Current year's data
  isLeapYear           <-   (CurrentYear %% 4) == 0
  year.length          <-   365
  if (isLeapYear) {year.length = 366}
  CurrentYearFirstDay  <-   paste0(CurrentYear,"-01-01")
  CurrentYearDates     <-   seq(as.Date(CurrentYearFirstDay),length.out=year.length, by="days")   #build a vector for that year
  CurrentYearSnow      <-   SnowWaterEquivalent[CurrentYearDates,"SWE"]
  CurrentYearSnow      <-   round(CurrentYearSnow, digits=2)                            #round to 2 decimal places
  
  #Get the year before's data
  isLeapYear           <-   (CurrentYear %% 4) == 1
  year.length          <-   365
  if (isLeapYear) {year.length = 366}
  PreviousYearFirstDay  <-   seq(as.Date(CurrentYearFirstDay),length=2,by="-1 years")[2]        
  PreviousYearDates     <-   seq(as.Date(PreviousYearFirstDay),length.out=year.length, by="days")
  PreviousYearSnow      <-   SnowWaterEquivalent[PreviousYearDates,"SWE"]
  PreviousYearSnow      <-   round(PreviousYearSnow, digits=2)
  
  #create a data ready for export
  GraphDataDates       <-    seq.Date(from = as.Date(CurrentYearFirstDay),by = "day",length = 365)
  GraphData          <-    data.frame(Date=GraphDataDates[1:365],"Current.Year"=coredata(CurrentYearSnow)[1:365],
                                      "Previous.Year"=coredata(PreviousYearSnow)[1:365],
                                      Percentiles=Percentiles[1:365,],
                                      stringsAsFactors=FALSE)
  #Scale data to millions of cubic metres
  GraphData[,-1]     <- GraphData[,-1] * ScaleBackToRaw * ScaleRawToWaterBalanceM3 / 1000000
  
  
  
  colnames(GraphData)[c(4,5,6,7,8)]   <-    c("Percentile.5th","Percentile.25th","median","Percentile.75th","Percentile.95th")
  #GraphData[is.na(GraphData)]<- ""
 
  #remove missing data at the end, but it can't be later than yesterday
  LastDate <-  min(max(as.Date(OpuhaWeather$Date, "%Y/%m/%d")),Sys.Date())
  GraphData$Current.Year[GraphData$Date > LastDate] <- NA
  
  #Add the percentile data to the graph data
  GraphData$CurrentYearSWEpercentile <- apply(GraphData,1, function(x) {

    Percentiles <- c(5,25,50,75,95)
    PercentileValues <- as.numeric(as.character(x[c("Percentile.5th","Percentile.25th","median","Percentile.75th","Percentile.95th")]))
    TodaysPercentile <- approx(x=PercentileValues,y=Percentiles,xout=as.numeric(as.character(x['Current.Year'])),yleft=1,yright=99,ties="ordered")$y
  })
  GraphData$PreviousYearSWEpercentile <- apply(GraphData,1, function(x) {

    Percentiles <- c(5,25,50,75,95)
    PercentileValues <- as.numeric(as.character(x[c("Percentile.5th","Percentile.25th","median","Percentile.75th","Percentile.95th")]))
    TodaysPercentile <- approx(x=PercentileValues,y=Percentiles,xout=as.numeric(as.character(x['Previous.Year'])),yleft=1,yright=99,ties="ordered")$y
  })
  
  #write the formatted file to output
  write.csv(GraphData,OutputFileName,row.names=FALSE,quote=FALSE)

  #Plot functions
  pdf(OutputPlotFileName,paper="a4r",width=10,height=7)
  print.eval=TRUE
  graphdata.reshaped<- melt(GraphData,measure.vars=c("Percentile.5th","Percentile.25th","median","Percentile.75th","Percentile.95th","Previous.Year","Current.Year"))
  
  #remove missing data at the end
  #LastDate <-  max(as.Date(OpuhaWeather$Date, "%Y/%m/%d"))
  #graphdata.reshaped$value[graphdata.reshaped$Date > LastDate & graphdata.reshaped$variable == "Current.Year"] <- NA  
  #graphdata.reshaped$Date <- as.Date(graphdata.reshaped$Date)

  graph <- ggplot(data=graphdata.reshaped)+
    #geom_line(aes(x=as.Date(Date),y=value,colour=variable,linetype=variable),size=1.2) +
    geom_line(aes(x=Date,y=value,colour=variable,linetype=variable),size=1.2) +
    scale_linetype_manual(values=c("solid","dashed","solid","dashed",rep("solid",4)))+
    scale_color_manual(values=c("light grey","grey","black","grey","light grey","pink","red")) +
    theme_bw(base_size=18) +
    theme(axis.title.y=element_text(vjust=2.5),legend.title=element_blank()) +
    theme(legend.key = element_blank(),plot.margin=unit(c(3,0,0,3),"cm")) +
    xlab("") +
    ylab("Snow storage\n (Millions of cubic metres)") +
    #axis(2, at=TickMarkLocations,labels=round(TickMarkLocations * ScaleBackToRaw * ScaleRawToWaterBalanceM3 / 1000000,0),line=3.5,las=1)
    #scale_y_continuous(breaks=seq(0,1.4,0.2)) +
    #scale_x_date(breaks = "1 month", labels = date_format("%b")) +
    scale_y_continuous(breaks=pretty_breaks(n=10)) +
    guides(colour=guide_legend(reverse=TRUE,keywidth = 3),linetype=guide_legend(reverse=TRUE,keywidth = 3))
  
  print(graph)
  dev.off()
  

  
    #Now do it all again for plotly. Unfortunately I have been unable to make plotly work with ggplot2. I suspect because I have been unable to install the development version of ggplot2
  #So I'll try to plot it direct in Plotly
   ColourPalette <- c("red","pink","light grey","grey","black","grey","light grey")
  ColourPalette <- setNames(ColourPalette, c("Percentile.5th","Percentile.25th","median","Percentile.75th","Percentile.95th","Previous.Year","Current.Year"))
  GraphRange <- as.numeric(as.POSIXct(c(paste0(CurrentYear,"-01-01"),paste0(CurrentYear + 1,"-01-02"))))*1000 #This is in milliseconds for some reason

    py2 <- plot_ly(GraphData, x = as.Date(GraphData$Date), y = ~GraphData$'Percentile.95th', name = 'Percentile 95th',line= list(color = "lightgrey",width=3)
      , hoverinfo = 'text', text = ~paste(format(Date, "%e %b"),'<br>', sprintf('%.2f',Percentile.95th), 'x10<sup>6</sup> m<sup>3</sup>','<br>','(95th %ile)'), mode="lines", type = 'scatter') %>%
    #add_trace(y = ~GraphData$'Percentile 95th', name = 'Percentile 95th',line= list(color = "lightgrey",width=3), mode="lines") %>%
    add_trace(y = ~GraphData$'Percentile.75th', name = 'Percentile 75th',line= list(color = "grey",width=3,dash='dash')
      , hoverinfo = 'text', text = ~paste(format(Date, "%e %b"),'<br>', sprintf('%.2f',Percentile.75th), 'x10<sup>6</sup> m<sup>3</sup>','<br>','(75th %ile)'))%>%
    add_trace(y = ~median, name = 'Median',line=list(color = "black",width=3)
      , hoverinfo = 'text', text = ~paste(format(Date, "%e %b"),'<br>', sprintf('%.2f',median), 'x10<sup>6</sup> m<sup>3</sup>','<br>','(50th %ile)'))  %>%
    add_trace(y = ~GraphData$'Percentile.25th', name = 'Percentile 25th',line = list(color = "grey",width=3,dash = 'dash')
      , hoverinfo = 'text', text = ~paste(format(Date, "%e %b"),'<br>', sprintf('%.2f',Percentile.25th), 'x10<sup>6</sup> m<sup>3</sup>','<br>','(25th %ile)'))%>%
    add_trace(y = ~GraphData$'Percentile.5th', name = 'Percentile 5th',line= list(color = "lightgrey",width=3)
      , hoverinfo = 'text', text = ~paste(format(Date, "%e %b"),'<br>', sprintf('%.2f',Percentile.5th), 'x10<sup>6</sup> m<sup>3</sup>','<br>','(5th %ile)')) %>%
    add_trace(y = ~Previous.Year, name = 'Previous year', line = list(color = "pink",width=3)
      , hoverinfo = 'text', text = ~paste(format(Date, "%e %b"),'<br>', sprintf('%.2f',Previous.Year), 'x10<sup>6</sup> m<sup>3</sup>','<br>','(',ordinal(PreviousYearSWEpercentile),'%ile)')) %>%
    add_trace(y = ~Current.Year, name = 'Current year', line = list(color = "red",width=3)
      , hoverinfo = 'text', text = ~paste(format(Date, "%e %b"),'<br>', sprintf('%.2f',Current.Year), 'x10<sup>6</sup> m<sup>3</sup>','<br>','(',ordinal(CurrentYearSWEpercentile),'%ile)')) %>%
      layout(xaxis = list(type = "date", dtick = "M3",range = GraphRange, title = "", ticklen = 5, tickformat="%b %Y")) %>%
    layout(yaxis = list(title ="Snow storage (Millions of cubic metres)" )) %>%
    layout(legend = list(traceorder = "reversed"))
      #layout(yaxis = list(hoverformat = '.2f.'))
 print(py2)             
 browser()
 # #Put the graph on the web via plotly
 # Sys.setenv("plotly_username"="timkerr37")
 # Sys.setenv("plotly_api_key"="1maimuigbz")
 # 
 # #py <- ggplotly()
 # 
 # WebPlot <- api_create(py2, filename="OpuhaSnow",fileopt="overwrite",sharing="public")
 
 #Save the plot as an html file
 htmlwidgets::saveWidget(as_widget(py2), "OpuhaSnow.html", selfcontained = TRUE)
 #Copy to the Rainfall.NZ web server
 #On Aqualinc windows server, copy to web server using the putty secure copy utility
 if ((Sys.info()['sysname'] == "Windows") & (Sys.info()['nodename'] == "APPSERVER-01")) {
   system2("C:\\Program Files (x86)\\PuTTY\\pscp.exe", args="-P 22 -v -i Rainfall.ppk OpuhaSnow.html rainfall@rainfall.nz:public_html/OWL")
 }        
} #end of function



#*************************************
#  Function to plot the data with multiple y axis. Note that this is a graph in development!!
#*************************************
#  Remove the data for future dates


GraphOpuhaSnow3Axis<-function(InputFileName = "\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaSnowDataComplete.csv",CurrentYear= 2017)
{
  
  #  InputFileName        <-  "\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C15114_OWL snow storage estimates\\automation\\Input\\OpuhaSnowDataComplete.csv"
  #  CurrentYear          <-  2015

  InputDirectory       <-  dirname(InputFileName)
  OpuhaWeather  		   <- read.csv(file.path(InputDirectory,"OpuhaClimateDataComplete.csv"), header=T)			#Climate data  
  
  OutputFileName       <-  file.path(InputDirectory,"OpuhaGraphData.csv")
  OutputPlotFileName   <-  file.path(InputDirectory,"OpuhaSnowGraphMultiScale.pdf")
  
  
  #load libraries
  if (!require(zoo)) install.packages("zoo"); library(zoo)    
  if (!require(xts)) install.packages("xts"); library(xts)    
  if (!require(plyr)) install.packages("plyr"); library(plyr)    
  if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)    
  if (!require(scales)) install.packages("scales"); library(scales)    
  if (!require(reshape2)) install.packages("reshape2"); library(reshape2)    
  if (!require(grid)) install.packages("grid"); library(grid)    
  if (!require(plotly)) install.packages("plotly"); library(plotly)    
  if (!require(devtools)) install.packages("devtools"); library(devtools)
  
  #Load in the snow time series data
  SnowWaterEquivalent  <-  read.zoo(InputFileName,index.column="Date",sep=",",format="%Y-%m-%d",header=TRUE,regular=FALSE,colClasses=c("character","double","double")) 
  SnowWaterEquivalentRaw <- SnowWaterEquivalent
  #SnowWaterEquivalent  <-  SnowWaterEquivalent / 43356988   #turns the number into a value relative to the maximum of the median. This was last updated 23/8/2017. This should be calculated dynamically.
  MedianMaximum <- MedianMaximum(InputFileName = InputFileName)
  SnowWaterEquivalent  <-  SnowWaterEquivalent / MedianMaximum   #turns the number into a value relative to the maximum of the median.
  
  
  ScaleBackToRaw <- MedianMaximum
  #ScaleRawToWaterBalanceM3 <- 0.8 * 25 * 25 / 1000
  ScaleRawToWaterBalanceM3 <- 0.84 * 25 * 25 / 1000   #The 0.84 comes from water balance estimate of mean annual rainfall (1012 mm, cf 1193 mm from the Kerr2018 rainfall surface), and correction for the precipiation mulltiplier (1.6) determined from model calibration
  
  ScaleWaterBalanceM3ToLakeVolume <- 1 / 65531687    #This value is now incorrect and needs to be re-calculated following change in rainfall surface, temperature surface and input data 1/2018.Tim Kerr
  
  #****Remove THE MISSING YEARS*******
  missing_data<-seq(as.Date("1989-01-01"),as.Date("1992-12-31"),by="day")
  SnowWaterEquivalent<-SnowWaterEquivalent[!time(SnowWaterEquivalent) %in% missing_data]
  
  #build a vector of day of year
  DayOfYear   <-  strptime(index(SnowWaterEquivalent),"%Y-%m-%d")$yday+1
  Percentiles <-  aggregate(SnowWaterEquivalent$SWE, DayOfYear, function(x) quantile(x, c(0.05,0.25,0.5,0.75,0.95)) )
  Percentiles<- apply(Percentiles,2, function(x) loess(x ~ as.numeric(index(Percentiles)),span=0.3)$fitted )   #Smooth the percentiles
  Percentiles <- as.data.frame(Percentiles)       #Convert back into a data frame (from the matrix that apply generates on the previous line)
  Percentiles[Percentiles< 0.001]<- 0                   #set the small number to 0 for tidiness
  
  #get the Current year's data
  isLeapYear           <-   (CurrentYear %% 4) == 0
  year.length          <-   365
  if (isLeapYear) {year.length = 366}
  CurrentYearFirstDay  <-   paste0(CurrentYear,"-01-01")
  CurrentYearDates     <-   seq(as.Date(CurrentYearFirstDay),length.out=year.length, by="days")   #build a vector for that year
  CurrentYearSnow      <-   SnowWaterEquivalent[CurrentYearDates,"SWE"]
  CurrentYearSnow      <-   round(CurrentYearSnow, digits=2)                            #round to 2 decimal places
  
  #Get the year before's data
  isLeapYear           <-   (CurrentYear %% 4) == 1
  year.length          <-   365
  if (isLeapYear) {year.length = 366}
  PreviousYearFirstDay  <-   seq(as.Date(CurrentYearFirstDay),length=2,by="-1 years")[2]        
  PreviousYearDates     <-   seq(as.Date(PreviousYearFirstDay),length.out=year.length, by="days")
  PreviousYearSnow      <-   SnowWaterEquivalent[PreviousYearDates,"SWE"]
  PreviousYearSnow      <-   round(PreviousYearSnow, digits=2)
  
  #create a list of the data ready for export
  GraphDataDates      <-    format(index(CurrentYearSnow), "%Y/%m/%d")  #Note that in the complete SWE series, 31st of March is missing on the leap years in an effort to keep each year with 365 days.
  #Unfortunately that causes a few issues with keeping the dates correct.
  
  GraphData          <-    data.frame(Date=GraphDataDates[1:365],"Current Year"=CurrentYearSnow[1:365],
                                      "Previous Year"=PreviousYearSnow[1:365],
                                      Percentiles=Percentiles[1:365,],
                                      stringsAsFactors=FALSE)
  
  colnames(GraphData)[c(4,5,6,7,8)]   <-    c("Percentile 5th","Percentile 25th","median","Percentile 75th","Percentile 95th")
  GraphData[is.na(GraphData)]<- ""
 
  #write the formatted file to output
  write.csv(GraphData,OutputFileName,row.names=FALSE,quote=FALSE)
  
  #PLot functions
  pdf(OutputPlotFileName,paper="a4r",width=10,height=7)
  print.eval=TRUE
  graphdata.reshaped<- melt(GraphData,measure.vars=c("Percentile 5th","Percentile 25th","median","Percentile 75th","Percentile 95th","Previous.Year","Current.Year"))
  #remove missing data at the end
  #get the date prior to the earliest date for which climate data was missing
  LastDate  <- head(as.Date(OpuhaWeather$Date[apply(is.na(OpuhaWeather[2:4]),1,any)], "%y/%m/%d"),1) - 1
  if (length(LastDate) == 0L) { LastDate <- max(as.Date(OpuhaWeather$Date, "%y/%m/%d"))}
  
  GraphData$Current.Year[as.Date(GraphData$Date) > LastDate] <- NA
  TickMarkLocations <- seq(0,2.4,by=0.2)
  
  par(mar=c(5,13,4,1)+0.1)
  graph <- plot(x=as.Date(GraphData$Date),y=GraphData$median,yaxt='n', xlab="",ylab="",type="l",ylim=c(0,2.4),lwd=2)
  axis(2, at=TickMarkLocations,las=1)
  mtext(2,text="Fraction of the median maximum",line=2.5)
  axis(2, at=TickMarkLocations,labels=round(TickMarkLocations * ScaleBackToRaw * ScaleRawToWaterBalanceM3 / 1000000,0),line=3.5,las=1)
  mtext(2,text="Millions of cubic metres",line=6)
  axis(2, at=TickMarkLocations, labels= round(TickMarkLocations * ScaleBackToRaw * ScaleRawToWaterBalanceM3 * ScaleWaterBalanceM3ToLakeVolume,2),las=1,line=7)
  mtext(2,text="Lake Volumes",line=10)
  mtext(2,text="Snow Storage",line=11.5,cex=1.5)
  abline(v=axis.Date(1,x=pretty(as.Date(GraphData$Date))),col="grey",lwd=0.1)
  abline(h=TickMarkLocations,col="grey",lwd=0.1)
  
  lines(x=as.Date(GraphData$Date),y=GraphData$'Percentile 5th',col="grey",lwd=2)
  lines(x=as.Date(GraphData$Date),y=GraphData$'Percentile 25th',col="grey",lty=2,lwd=2)
  lines(x=as.Date(GraphData$Date),y=GraphData$'Percentile 75th',col="grey",lty=2,lwd=2)
  lines(x=as.Date(GraphData$Date),y=GraphData$'Percentile 95th',col="grey",lwd=2)
  lines(x=as.Date(GraphData$Date),y=GraphData$'Current.Year',col="red",lwd=2)
  lines(x=as.Date(GraphData$Date),y=GraphData$'Previous.Year',col="pink",lwd=2)
  legend(xpd=TRUE,"topleft",inset=c(0.01,0.01),
         legend=c("Current Year","Previous Year","95th Percentile","75th Percentile","Median","25th Percentile","5th Percentile"),
         col=c("red","pink","grey","grey","black","grey","grey"),
         lty=c(1,1,1,2,1,2,1),
         lwd=2,
         bty="o",box.col="white",bg="white")


 print(graph)
  
dev.off()
} #end of function


#*****************************************
# This is where it all gets put together
#
#*****************************************
#*******************
#!!!!!!!!!!!!!!!!Edit the following line to match the directory where it all happens!!!!!!!!!!!!
#setwd("C:\\Users\\t.kerr\\Documents\\Projects\\C15114_OWL snow storage estimates\\Processes")
#setwd("M:\\Processes")
ProcessesDirectory<-getwd()
BaseDirectory=normalizePath(dirname(ProcessesDirectory))
#BaseDirectory="\\\\aqualinc-sbs\\data\\ARL Projects\\RD Projects\\RD18004_Lake Opuha Further Work\\"

InputWorkingDirectory=file.path(BaseDirectory,"Input",fsep="\\")
#ClifloFile=file.path(InputWorkingDirectory,"FairlieAWSLatestData.csv",fsep="\\") #No longer used from Jan 2018 when data source switched to Opuha Water Ltd/ECS Ltd data
ECSFile = "\\\\Arlgateway\\ftp\\OpuhaWaterLtd\\ToAqualinc.zip"
PublicFTPSite = "\\\\Arlgateway\\ftp\\opuhawater"
CompleteECSDataFile = file.path(InputWorkingDirectory,"CompleteECSData.RDS")         #This is an R data file
CompleteClimateDataFileName=file.path(InputWorkingDirectory,"OpuhaClimateDataComplete.csv",fsep="\\")
CompleteSnowDataFileName=file.path(InputWorkingDirectory,"OpuhaSnowDataComplete.csv",fsep="\\")
CompleteSnowDataFileNameForFTP<- file.path(PublicFTPSite,"OpuhaSnowDataComplete.csv",fsep="\\")
RecentSnowDataFileName=file.path(InputWorkingDirectory,"OpuhaSnowDataRecent.csv",fsep="\\")
InputFileName = file.path(InputWorkingDirectory,"OpuhaSnowDataComplete.csv",fsep="\\")
CurrentCallendarYear <- as.numeric(format(Sys.Date(),"%Y"))
CurrentHydrologicalYear <- CurrentCallendarYear
if(as.numeric(format(Sys.Date(),"%m")) < 4){CurrentHydrologicalYear - 1}

#DownloadClifloData(BaseDirectory=BaseDirectory)                         #No longer used from Jan 2018 when data source switched to Opuha Water Ltd/ECS Ltd data
#RecentClimateData<-ReformatClifloData(ClifloFile=ClifloFile)            #No longer used from Jan 2018 when data source switched to Opuha Water Ltd/ECS Ltd data
RecentClimateData<-ReformatECSData(ECSFile=ECSFile,CompleteECSDataFile = CompleteECSDataFile)
UpdateClimateData(CompleteDataFileName=CompleteClimateDataFileName,RecentData=RecentClimateData)
#SnowSim_Opuha(InputWorkingDirectory=InputWorkingDirectory,start_year = CurrentHydrologicalYear, end_year = CurrentHydrologicalYear)
SnowSim_OpuhaV5(DataDirectory           = InputWorkingDirectory,
                start_year                      = CurrentHydrologicalYear, 
                end_year                        = CurrentHydrologicalYear,
                temperature_snowrain_threshold 	= 2.5,
                temperature_melt_threshold 	    = 0,
                temperature_melt_factor 	      = 2,
                PrecipMultiplier                = 1,
                MaximumMeltFactor               = 12,
                MinimumMeltFactorDayNumber      = 41,
                MaximumMeltFactorDayNumber      = 328) 

UpdateSnowData(CompleteDataFileName=CompleteSnowDataFileName,RecentDataFileName=RecentSnowDataFileName)

#GraphOpuhaSnow3Axis(InputFileName =InputFileName)
GraphOpuhaSnowM3Y(InputFileName =InputFileName, CurrentYear = CurrentCallendarYear)
#GraphOpuhaSnow(InputFileName =InputFileName)

#Convert snow storage data to millions of cubic metres of water, create a 30 day rolling average, and save it to the myirrigation ftp site
CompleteSnowData <- read.csv(CompleteSnowDataFileName, stringsAsFactors = FALSE)
CompleteSnowData$Millions.Of.Cubic.Metres <- CompleteSnowData$SWE * 0.84 * 25 * 25/1000/1000000
#Calculate 30 day rolling averages
f30 <- rep(1/30, 30)
CompleteSnowData$Thirty.Day.Average.Millions.Of.Cubic.Metres <- stats::filter(CompleteSnowData$Millions.Of.Cubic.Metre, f30, sides=1)
CompleteSnowData[,-1] <- round(CompleteSnowData[,-1],1)
CompleteSnowData <- CompleteSnowData[which(as.Date(CompleteSnowData$Date) < Sys.Date()),]
CompleteSnowData <- CompleteSnowData[order(as.Date(CompleteSnowData$Date),decreasing = TRUE),]
write.csv(CompleteSnowData[,c("Date","Millions.Of.Cubic.Metres","Thirty.Day.Average.Millions.Of.Cubic.Metres")],CompleteSnowDataFileNameForFTP,row.names=FALSE,quote=FALSE)


# clear memory
rm(list=ls())

#edit the line below to input your watershed number
watershed.number <- "12027500"           #** You need to put it in quotes (e.g. "03574500")

# CHEHALIS RIVER NEAR GRAND MOUND, WA

# read in data
MOPEX.data <- read.csv(paste("ftp://hydrology.nws.noaa.gov/pub/gcip/mopex/US_Data/Us_438_Daily/12027500.dly",sep=""), header=FALSE)

# have to do some work to get the data parsed into variables
head(MOPEX.data)
class(MOPEX.data$V1)
# change class of V1 data so can extract parts of it
MOPEX.data$V1 <- as.character(MOPEX.data$V1)

# extract year and convert to numeric format
year <- as.numeric(substr(MOPEX.data$V1,1,4))
class(year)

#convert year to data.frame so can join other variables to it
WS.data <- as.data.frame(year)

WS.data$month <- as.numeric(substr(MOPEX.data$V1,5,6))
WS.data$day <- as.numeric(substr(MOPEX.data$V1,7,8))
WS.data$ppt.mm.d <- as.numeric(substr(MOPEX.data$V1,9,18))
WS.data$PE.mm.d <- as.numeric(substr(MOPEX.data$V1,19,28))
WS.data$Q.mm.d <- as.numeric(substr(MOPEX.data$V1,29,38))
WS.data$Ta.max.degC <- as.numeric(substr(MOPEX.data$V1,39,48))
WS.data$Ta.min.degC <- as.numeric(substr(MOPEX.data$V1,49,58))

# once the data is converted to numeric format, we need to recode the -99.0000 values as NA for R to handle properly
# This is done here using the "ifelse" command
WS.data$ppt.mm.d <- ifelse(WS.data$ppt.mm.d < -90, NA, WS.data$ppt.mm.d)
WS.data$PE.mm.d <- ifelse(WS.data$PE.mm.d < -90, NA, WS.data$PE.mm.d)
WS.data$Q.mm.d <- ifelse(WS.data$Q.mm.d < -90, NA, WS.data$Q.mm.d)
WS.data$Ta.max.degC <- ifelse(WS.data$Ta.max.degC < -90, NA, WS.data$Ta.max.degC)
WS.data$Ta.min.degC <- ifelse(WS.data$Ta.min.degC < -90, NA, WS.data$Ta.min.degC)

# Combine year, month and date into a "date" variable 
WS.data$date <- as.Date(paste(WS.data$year,WS.data$month,WS.data$day, sep = "-"), format = "%Y-%m-%d")
# Create a julian date variable from the date variable (needed later to determine PET for the watershed)
WS.data$jday <- as.numeric(format(WS.data$date, "%j"))

head(WS.data)
summary(WS.data)

# load the packages needed for this activity
library(EcoHydRology)
library(plyr)


# enter site characteristics from Table S1 of [ENVR_420_2015W T1/Materials for final project/Trutch2009,Horton Index,7358_SupportingInfo.pdf]
site.lat.dd <- 46.7761                 # enter value here for latitude in decimal degreess from Table S1 that correspond to your watershed code
site.long.dd <- -123.034               # enter value here for longitude in decimal degreess from Table S1 that correspond to your watershed code
site.area.km2 <- 2318.1              # enter value here for area in square km from Table S1 that correspond to your watershed code
  
Lat.WS.rad <- site.lat.dd * pi/180 # converts your latitude from decimal degrees to radians to use in PET_fromTemp

#make some summary plots
plot(WS.data$date,WS.data$Q.mm.d)

#gap-fill any missing discharge data with mean discharge for the period of record - the baseflow separation cannot work with missing data
WS.data$Q.gf.mm.d <- ifelse(!is.na(WS.data$Q.mm.d),WS.data$Q.mm.d,mean(na.omit(WS.data$Q.mm.d)))
#compute baseflow (BF) using gap-filled Q data
WS.data$BF.mm.d <- BaseflowSeparation(WS.data$Q.gf.mm.d, filter_parameter = 0.925, passes = 3)[,1] 
# only keep BF values when Q data exists
WS.data$BF.mm.d <- ifelse(!is.na(WS.data$Q.mm.d),WS.data$BF.mm.d,NA)
#compute quickflow (QF) using gap-filled Q data
WS.data$QF.mm.d <- BaseflowSeparation(WS.data$Q.gf.mm.d, filter_parameter = 0.925, passes = 3)[,2]
# only keep QF values when Q data exists
WS.data$QF.mm.d <- ifelse(!is.na(WS.data$Q.mm.d),WS.data$QF.mm.d,NA)

summary(WS.data)

#set up a data.frame to plot the hydrolgraph and rainfall using funtion "hydrograph" in EcoHydRology
WS.data.hyd <- WS.data[,c(9,4,6,12)]
hydrograph(WS.data.hyd, P.units = "mm/d", S.units = "mm/d")
  
# Calculate annual means for the following:

Annual.mean <- ddply(WS.data, c("year"), summarise,
                  N    = length(Q.mm.d),
                  Q = mean(na.omit(Q.mm.d)),
                  B = mean(na.omit(BF.mm.d)),
                  S = mean(na.omit(QF.mm.d)),
                  P = mean(na.omit(ppt.mm.d)),
                  PE = mean(na.omit(PE.mm.d))   )
                  

# (1)	Streamflow (Q);
# (2)	Baseflow (B) (e.g. as BF above);
# (3)	Direct runoff (S) (e.g. quickflow, as QF above); 
# (4)	Precipitation (P);
# (5)	Climatic potential evaporation (PE);

# (6)	Potential Evapotranspiration (PET);
AlbedoCR <- 0.09
AspectCR <- 0
SlopeCR <- 0

WS.data["PET.mm.d"] <- PET_fromTemp(WS.data$jday, WS.data$Ta.max.degC, WS.data$Ta.min.degC, Lat.WS.rad, (WS.data$Ta.max.degC + WS.data$Ta.min.degC) / 2 , AlbedoCR, 0.97, AspectCR, SlopeCR, 0, 1.26 )

# Not sure if it's working at the moment


# (7) Catchment Wetting (W) from Brooks et al., 2011 (Eqn 1);

WS.data["W.mm.d"] <- WS.data$ppt.mm.d - WS.data$QF.mm.d


# (8) Evapotranspiration (ET) from Brooks et al., 2011 (Eqn 2);

WS.data["ET.mm.d"] <- WS.data$ppt.mm.d - WS.data$Q.mm.d

# (9)	Horton Index (HI) from Brooks et al., 2011 (Eqn 3)

WS.data["HI.mm.d"] <- WS.data$ET.mm.d / WS.data$W.mm.d


Annual.mean <- ddply(WS.data, c("year"), summarise,
                     N    = length(Q.mm.d),
                     Q = mean(na.omit(Q.mm.d)),
                     B = mean(na.omit(BF.mm.d)),
                     S = mean(na.omit(QF.mm.d)),
                     P = mean(na.omit(ppt.mm.d)),
                     PE = mean(na.omit(PE.mm.d)),   
                     PET = mean(na.omit(PET.mm.d)),
                     W = mean(na.omit(W.mm.d)),
                     ET = mean(na.omit(ET.mm.d)),
                     HI = mean(na.omit(HI.mm.d)) )


# How do you take an annual mean?? Was this correct??



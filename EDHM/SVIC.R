#### For the first time, please install the following package published in GitHub. ####
# install.packages("devtools")
# library(devtools)
# install_github("MomentVon/HMtools")
# install_github("MomentVon/EDHM")
#### If prompted to install other dependent packages, install them together. ####
library(HMtools)
library(EDHM)



SVIC <- setClass("SVIC", contains = "hm.list")
setMethod("ReferenceET", "SVIC", ReferenceET.PenmanMonteith)
setMethod("ET", "SVIC", ET.VIC)
# setMethod("INTERCEPTION", "SVIC", INTERCEPTION.SNOWVIC)
setMethod("InfiltratRat", "SVIC", InfiltratRat.GreenAmpt)
setMethod("RUNOFF", "SVIC", RUNOFF.VIC)
setMethod("BASEFLOW", "SVIC", BASEFLOW.ARNO)
setMethod("GROUNDWATER", "SVIC", GROUNDWATER.VIC)
setMethod("ROUTE", "SVIC", ROUTE.IUHG2RES)
setMethod("SNOW", "SVIC", SNOW.VIC)
## Param ####
ParamterModell = c(5, 50, 100, 150, 0.1, 1.3, 0.1,9, 0.1, 0.1, 0.1, 3,0.5, 0.5, 0.5 , 0.5,0.5,0.5,0.5)
Param <- list(GridN = 311,
              PeriodN = 6,
              TimeStepSec = 360*24,
              UHPeriodN = 180,
              UHUnitTranslate = 35,
              CanopyCapacity = ParamterModell[1],
              ZoneDepth1 = ParamterModell[2],
              ZoneDepth2 = ParamterModell[3],
              ZoneDepth3 = ParamterModell[4],
              ### 1.1 interception ##
              CoefficientFreeThroughfall = ParamterModell[5],
              ### 1.2 base flow ##
              ExponentARNOBase = ParamterModell[6],
              ARNOBaseThresholdRadio = ParamterModell[7],
              DrainageLossMax = ParamterModell[8],##*## #mm
              DrainageLossMin = ParamterModell[9], ##*## #mm
              ### 1.3 Runoff ##
              SoilMoistureCapacityB = ParamterModell[10], ##*## ##b is the soil moisture capacity shape paeter, which is a measure of the spatial variability of the soil moisture capacity
              InfiltrationRateB = ParamterModell[11],  ##*## ##b is the soil infiltrtionrate shape paeter,
              ### 1.4 ground water ##
              ClappHornbergerB = ParamterModell[12],
              UPPaList = ParamterModell[13:19])
Param <- mergeData(Param, ParamAll)
## In Data ####

MetData <- MetroList
MetData$TAir <- MetData$Tmean
GeoData <- list()
GeoData$Elevation <- array(GeoList$Evalution)
GeoData$Latitude <- array(GeoList$Location$Latitude)
# GeoData <- as.data.frame(GeoData)
GridData <- GridList
UPMethondList <- list("Shipeng", "Shipeng", "Shipeng", "Shipeng")
GridN = dim(GridData$TypeGridID$GridGridID)[1]
infoStarDay = "1999-1-1"
infoEndDay = "1999-1-6"
DateDay = seq(as.Date(infoStarDay),as.Date(infoEndDay),1)
PeriodN = length(DateDay)
NDay = toNDayofYear(DateDay)
MON = toMON(DateDay)

TimeData <- list(NDay = array(NDay))

LandP1Mat <- matrix(0.0,PeriodN, GridN)
LandP2Mat <- matrix(0.0,PeriodN, GridN)
ROU = paste("ROU",MON, sep = ".")
DIS = paste("DIS", MON, sep = ".")
for (i in 1:PeriodN) {
  LandP1Mat[i,] = GeoList$LanduseParam[[DIS[i]]]
  LandP2Mat[i,] = GeoList$LanduseParam[[ROU[i]]]
}
AerodynamicResistance <- fctAerodynamicResistance(LandP1Mat,
                                                  LandP2Mat,
                                                  MetData$WindSpeed)
Prec <- list(Precipitation = as.array(MetData$PrecipitationHoch))
SoilData <- list(WettingFrontSuction = array(GeoList$SoilParam$T_WettingFrontSoilSuctionHead_mm),
                 Conductivity = array(GeoList$SoilParam$T_SaturatedHydraulicConductivity_mm_day),
                 Porosity = array(GeoList$SoilParam$T_Porosity_),
                 FieldCapacity = array(GeoList$SoilParam$T_FieldCapacity_),
                 WiltingPoint = array(GeoList$SoilParam$T_WiltingPoint_),
                 SaturatedSoilSuctionHead = array(GeoList$SoilParam$T_SaturatedSoilSuctionHead_mm),
                 SaturatedConductivity = array(GeoList$SoilParam$T_SaturatedHydraulicConductivity_mm_day))
Canopy <- list(StorageCapacity = Param$CanopyCapacity)
Ground <- list(ZoneDepth = matrix(rep(c(Param$ZoneDepth1, Param$ZoneDepth2, Param$ZoneDepth3), GridN), 3, GridN, byrow = T),
               MoistureCapacityMax = (Param$ZoneDepth1 + Param$ZoneDepth2) * SoilData$Porosity)
TimeVariData <- list()
StoreData <- list()
TimeInvariData <- list()
ModelData <- list()
class(ModelData) = class(StoreData) = class(TimeVariData) = class(TimeInvariData) <- c("SVIC", "hm.list", "list")
StoreData$Ground$Runoff <- array(0, c(PeriodN, GridN))
StoreData$Ground$BaseFlow <- array(0, c(PeriodN, GridN))
TimeVariData$TimeData = TimeData
TimeVariData$MetData = MetData
TimeVariData$Prec = Prec
TimeVariData$Aerodyna$AerodynaResist = AerodynamicResistance

TimeInvariData$Aerodyna = list(ArchitecturalResist = GeoList$LanduseParam$rarc,
                               StomatalResist = GeoList$LanduseParam$rmin)
TimeInvariData$GeoData = GeoData
TimeInvariData$SoilData = SoilData
TimeInvariData$Canopy <- list(StorageCapacity = array(rep(Param$CanopyCapacity, GridN)))
ZoneDepth <- array(matrix(rep(c(Param$ZoneDepth1, Param$ZoneDepth2, Param$ZoneDepth3), GridN),GridN, 3, byrow = T), dim = c(GridN,3))
ZoneMoistureCapacityMax = array(c(ZoneDepth[1] * SoilData$Porosity, ZoneDepth[2] * SoilData$Porosity, ZoneDepth[3] * SoilData$Porosity), dim = c(GridN,3))
TimeInvariData$Ground <- list(ZoneDepth = ZoneDepth, ZoneMoistureCapacityMax = ZoneMoistureCapacityMax)
ModelData <- mergeData(TimeVariData, TimeInvariData)
## RET ####
RETOut <- ReferenceET(ModelData, Param)
TimeVariData <- mergeData(TimeVariData, RETOut)

## PRECDivid ####

PDOut <- PRECDivid(ModelData, Param)
TimeVariData <- mergeData(TimeVariData, PDOut)

SNVOut <- SNOW.VIC(runMode = "VIEW", viewGN = 311)

## model Data ####
PeriodData <- list()
Ground <- list()
Ground$ZoneMoistureVolume <- array(0.0, c(GridN, 3))
Ground$MoistureVolume = array(Ground$ZoneMoistureVolume[,1])
Intercept <- list(Interception = array(0.0, c(GridN)))
PeriodData$Ground <- Ground
PeriodData$Intercept <- Intercept


PeriodData <- mergeData(PeriodData, TimeInvariData)
PeriodData <- mergeData(SNVOut$Arguments$InData, PeriodData)
Param <- mergeData(SNVOut$Arguments$Param, Param)
class(PeriodData) <- c("SVIC", "hm.list", "list")

for (i in 2:PeriodN) {
  PeriodData <- mergeData(PeriodData, TimeVariData[i])
  ## ET ####
  PeriodData$Ground$MoistureCapacityMax <- PeriodData$Ground$ZoneMoistureCapacityMax[,1]
  ETOut <- ET(PeriodData, Param)
  PeriodData <- mergeData(PeriodData, ETOut)
  ## Intercept ####
  SNOut <- SNOW.VIC(PeriodData, Param)
  PeriodData <- mergeData(PeriodData, SNOut)
  PeriodData$Prec$Precipitation <- PeriodData$Snow$Melt
  ## base flow ####
  PeriodData$Ground$MoistureVolume <- PeriodData$Ground$ZoneMoistureVolume[,3]
  PeriodData$Ground$MoistureCapacityMax <- PeriodData$Ground$ZoneMoistureCapacityMax[,3]
  BFOut = BASEFLOW(PeriodData, Param)
  PeriodData <- mergeData(PeriodData, BFOut)
  ## runoff ####
  PeriodData$Ground <- mergeList(PeriodData$Ground, SoilData)
  PeriodData$Ground$Depth <- PeriodData$Ground$ZoneDepth[,1]
  IFROut = InfiltratRat(PeriodData)
  PeriodData <- mergeData(PeriodData, IFROut)
  
  PeriodData$Ground$MoistureCapacityMax <- (Param$SoilMoistureCapacityB + 1) *
    (PeriodData$Ground$ZoneDepth[,1] + PeriodData$Ground$ZoneDepth[,2]) * PeriodData$Ground$Porosity
  PeriodData$Ground$MoistureVolume <- PeriodData$Ground$ZoneMoistureVolume[,1] + PeriodData$Ground$ZoneMoistureVolume[,2]
  
  RFOut <- RUNOFF(PeriodData, Param)
  PeriodData <- mergeData(PeriodData, RFOut)
  ## ground water ####
  GWOut <- GROUNDWATER(PeriodData, Param)
  PeriodData <- mergeData(PeriodData, GWOut)
  ## for end ##
  StoreData[i] <- PeriodData
}

## route ####
UPMethondList <- list("Shipeng", "Shipeng", "Shipeng", "Shipeng")
G2AimGAll <- fctG2AimGAll(GridData$TypeGridID,
                          GridData$GridID,
                          GridData$FlowDirection,
                          GridData$GridDEM)
TransAll <- fctGTransMatAll(G2AimGAll, GridData$TypeGridID)
UHParam <- list(list(StreamLength = G2AimGAll[[1]][,3],
                     RiverheadNumber = ParamterModell[13],
                     WaveVelocity = ParamterModell[16]),
                list(StreamLength = G2AimGAll[[1]][,3],
                     RiverheadNumber = ParamterModell[14],
                     WaveVelocity = ParamterModell[17]),
                list(StreamLength = G2AimGAll[[2]][,3],
                     RiverheadNumber = ParamterModell[15],
                     WaveVelocity = ParamterModell[18]),
                list(StreamLength = G2AimGAll[[3]][,3],
                     RiverheadNumber = ParamterModell[15],
                     WaveVelocity = ParamterModell[19]))

ModelData$IUH <- list(UHMethond = UPMethondList, UHParam = UHParam)
ModelData$Confluence <- list(WaterSource = StoreData$Ground, 
                             TypeGridID = GridData$TypeGridID, TransAll = TransAll)
MUOut <- makeUHALL(ModelData, Param)
ModelData$IUH <- mergeData(ModelData$IUH, MUOut)

RTOut <- ROUTE(ModelData, Param)

#### For the first time, please install the following package published in GitHub. ####
# install.packages("devtools")
# library(devtools)
# install_github("MomentVon/HMtools")
# install_github("MomentVon/OptimizationPrg")
# install_github("MomentVon/VectorTools")
# install_github("MomentVon/EDHM")
#### If prompted to install other dependent packages, install them together. ####

#### Load necessary packages ####
library(HMtools)
library(plyr)  ## join
library(EDHM)
library(mcga)

#### Set file path ####
wdOtherData = "./OtherData"
filePathGridID = "ct_id5.txt"
filePathLocation = "ct_location.txt"
filePathFlowDirection = "ct_direction5.txt"
filePathRiverGridID = "ct_river_id.txt"
filePathEstuaryID = "ct_estuary_id.txt"
filePathHydroStationID = "ct_hydrostation_id.txt"
filePathDEM = "ct_dem025.txt"
filePathLanduse = "ct_landuse025.txt"
filePathLanduseLib = "landuse_lib_qinguha1ji.txt"
filePathSoil = "ct_soil025.txt"
filePathSoilInterpolation = "soil_interpolation.txt"
filePathMU2Class = "MU2Class.txt"
filePathSoilLib = "soil_lib.txt"
filePathObserve = "cuntan1979-2012.txt"




setwd(wdOtherData)
#### read Grid Info include river, estuary, hydrostation infomation ####
GridID <- read.grid(filePathGridID)
infoGridRowN = attr(GridID, "nrows")  #the rows number of FLOWDRIC
infoGridColN = attr(GridID, "ncols")   #the clows number of FLOWDRIC
GridAllIDVector = array(as.matrix(GridID),c(infoGridRowN * infoGridColN))
infoGridN <- length(GridID[!is.na(GridID)])
GridGridID = as.data.frame(GridID[!is.na(GridID)])
GridLocation = read.table(filePathLocation, header = T)
FlowDirection <- read.grid(filePathFlowDirection)
RiverGridID = read.table(filePathRiverGridID) #one demesion
EstuaryID = read.table(filePathEstuaryID) #one demesion
HydroStationID = read.table(filePathHydroStationID) #one wert

#### read DEM ####
GridDEM = fctClassify(read.grid(filePathDEM), GridID, 20, 0, 5) ##m
GridEvalution = tapply(GridDEM[,2] * GridDEM[,3], GridDEM[,1], sum)  ##m
id = as.data.frame(GridAllIDVector)
names(id) = "id"
GridDEM_m_Matrix = join(id,as.data.frame(cbind(id = seq(1,infoGridN,1),GridEvalution)))
GridDEM4UH = array(GridDEM_m_Matrix[,2], dim = c(infoGridRowN,infoGridColN))


#### read LanduseData ####
GridLanduse = fctClassify(read.grid(filePathLanduse), GridID, 20, 0, 5)
names(GridLanduse) = c("GridID", "Code", "Rate")
LanduseLib = read.table(filePathLanduseLib, header = T, row.names = 1)
GridLanduseParam = gridParamJion(GridLanduse, infoGridN, LanduseLib)


#### read SoilData ####
GridSoil = fctClassify(read.grid(filePathSoil), GridID, 20, 0, 5)
MU2Class = read.table(filePathMU2Class, header = T)
SoilLib = read.table(filePathSoilLib, header = T, row.names = 1)
infoSoilParamN = dim(SoilLib)[2]
MU2Class[which(MU2Class[,3] == 0),3] = MU2Class[which(MU2Class[,3] == 0),2]
names(GridSoil) = c("GridID", "MU_GLOBAL", "Rate")
GridSoilClass = join(GridSoil, MU2Class)
GridTopSoil = GridSoilClass[,c(1,4,3)]
names(GridTopSoil) = c("GridID", "Code", "Rate")
TopSoilParam = gridParamJion(GridTopSoil, infoGridN, SoilLib)
GridSubSoil = GridSoilClass[,c(1,5,3)]
names(GridSubSoil) = c("GridID", "Code", "Rate")
SubSoilParam = gridParamJion(GridSubSoil, infoGridN, SoilLib)
colnames(TopSoilParam) = paste("T_",names(SoilLib),sep = "")
colnames(SubSoilParam) = paste("S_",names(SoilLib),sep = "")
GridSoilParam = as.data.frame(cbind(TopSoilParam, SubSoilParam[,2:infoSoilParamN]))

#### read observe Q in cuntan sattion ####
ObserveQ = read.table(filePathObserve)
ObserveQ89_93 = ObserveQ[3654 : (3653 + 1826),1]

#### The reading of weather data is not shown here, because the weather data is too large to upload, 
#### but there is an example of IDW interpolation in the HMTools package. 
#### The MetroList data used here is the data provided by EDHMvic.
#### make LIst for VIC ####
GeoL <- list(Evalution = GridEvalution,
                Location = GridLocation,
                SoilParam = GridSoilParam,
                LanduseParam = GridLanduseParam)
TypeGridID <- list(GridGridID = GridGridID,
                   RiverGridID = RiverGridID,
                   EstuaryID = EstuaryID,
                   HydroStationID = HydroStationID)
GridL <- list(TypeGridID = TypeGridID,
                 GridID = GridID,
                 FlowDirection = FlowDirection,
                 GridDEM = GridDEM4UH)

ClsNa <- c("VIC", "PenmanMonteith", "GreenAmpt", "Gash", "ARNO", "G2RES")
class(ClsNa) <- ClsNa
UPMethondList <- list("Shipeng", "Shipeng", "Shipeng", "Shipeng")
VICInList <- InListMake(ClsNa,
                        "1989-1-1",
                        "1993-12-31",
                        
                        MetroList,
                        GeoL,
                        GridL,
                        UPMethondList,
                        180,
                        35)
ParamterMax = c(15, 300, 600, 900, 0.9, 2.7, 0.7, 30, 9, 1,1,15,7,7,7,10,10,10,15)
ParamterMin = c(5, 50, 100, 150, 0.1, 1.3, 0.1,9, 0.1, 0.1, 0.1, 3,0.5, 0.5, 0.5 , 0.5,0.5,0.5,0.5)
ParamterModell = 0.5 * (ParamterMax + ParamterMin)
class(ParamterModell) <- ClsNa
VICPaList <- PaListMake(ParamterModell)
#### run VIC ####
system.time(Q <- MODELL(VICInList, VICPaList))

ParamterCalibrateMax = c(15, 300, 600, 900, 0.9, 2.7, 0.7, 30, 9, 1,1,15,7,7,7,10,10,10,15)
ParamterCalibrateMin = c(5, 50, 100, 150, 0.1, 1.3, 0.1,9, 0.1, 0.1, 0.1, 3,0.5, 0.5, 0.5 , 0.5,0.5,0.5,0.5)
#### calibrate parameter set with GA ####
FitGA = mcga2(fitness  = CALIBRATE, 
              InList = VICInList, 
              Observe = ObserveQ89_93, 
              min = ParamterCalibrateMin, 
              max = ParamterCalibrateMax, 
              popSize =20, 
              maxiter =20)
#### ga with parallel ####
# FitGA = mcga2(fitness  = CALIBRATE, 
#               InList = VICInList, 
#               Observe = ObserveQ89_93, 
#               min = ParamterCalibrateMin, 
#               max = ParamterCalibrateMax, 
#               popSize =20, 
#               maxiter =20,
#               parallel = T)

#### creat and run a model ########
######## ET ##########
ModelMoistureVolume = array(1.0, c(infoPeriodN, infoGridN, infoZoneN + 2))
ModelMoistureVolume[1,,1:2] = 0.2 * as.matrix(ZoneDepth[,1:2])

testn = 2
str(RET)
dim(ModelMoistureVolume)
ModelMoistureVolume[1,,1:2]
### evaptranspiration ############
RET = ReferenceET_PenmanMonteith(NDay[testn], GridEvalution / 1000.0, GridLocation[,2], GridMetroData[testn,,3], GridMetroData[testn,,4], GridMetroData[testn,,2], GridMetroData[testn,,5], infoWindH, GridMetroData[testn,,6], GridMetroData[testn,,7])

AerodynamicResistance = fctAerodynamicResistance(GridLanduseParam[,29 + NMonth[testn]], GridLanduseParam[,17 + NMonth[testn]], GridMetroData[testn,,5])

ET = ET_VIC(RET, GridMetroData[testn,,8], ModelMoistureVolume[(testn-1),,1], ZoneDepth$Depth0, ModelMoistureVolume[(testn-1),,2], ZoneDepth$Depth1 * GridSoilParam$T_Porosity_, AerodynamicResistance, GridLanduseParam$rarc, GridLanduseParam$rmin)

### interception ###############
Interception = INTERCEPTION_Gash(ModelMoistureVolume[(testn-1),,1], ZoneDepth$Depth0, GridMetroData[testn,,8], ET$EvapC)

### baseflow #########
Baseflow = BASEFLOW_ARNO (ModelMoistureVolume[(testn-1),,5], ZoneDepth$Depth4 * GridSoilParam$S_Porosity_)

### runoff ############

InfiltrationRateMax = fctInfiltrationGreenAmpt(GridSoilParam$T_SaturatedHydraulicConductivity_mm_day, GridSoilParam$T_WettingFrontSoilSuctionHead_mm, ModelMoistureVolume[(testn-1),,2] / ZoneDepth$Depth1, GridSoilParam$T_Porosity_, ModelMoistureVolume[(testn-1),,2])

Runoff = RUNOFF_VIC(GridMetroData[testn,,8] - Interception, (ZoneDepth$Depth1 + ZoneDepth$Depth23) * GridSoilParam$T_Porosity_, ModelMoistureVolume[(testn-1),,2] + ModelMoistureVolume[(testn-1),,3], InfiltrationRateMax)
Runoff$Runoff
Runoff$Infiltration

###########################

### ground water replan / interflow ##########
SoilParam4GroundWaterTem = GridSoilParam[,c(2,3,4,6,11)]
names(SoilParam4GroundWaterTem) = c("WiltingPoint", "FieldCapacity", "Porosity", "SaturatedHydraulicConductivity", "Porosity_Sub")
SoilParam4GroundWater = cbind(ZoneDepth, SoilParam4GroundWaterTem)
GroundWaterIn = as.data.frame(as.matrix(ModelMoistureVolume[testn - 1,,]))
colnames(GroundWaterIn) = c("Volum0", "Volum1", "Volum2", "Volum3", "Volum4", "Volum5", "Depth2", "Depth3")

Groundwater = GROUNDWATER_LK5Z(ET, Interception, Runoff$Infiltration, Baseflow, GroundWaterIn, SoilParam4GroundWater, SoilMatricPotential, GridSoilClass[,c(1,3,4)])
Groundwater = as.data.frame(as.matrix(Groundwater))

############ route ###############

RoutSurFlow = Groundwater$Volum5 + Runoff$Runoff
RoutBasFlow = Baseflow
testGrid2River = fctGrid2AimGrid (GridGridID, RiverGridID, GridDEM. =  GridDEM4UH)
testRiver2Estuary = fctGrid2AimGrid(RiverGridID, EstuaryID, GridDEM. =  GridDEM4UH)
testEstuary2Station = fctGrid2AimGrid(EstuaryID, HydroStationID, GridDEM. =  GridDEM4UH)

############
testUHGrid2RiverScf = fctMakeUH_Shipeng(testGrid2River)
testUHGrid2RiverBas = fctMakeUH_Shipeng(testGrid2River)
testUHRiver2Estuary = fctMakeUH_Shipeng(testRiver2Estuary)
testUHEstuary2Station = fctMakeUH_Shipeng(testEstuary2Station)
################
TranslateMatrixGrid2River = fctMakeGridTranslateMatrix(testGrid2River, RiverGridID)
TranslateMatrixRiver2Estuary = fctMakeGridTranslateMatrix(testRiver2Estuary, EstuaryID)
TranslateMatrixEstuary2Station = fctMakeGridTranslateMatrix(testEstuary2Station, HydroStationID)
##############

testScfFlow = rep(as.matrix(Runoff$Runoff),infoGridN)
testScfFlow = array(testScfFlow,c(infoGridN, infoPeriodN))
testScfFlow = t(testScfFlow)
dim(testScfFlow)
testScfFlow[1:2,]
SfcFlowAllGrid = testScfFlow
BasFlowAllGrid = testScfFlow

CutGridSfc = fctCutGridFlow(SfcFlowAllGrid, GridGridID, RiverGridID)
ScfRiverFlow = fctUHConfluence(CutGridSfc$FlowOtherGrid, testUHGrid2RiverScf) %*% TranslateMatrixGrid2River + CutGridSfc$FlowAimGrid

CutGridBas = fctCutGridFlow(BasFlowAllGrid, GridGridID, RiverGridID)
BasRiverFlow = fctUHConfluence(CutGridBas$FlowOtherGrid, testUHGrid2RiverBas) %*% TranslateMatrixGrid2River + CutGridBas$FlowAimGrid

RiverFlow = ScfRiverFlow + BasRiverFlow

CutRiver = fctCutGridFlow(RiverFlow, RiverGridID, EstuaryID)
EstuaryFlow = fctUHConfluence(CutRiver$FlowOtherGrid, testUHRiver2Estuary) %*% TranslateMatrixRiver2Estuary + CutRiver$FlowAimGrid

CutEstuary = fctCutGridFlow(EstuaryFlow, EstuaryID, HydroStationID)
StationFlow = fctUHConfluence(CutEstuary$FlowOtherGrid, testUHEstuary2Station) %*% TranslateMatrixEstuary2Station + CutEstuary$FlowAimGrid

StationFlowQ = StationFlow * UHUnitTranslate


#########
testUHShipeng = fctMakeUH_Shipeng(testGrid2River, UHPeriodN. = UHPeriodN)



###########################












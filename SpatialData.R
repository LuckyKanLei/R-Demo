## R for hydrology. ####
## https://style.tidyverse.org/
## https://adv-r.hadley.nz/
## https://ggplot2-book.org/
## https://rspatial.org/
## 1 Data import ####
## 1.0 Set the filepath ##
nowWD <- getwd() # get the directory of the R process
## 1.0 set directory
setwd("F:\\G2\\R-program\\R-Demo\\KD-SVIC")

## 1.1 txt file import ##
Data1 <- read.table("TempMaxMinRate.txt")
setwd(nowWD)
Data2 <- read.table("F:\\G2\\R-program\\R-Demo\\KD-SVIC\\TempMaxMinRate.txt") # absolute filepath

## 1.2 nc file import ##
library(ncdf4)
metWd <- "F:\\G2\\DataSet\\MET\\CMFD\\day"
setwd(metWd)

nc <- nc_open("lrad_ITPCAS-CMFD_V0106_B-01_01dy_010deg_200501-200512.nc") # open
NcData <- ncvar_get(nc, # read Data
                    start = c(30, 50, 1), # start point in every dimensions
                    count = c(2, 7, 11)) # number for every dimensions
nc_close(nc) # close file

setwd(nowWD) ##*##

## 1.3 spatial data ##
library(rgdal)
library(raster)
BasinPolyFP <- "F:\\G2\\kkBasinProject\\Kaikong\\kdB.kml" # polygon(vector data) from google earth
RiverFP <- "F:\\G2\\kkBasinProject\\Kaikong\\kkRiver.shp" # lines(vector data) from some GIS applications, e.g. ArcGIS

DEMFP <- "F:\\G2\\DataSet\\GEO\\GEO-RS-CN-DEM\\dem_1km\\hdr.adf" # raster data
SoilRasterFP <- "F:\\G2\\DataSet\\GEO\\SOL-RS-GL-HWSD-V1\\hwsd.bil" # raster data
LandUse1FP <- "F:\\G2\\DataSet\\GEO\\LCV-RS-GL-GLCTHU-2010\\globalLC_250m_mod_2010\\h24v04.tif" # tif file
LandUse2FP <- "F:\\G2\\DataSet\\GEO\\LCV-RS-GL-GLCTHU-2010\\globalLC_250m_mod_2010\\h24v05.tif"

##*##*## set the project ##*##*##
newcrs <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

## vector data
kdB <- readOGR(BasinPolyFP) # read vector data
kdB ## check the Project
kdB <- spTransform(kdB, newcrs) # translate the Project to the united Project
kkR <- readOGR(RiverFP)
kkR <- spTransform(kkR, newcrs)

## raster data
chinaDem1km <- raster(DEMFP)
HWSDwold <- raster(SoilRasterFP)
crs(HWSDwold) <- newcrs # translate the Project to the united Project

## 2 spatial data processing/ manipulation ####
## https://rspatial.org/

## 2.1 vector cropping
kdR <- intersect(kkR, kdB) ## Cropping to the kdBasin

## 2.2 vector data resampling, change the scale
LandUse1 <- raster(LandUse1FP)
LandUse2 <- raster(LandUse2FP)
LandUse <- merge(LandUse1, LandUse2) # merge data
LandUse <- projectRaster(LandUse, crs = newcrs, method = "ngb") ##*## category data must use "ngb" method

## 2.3 raster cropping
kdHWSD <- mask(kdHWSD1, kdB)

## 2.4 raster resampling
kdHWSDRaster <- resample(kdHWSD, kdBRaster)

## 3 plot spatial data ####
## Plot the simulation-direction ##
kdDirPlot <-
  ggplot() +
  geom_raster.seq(kdDEM) +
  scale_fill_gradientn("DEM/m", colours = terrain.colors(6000)) +
  geom_basin(kdB) +
  geom_river(kdR) +
  geom_text(data = raster.qual(kdIDGrid), aes(x = x, y = y, label = fill), color = "white", size = 3) +
  geom_dir(kdDirBGrid) +
  geom_vline(xintercept = seq(82.65, 86.25, 0.1), color = "#a1d99b") +
  geom_hline(yintercept = seq(41.95, 43.55, 0.1), color = "#a1d99b") +
  coord_equal() +
  theme_bw() +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Kaidu Basin", "Orignal River and simulated River(grid Directions)")
ggsave("kdDir1.png", kdDirPlot) # save the file









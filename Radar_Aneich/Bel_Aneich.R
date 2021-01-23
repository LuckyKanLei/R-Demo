library(raster)
library(sp)
library(purrr)
library(HMtools)
#if(!require(devtools)) install.packages("devtools")
# devtools::install_github("LuckyKanLei/HMtools")
library(geosphere)
library(reshape2)
## Data eingeben ####
load("Radar_Drs_4km.RData")
Rd_Data <- rast
n_BS <- 19
BS_File <- "produkt_rr_stunde_2019010912.txt"
BS_Data_ <- read.table(BS_File, header = T, sep = ";")
BS_Data <- BS_Data_[,c("STATIONS_ID", "Geogr.Laenge", "Geogr.Breite", "R1", "Stationsname")]
names(BS_Data) <- c("ID", "Lon", "Lat", "Value", "Name")
## 1.Radar Pixel aus 9-Pixeln ausählen ####
## suchen die Pixeln, die auf Boden– oder Festzielechos beruhen. ##
## check die bodenstation Position ##
plot(Rd_Data)
points(BS_Data$Lon, BS_Data$Lat)
## löschen dei Stationen, dei nicht in der Forschunggebiet steht. ##
BS_row <- rowFromY(Rd_Data, BS_Data$Lat)
BS_col <- colFromX(Rd_Data, BS_Data$Lon)
BS_Data[(which(is.na(Rd_Data[cbind(BS_row, BS_col)]))),] <- NA
BS_Data <- na.omit(BS_Data)

## löschen die Bodenstationen, die Daten kleiner als 0.1 mm für Faktormethode. ##
BS_Data_A <- delete_BS_A(Rd_Data, BS_Data, pixeln_Weit = 2)


## Aneichungfaktoren in BS ####
## stellen die Schwellewerte ##
Rd_Data_NA <- replace(Rd_Data, Rd_Data < 0.0002, NA)
Rd_Data_0 <- replace(Rd_Data, Rd_Data < 0.0002, 0)

AF_D_BS <- faktor_BS(Rd_Data_0, BS_Data, methond = "D")
AF_A_BS <- faktor_BS(Rd_Data_NA, BS_Data_A, pixeln_Weit = 2)
## Aneichungfaktoren interpolieren ####
## Suchradius ##
Dis_BS_BS <- distm(BS_Data[,2:3], BS_Data[,2:3])
Dis_BS_BS <- replace(Dis_BS_BS, Dis_BS_BS == 0, NA)
Dis_BS_BS_Min <- as.numeric(apply(Dis_BS_BS, 1, function(a) min(a, na.rm = T)))
Dis_BS_BS_min_max <- max(Dis_BS_BS_Min)

Gewicht <- radar_Gewicht(BS_Data[,2:3], Rd_Data, Dis_BS_BS_min_max)
AF_D_Rd <- interpolat_AF(Gewicht, AF_D_BS)
AF_D_raster <- Rd_Data
AF_D_raster@data@values <- AF_D_Rd
plot(AF_D_raster)
title("Faktoren D")
points(BS_Data$Lon, BS_Data$Lat)
min(AF_D_raster@data@values, na.rm = T)
max(AF_D_raster@data@values, na.rm = T)
Gewicht_A <- radar_Gewicht(BS_Data_A[,2:3], Rd_Data, Dis_BS_BS_min_max)
AF_A_Rd <- interpolat_AF(Gewicht_A, AF_A_BS)
AF_A_raster <- Rd_Data
AF_A_raster@data@values <- AF_A_Rd
min(AF_A_raster@data@values, na.rm = T)
max(AF_A_raster@data@values, na.rm = T)

plot(AF_A_raster)
title("Faktoren A")
points(BS_Data_A$Lon, BS_Data_A$Lat)
## Aneichung ####
## Faktorenverfahren ##
An_A <- Rd_Data * AF_A_raster
## Differenzenverfahren ##
An_D <- Rd_Data + AF_D_raster
## korriegieren negativ ##
An_D@data@values <- replace(An_D@data@values, An_D@data@values < 0, 0)

## kritieren ####
Sm_Krige <- interpolat_Kriging(BS_Data, Rd_Data, 4)
## korriegieren negativ ##
Sm_Krige@data@values <- replace(Sm_Krige@data@values, Sm_Krige@data@values < 0, 0)

min(Sm_Krige@data@values, na.rm = T)
max(Sm_Krige@data@values, na.rm = T)

BS_row <- rowFromY(Rd_Data, BS_Data$Lat)
BS_col <- colFromX(Rd_Data, BS_Data$Lon)
Ra_Ori <- umfang_best(Rd_Data, BS_Data)
Ra_An_D <- An_D[cbind(BS_row, BS_col)]
Ra_An_A <- An_A[cbind(BS_row, BS_col)]
Vergleich <- data.frame(BS_Data$Name, Ra_Ori, BS_Data$Value, Ra_An_D, Ra_An_A)
names(Vergleich) <- c("Station", "Radar Daten", "Station Daten", "Differenzenverfahren", "Faktorenverfahren")
Vergleich_plot <- melt(Vergleich)


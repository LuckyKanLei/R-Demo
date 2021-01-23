library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
## Abbildungen stellen ####
min_Lon <- 11.85
max_Lon <- 15.65
min_Lat <- 49.95
max_Lat <- 52.35
## Abb_0 ####
Ab_Ra_BS_0 <-
  ggplot() +
  geom_raster.seq(Rd_Data) +
  geom_point(aes(x = BS_Data$Lon, y = BS_Data$Lat, colour = BS_Data$Value)) +
  geom_text(aes(x = BS_Data$Lon, y = BS_Data$Lat, label = BS_Data$Name), size = 2.5, vjust = 1) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  scale_fill_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  coord_fixed(ratio = 1) + theme_bw() +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  xlab("Längengrad [°]") + ylab("Breitengrad [°]") +
  theme(#axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ggtitle("Niederschlagsradar und Bodenstationen - ohne Aneichung", "9. Januar 2019, 12 Uhr")
ggsave("Ab_Ra_BS_0.png", Ab_Ra_BS_0, "png", width = 9, height = 5)
## Abb_1 ####
Ab_Ra_BS <-
  ggplot() +
  geom_raster.seq(Rd_Data) +
  geom_point(aes(x = BS_Data$Lon, y = BS_Data$Lat, colour = BS_Data$Value), show.legend = F) +
  geom_text(aes(x = BS_Data$Lon, y = BS_Data$Lat, label = BS_Data$Name), size = 2.5, vjust = 1) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  scale_fill_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  coord_fixed(ratio = 1) + theme_bw() +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ggtitle("Niederschlagsradar und Bodenstationen - ohne Aneichung", "9. Januar 2019, 12 Uhr")
ggsave("Ab_Ra_BS.png", Ab_Ra_BS, "png", width = 9, height = 5)

## Abb_2 ####
Ab_Krige <-
  ggplot() +
  geom_raster.seq(Sm_Krige) +
  geom_point(aes(x = BS_Data$Lon, y = BS_Data$Lat, colour = BS_Data$Value)) +
  geom_text(aes(x = BS_Data$Lon, y = BS_Data$Lat, label = BS_Data$Name), size = 2.5, vjust = 1) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  scale_fill_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  coord_fixed(ratio = 1) + theme_bw() +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ggtitle("Interpolierte Niederschlagshöhen aus den Bodenstationen", "Kriging Interpolation")

Ab_2 <-
  ggplot() +
  geom_raster.seq(Sm_Krige) +
  geom_point(aes(x = BS_Data$Lon, y = BS_Data$Lat, colour = BS_Data$Value), show.legend = F) +
  geom_text(aes(x = BS_Data$Lon, y = BS_Data$Lat, label = BS_Data$Name), size = 2.5, vjust = 1) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  scale_fill_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  coord_fixed(ratio = 1) + theme_bw() +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ggtitle("Interpolierte Niederschlagshöhen aus den Bodenstationen", "Kriging Interpolation")
# ggsave("Ab_Krige.png", Ab_Krige, "png", width = 9, height = 5)


## Abb_3 ####
Ab_An_D <-
  ggplot() +
  geom_raster.seq(An_D) +
  geom_point(aes(x = BS_Data$Lon, y = BS_Data$Lat, colour = BS_Data$Value), show.legend = F) +
  geom_text(aes(x = BS_Data$Lon, y = BS_Data$Lat, label = BS_Data$Name), size = 2.5, vjust = 1) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  scale_fill_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  coord_fixed(ratio = 1) + theme_bw() +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ggtitle("Angeeichtes Niederschlagsradarfeld", "Unter Verwendung des Differenzenverfahrens")
ggsave("Ab_An_D.png", Ab_An_D, "png", width = 9, height = 5)

## Abb_4 ####
Ab_D <-
  ggplot() +
  geom_raster.seq(AF_D_raster) +
  geom_point(aes(x = BS_Data$Lon, y = BS_Data$Lat, colour = BS_Data$Value), show.legend = F) +
  geom_text(aes(x = BS_Data$Lon, y = BS_Data$Lat, label = BS_Data$Name), size = 2.5, vjust = 1) +
  geom_label(aes(x = 15.1, y = 50.25), label = expression(paste("D = N"[B], " - N"[R])), label.r = unit(0, "lines"), size = 5) +
  scale_fill_gradientn("[mm/h]", colours = brewer.pal(9, "YlOrRd"), limits = c(-0.1, 1.1)) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  coord_fixed(ratio = 1) + theme_bw() +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ggtitle("Gewichtete Differenzen D", "Differenzenverfahren")

Ab_4 <-
  ggplot() +
  geom_raster.seq(AF_D_raster, show.legend = F) +
  geom_point(aes(x = BS_Data$Lon, y = BS_Data$Lat, colour = BS_Data$Value), show.legend = F) +
  geom_text(aes(x = BS_Data$Lon, y = BS_Data$Lat, label = BS_Data$Name), size = 2.5, vjust = 1) +
  geom_label(aes(x = 15.1, y = 50.25), label = expression(paste("D = N"[B], " - N"[R])), label.r = unit(0, "lines"), size = 5) +
  scale_fill_gradientn("[mm/h]", colours = brewer.pal(9, "YlOrRd"), limits = c(-0.1, 1.1)) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  coord_fixed(ratio = 1) + theme_bw() +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ggtitle("Gewichtete Differenzen D", "Differenzenverfahren")
# ggsave("Ab_D.png",Ab_D, "png", width = 9, height = 5)

## Abb_5 ####
Ab_An_A <-
  ggplot() +
  geom_raster.seq(An_A) +
  geom_point(aes(x = BS_Data_A$Lon, y = BS_Data_A$Lat, colour = BS_Data_A$Value), show.legend = F) +
  geom_text(aes(x = BS_Data_A$Lon, y = BS_Data_A$Lat, label = BS_Data_A$Name), size = 2.5, vjust = 1) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  scale_fill_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  coord_fixed(ratio = 1) + theme_bw() +
  xlab(NULL) + ylab(NULL) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  ggtitle("Angeeichtes Niederschlagsradarfeld", "Unter Verwendung des Faktorenverfahrens")
ggsave("Ab_An_A.png",Ab_An_A, "png", width = 9, height = 5)

## Abb_6 ####
Ab_A <-
  ggplot() +
  geom_raster.seq(AF_A_raster) +
  geom_point(aes(x = BS_Data_A$Lon, y = BS_Data_A$Lat, colour = BS_Data_A$Value), show.legend = F) +
  geom_text(aes(x = BS_Data_A$Lon, y = BS_Data_A$Lat, label = BS_Data_A$Name), size = 2.5, vjust = 1) +
  geom_label(aes(x = 15.1, y = 50.25), label = expression(paste("A = ", frac("N"[B], "N"[R]))), label.r = unit(0, "lines"), size = 5) +
  scale_fill_gradientn("[-]", colours = brewer.pal(9, "Greens"), limits = c(0.9, 6.5)) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  coord_fixed(ratio = 1) + theme_bw() +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ggtitle("Gewichtete Aneichfaktoren A", "Faktorenverfahren")
Ab_6 <-
  ggplot() +
  geom_raster.seq(AF_A_raster, show.legend = F) +
  geom_point(aes(x = BS_Data_A$Lon, y = BS_Data_A$Lat, colour = BS_Data_A$Value), show.legend = F) +
  geom_text(aes(x = BS_Data_A$Lon, y = BS_Data_A$Lat, label = BS_Data_A$Name), size = 2.5, vjust = 1) +
  geom_label(aes(x = 15.1, y = 50.25), label = expression(paste("A = ", frac("N"[B], "N"[R]))), label.r = unit(0, "lines"), size = 5) +
  scale_fill_gradientn("[-]", colours = brewer.pal(9, "Greens"), limits = c(0.9, 6.5)) +
  scale_colour_gradient2("[mm/h]", low = "white", mid = "skyblue", high = "blue4", midpoint = .8, limits = c(0, 1.8)) +
  scale_x_continuous(limits = c(min_Lon,max_Lon), expand = c(0,0)) +
  scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  coord_fixed(ratio = 1) + theme_bw() +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ggtitle("Gewichtete Aneichfaktoren A", "Faktorenverfahren")
# ggsave("Ab_A.png",Ab_A, "png", width = 9, height = 5)



## Abb_7 ####
Ab_Vg <-
  ggplot(Vergleich_plot, aes(x=Station,y=value,fill=variable))+
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual("", values = c("skyblue", "blue2", "orange1", "springgreen2"),
                    labels = c("Pixelwert\n(bester Wert in Neuner-Umgebung)",
                               "Ombrometermessung",
                               "Pixelwert nach Aneichung\n(Differenzenverfahren)",
                               "Pixelwert nach Aneichung\n(Faktorenverfahren)")) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Station") + ylab("Niederschlag [mm]") +
  scale_y_continuous(limits = c(0,1.5), expand = c(0,0)) +
  # scale_y_continuous(limits = c(min_Lat,max_Lat), expand = c(0,0)) +
  ggtitle("Vergleich der Niederschlagshöhen: Bodenstation - Radarpixel")
ggsave("Ab_Vg.png",Ab_Vg, "png", width = 12, height = 6)



## arrange ####

aligned_0 <- align_plots(Ab_Ra_BS, Ab_Krige, Ab_An_D, Ab_D, Ab_An_A, Ab_A, align="hv", axis="tblr")
aligend_r <- align_plots(Ab_Krige, Ab_D, Ab_A, align="hv", axis="tblr")

aligned_1 <- align_plots(Ab_Ra_BS, aligend_r[[1]], align="hv", axis="tb")
aligned_2 <- align_plots(Ab_An_D, aligend_r[[2]], align="hv", axis="tb")
aligned_3 <- align_plots(Ab_An_A, aligend_r[[3]], align="hv", axis="tb")
Ab_Arrange <- ggarrange(aligned_1[[1]], aligned_1[[2]],
                        aligned_2[[1]], aligned_2[[2]],
                        aligned_3[[1]], aligned_3[[2]],
                        ncol = 2, nrow = 3, align="hv", label.x = "x")
Ab_Arrange <- annotate_figure(Ab_Arrange, left = text_grob("Breitengrad [°]",
                                                           rot = 90, size = 15),
                bottom = text_grob("Längengrad [°]", size = 15))
ggsave("Ab_Arrange.png",Ab_Arrange, "png", width = 12.5, height = 12.5)



for (i in 1:6) {
  ggsave(paste0("Abb_", i, ".png"), aligned_plots[[i]], "png", width = 6, height = 5)
}




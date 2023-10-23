
############## START SETUP
prj_path <- agvise::setup_project("RWA_potato", "agworkflows")
################ SETUP END

ds <- readRDS(file.path(prj_path, "/data/intermediate/compiled_potato_fertiliser_trial_data.RDS"))

library(ggplot2)

png(file.path(prj_path, "img/explore_yield_range.png"))
	#plot showing yield ranges by experiment and season:
	ds |>
	  ggplot(aes(x = season, y = TY)) +
	  geom_boxplot() +
	  facet_wrap(~expCode, scales="free_y", ncol=1) +
	  coord_flip()+
	  theme_gray()+
	  ylab("\nPotato tuber yield [t/ha]")+
	  theme(axis.title.x = element_text(size = 15, face="bold"),
			axis.title.y = element_blank(),
			axis.text = element_text(size = 14),
			strip.text = element_text(size = 14, face="bold", hjust=0))
dev.off()

#plot showing variation in yield as affected by NPK rate by experiment and season:

png(file.path(prj_path, "img/explore_yield_npk.png"))
	ds |>
	  tidyr::gather(nutrient, rate, N:K) |>
	  dplyr::mutate(nutrient = factor(nutrient, levels=c("N", "P", "K"))) |>
	  ggplot(aes(rate, TY)) + 
	  geom_point(alpha=.33, shape=16) +
	  facet_grid(nutrient ~ expCode+season) + 
	  xlab("\nFertilizer nutrient application rate [kg/ha]") +
	  ylab("Observed tuber yield [kg/ha]\n") +
	  theme(axis.title = element_text(size = 15, face="bold"),
			axis.text = element_text(size = 14),
			strip.text = element_text(size = 14, face="bold"))
dev.off()

#map with trial locations:
rwshp0 <- geodata::gadm("RWA", level=0) |> sf::st_as_sf()
rwshp1 <- geodata::gadm("RWA", level=1) |> sf::st_as_sf()
rwshp2 <- geodata::gadm("RWA", level=2) |> sf::st_as_sf()
#rwshp3 <- geodata::gadm("RWA", level=3) |> sf::st_as_sf()
#rwshp4 <- geodata::gadm("RWA", level=4) |> sf::st_as_sf()

rwlake <- sf::st_as_sf(terra::vect(file.path(prj_path, "data/raw/spatial/RWA_Lakes_NISR.shp")))
rwAEZ <- sf::st_as_sf(terra::vect(file.path(prj_path,"data/raw/spatial/AEZ_DEM_Dissolve.shp")))
rwAEZ <- rwAEZ[rwAEZ$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]

provs <- c("Amajyaruguru", "Amajyepfo", "Iburengerazuba")

png(file.path(prj_path, "img/explore_trial_locations.png"))

	ggplot()+
	  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
	  geom_sf(data = rwAEZ, aes(fill = Names_AEZs)) +
	  geom_sf(data = rwlake, size=NA, fill="lightblue")+
	  #geom_sf(data = rwshp3[rwshp3$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.2, color = "white", fill=NA) + 
	  geom_sf(data = rwshp2, linewidth = 0.6, color = "grey", fill=NA) +
	  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
	  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
	  geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% provs,], aes(label = NAME_2))+
	  geom_point(data = ds, aes(x=as.numeric(lon), y=as.numeric(lat), shape = expCode, colour = expCode, size = expCode))+
	  scale_shape_manual(values = c(15, 16, 18))+
	  scale_size_manual(values = c(3,3,4))+
	  scale_colour_manual(values = c("cornflowerblue", "blue", "blue4"))+
	  scale_fill_manual(values = c("darkgoldenrod1", "darkgoldenrod", "burlywood"))+
	  #xlim(29.2, 30.3)+
	  #ylim(-2.3,-1.3)+
	  theme_bw()+
	  xlab("Longitude")+
	  ylab("Latitude")+
	  theme(axis.title = element_blank(),
			axis.text = element_text(size=14),
			legend.title = element_text(size=18, face="bold"),
			legend.text = element_text(size=18),
			strip.text = element_text(size=14, face="bold"))

dev.off()

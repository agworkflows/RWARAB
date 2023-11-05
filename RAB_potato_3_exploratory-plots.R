### SETUP
prj_path <- wow::init("RWA_potato", "agworkflows")


ds <- readRDS(file.path(prj_path, "/data/intermediate/compiled_potato_fertiliser_trial_data.RDS"))

library(ggplot2)

png(file.path(prj_path, "img/3_explore_yield_range.png"))
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

png(file.path(prj_path, "img/3_explore_yield_npk.png"))
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

rwa0 <- geodata::gadm("RWA", level=0, path=prj_path) |> sf::st_as_sf()
rwa1 <- geodata::gadm("RWA", level=1, path=prj_path) |> sf::st_as_sf()
rwa2 <- geodata::gadm("RWA", level=2, path=prj_path) |> sf::st_as_sf()
#rwa3 <- geodata::gadm("RWA", level=3, path=prj_path) |> sf::st_as_sf()

rwa1$ENAME_1 <- sapply(strsplit(rwa1$VARNAME_1, "\\|"), \(i)i[1])
rwa2 <- merge(rwa2, data.frame(rwa1)[, c("NAME_1", "ENAME_1")], by="NAME_1")
#rwa3 <- merge(rwa3, data.frame(rwa1)[, c("NAME_1", "ENAME_1")], by="NAME_1")

rwlake <- sf::st_as_sf(terra::vect(file.path(prj_path, "data/raw/spatial/RWA_Lakes_NISR.shp")))
rwAEZ <- sf::st_as_sf(terra::vect(file.path(prj_path,"data/raw/spatial/AEZ_DEM_Dissolve.shp")))
rwAEZ <- rwAEZ[rwAEZ$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]

#provs <- c("Amajyaruguru", "Amajyepfo", "Iburengerazuba")

png(file.path(prj_path, "img/3_explore_trial_locations.png"))

	ggplot()+
	  geom_sf(data = rwa0, linewidth = 1.2, color = "black", fill=NA) + 
	  geom_sf(data = rwAEZ, aes(fill = Names_AEZs)) +
	  geom_sf(data = rwlake, size=NA, fill="lightblue")+
	  geom_sf(data = rwa2, linewidth = 0.6, color = "grey", fill=NA) +
	  geom_sf(data = rwa1, linewidth = 0.8, color = "black", fill=NA) + 
	  geom_sf(data = rwa0, linewidth = 1.2, color = "black", fill=NA) + 
		geom_sf_text(data = rwa2[rwa2$ENAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
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

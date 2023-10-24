
############## START SETUP
prj_path <- agvise::setup_project("RWA_potato", "agworkflows")
################ SETUP END

ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blup.RDS"))

#############################
# 5. Running reverse QUEFTS #
#############################

supply <- NULL

#converting to kg DM/ha, assuming 79% moisture content
ds$Y <- ds$blup * 1000 * 0.21
qcrop <- Rquefts::quefts_crop("potato")
qsoil <- Rquefts::quefts_soil()

supply <- lapply(test, function(i) {
  #subsetting and preparing data for revQUEFTS:
	dsi <- ds[ds$TLID == i,]

  #attainable yield is set to 20% above yield obtained with high NPK rate:
	Yai <- mean(dsi[dsi$N > 75 & dsi$P > 30 & dsi$K > 50, "Y"]) * 1.2
  
  #at least 3 rows of data are needed + attainable yield:
	if (length(unique(dsi$treat)) > 2 & !is.na(Yai)){
    
		si <- Rquefts::revSupply(dsi, Yai, qcrop, qsoil, leaf_ratio=0.17, stem_ratio=0.14, SeasonLength = 120)
		data.frame(TLID = i, Ya = Yai, N_base_supply = si[1],
                             P_base_supply = si[2], K_base_supply = si[3])
	} else {
		data.frame(TLID = i, Ya = Yai, N_base_supply = NA, P_base_supply = NA, K_base_supply = NA)
	}
})

supply <- do.call(rbind, out)

saveRDS(supply, file.path(prj_path, "data/intermediate/compiled_potato_fertiliser_trial_calculated_supply_afterlmer_sqrttf.RDS")


##### plots ###

library(ggplot2)


INS <- supply |>
  #adding lats and lons and data source:
  dplyr::left_join(ds |> dplyr::select(TLID, lat, lon, expCode, season) |> unique()) |>
  dplyr::mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) |>
  #setting negative values to zero and maximal values to 750:
  dplyr::mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x)),
         #across(c(N_base_supply:K_base_supply), ~ ifelse(.x > 400, 400, .x)),
         #N_base_supply = ifelse(N_base_supply > 90, 90, N_base_supply),
         P_base_supply = ifelse(P_base_supply > 1000, 1000, P_base_supply),
         #K_base_supply = ifelse(K_base_supply > 105, 105, K_base_supply)
         ) |>
  dplyr::mutate(season_AB = ifelse(grepl("A", season), "A", "B")) |>
  #remove incomplete rows:
  na.omit()

#Create plot to demonstrate ranges in supply by expCode and season combinations:
png(file.path(prj_path, "img/rev_quefts.png"))

INS |>
  dplyr::gather(variable, value, N_base_supply:K_base_supply) |>
  dplyr::mutate(variable = factor(variable, levels = c("N_base_supply", "P_base_supply", "K_base_supply")),
         variable = revalue(variable, c("N_base_supply" = "N",
                                        "P_base_supply" = "P",
                                        "K_base_supply" = "K")),
         season = ifelse(grepl("A", season), "A", "B")) |>
  ggplot(aes(x = expCode, y = value, fill = season)) + 
  geom_boxplot()+
  scale_fill_manual(values = c("grey90", "grey50"))+
  facet_wrap(~variable, nrow=1) +
  #ylim(0,500) +
  theme_gray() +
  ylab("Indigenous nutrient supply (kg/ha)\n") +
  theme(axis.title.y = element_text(size = 15, face="bold"),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.07, 0.9),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))

dev.off()

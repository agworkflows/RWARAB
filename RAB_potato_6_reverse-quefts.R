### SETUP
prj_path <- wow::init("RWA_potato", "agworkflows")


ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blup.RDS"))

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


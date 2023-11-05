### SETUP
prj_path <- wow::init("RWA_potato", "agworkflows")

# Predict yield and compare different models #

INS <- readRDS(file.path(prj_path, "data/intermediate/INS.RDS"))
ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blup.RDS"))
supply <- readRDS(file.path(prj_path, "data/intermediate/compiled_potato_fertiliser_trial_calculated_supply_afterlmer_sqrttf.RDS"))


d <- INS[, c("TLID", "N_base_supply", "P_base_supply", "K_base_supply", "N_pred", "P_pred", "K_pred")]
sni <- apply(supply[,c("N_base_supply", "P_base_supply", "K_base_supply")], 2, median)
d$N_sup <- sni[1]
d$P_sup <- sni[2]
d$K_sup <- sni[3]
d$ayi <- (as.numeric(INS$refY) * 10 * 1000 * 0.21) |> unlist() 
d <- merge(ds[, c("TLID", "N", "P", "K", "blup", "TY")], d, by="TLID")
d <- d[order(d[,1], d[,2], d[,3], d[,4], d[,6]), ]

rownames(d) <- NULL
leafrat <- 0.17
stemrat <- 0.14
qmod <- Rquefts::quefts(crop=Rquefts::quefts_crop("potato"))

py <- d[, c("TLID", "N", "P", "K")]
py$Yq = Rquefts::batch(qmod, d[,c("N_base_supply", "P_base_supply", "K_base_supply")], d[,c("N", "P", "K")], d$ayi, leafrat, stemrat) / 210
py$Yp = Rquefts::batch(qmod, d[,c("N_pred", "P_pred", "K_pred")], d[,c("N", "P", "K")], d$ayi, leafrat, stemrat) / 210
py$Yn = Rquefts::batch(qmod, d[,c("N_sup", "P_sup", "K_sup")], d[,c("N", "P", "K")], d$ayi, leafrat, stemrat) / 210

py$Yb <- d$blup
py$Yo <- d$TY

saveRDS(py, file.path(prj_path, "/data/intermediate/py.RDS"))


## Old (but improved version) method
#runQUEFTS <- function(nut_rates, #df with columns N, P and K with fertilizer nutrient rates in kg/ha
#                      supply, #vector with N, P and K supply
#                      q, 
#					  Ya, #attainable yield
#                      leaf_ratio, stem_ratio){ 
    
  #replacing the default by the values for N, P and K supply:
#	q$soil$N_base_supply <- supply[1]
#	q$soil$P_base_supply <- supply[2]
#	q$soil$K_base_supply <- supply[3]
  
  #attainable yield, assuming ds contains a treatment which eliminates nutrient deficiency
  ## assuming the leaf ratio is leaf biomass estimation knowing the yield (leaf / yield)
#	q$leaf_att  = Ya * leaf_ratio
#	q$stem_att  = Ya * stem_ratio
#   q$store_att = Ya
  
#	Yq <- vector(mode = "numeric", length = nrow(nut_rates))
#	for(i in 1:nrow(nut_rates)){
#		q$N <- nut_rates$N[i] 
#		q$P <- nut_rates$P[i] 
#        q$K <- nut_rates$K[i]
#		Yq[i] <- Rquefts::run(q)["store_lim"]
#	}
#	Yq
#}


# create quefts model (only do this once for efficiency)
#qmod <- Rquefts::quefts(crop=Rquefts::quefts_crop("potato"))
#leaf_ratio <- 0.17
#stem_ratio <- 0.14

#sni <- INS |> dplyr::select(N_base_supply, P_base_supply, K_base_supply) |> 
#    dplyr::summarise(across(everything(), list(median))) |> unlist()

#py <- lapply(unique(INS$TLID), function(i) {
#    dsi <- ds[ds$TLID == i,]
#	fri <- dsi[, c("N", "P", "K")]
  
  #different supply estimates:
#	sqi <- INS[INS$TLID == i, c("N_base_supply", "P_base_supply", "K_base_supply")] |> unlist()
#	spi <- INS[INS$TLID == i, c("N_pred", "P_pred", "K_pred")] |> unlist()

#	ayi <- (as.numeric(INS[INS$TLID == i,]$refY) * 10 * 1000 * 0.21) |> unlist() 
  
  #yield predicted using reverse Quefts calculated supply
#	yqi <- runQUEFTS(nut_rates = fri, supply = sqi, qmod, Ya = ayi, leaf_ratio, stem_ratio)
  
  #yield predicted using supply obtained from predictions by RF
#	ypi <- runQUEFTS(nut_rates = fri, supply = spi, qmod, Ya = ayi, leaf_ratio, stem_ratio)

  #yield predicted by a naive model using median values of NPK supply across all TLIDs:
#	yni <- runQUEFTS(nut_rates = fri, supply = sni, qmod, Ya = ayi, leaf_ratio, stem_ratio)
  
#	data.frame(TLID = i, N = fri$N, P = fri$P, K = fri$K,
#               Yq = yqi,   #yield predicted using revQUEFTS supply
#				Yp = ypi,  #yield predicted using RF predicted supply
#                Yn = yni,  #yield predicted using median nutrient supply
#                Yb = dsi$blup,          #yield blup
#                Yo = dsi$TY)          #yield observed
#})

#py <- do.call(rbind, py)

#py$Yq <- py$Yq / 1000 / 0.21
#py$Yp <- py$Yp / 1000 / 0.21
#py$Yn <- py$Yn / 1000 / 0.21

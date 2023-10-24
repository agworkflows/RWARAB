############## START SETUP
prj_path <- agvise::setup_project("RWA_potato", "agworkflows")
################ SETUP END

INS <- readRDS(file.path(prj_path, "data/intermediate/INS.RDS"))
ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blup.RDS"))


#################################################
# 7. Predict yield and compare different models #
#################################################



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



## Old (but improved method)
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

library(ggplot2)

py |> dplyr::left_join(ds |> dplyr::select(TLID, expCode, season) |> unique()) |>
  #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") |>
  dplyr::mutate(refY = ifelse(N > 75 & P > 30 & K > 50, "Reference treatment", "Other treatments")) |>
  ggplot(aes(x = Yp, y = Yb)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  #stat_poly_line(se = F) +
  #stat_poly_eq(aes(label = after_stat(eq.label)), size=6) +
  #stat_poly_eq(label.y = 0.9, size = 6) +
  #facet_wrap(~refY) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +

  #facet_wrap(~expCode+season, ncol=3) + 
  xlab("\nPredicted tuber yield (t/ha)")+
  ylab("BLUP potato tuber yield (t/ha)\n")+
  xlim(0, 62.5)+
  ylim(0, 62.5)+
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))
  

pyr <- py |>
  tidyr::gather(variable, value, Yq:Yo) |>
  dplyr::group_by(TLID, N, P, K, variable) |>
  dplyr::summarise(value = mean(value)) |>
  dplyr::mutate(treat = ifelse(N>75 & P>30 & K>50, "ref", "other")) |>
  dplyr::group_by(TLID, variable) |>
  dplyr::mutate(refY = mean(ifelse(treat == "ref", value, NA), na.rm=TRUE),
         dY = refY - value) |>
  dplyr::filter(treat != "ref") |>
  dplyr::select(-treat, -value, -refY) |>
  tidyr::spread(variable, dY) |>
  tidyr::gather(variable, value, c(Yq, Yp, Yo, Yn)) |>
  dplyr::mutate(variable = plyr::mapvalues(variable, 
                              from = c("Yq", "Yp", "Yn"),
                              to = c("supply from reverse QUEFTS", "supply by RF prediction", "simple medians for supply"))) |>
  dplyr::ungroup()


pyr |>
  dplyr::filter(variable != "Yo") |>
  #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") |>
  dplyr::mutate(variable = as.character(variable)) |>
  ggplot(aes(x = value, y = Yb)) + 
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = pyr |> 
              dplyr::filter(variable != "Yo") |> 
              dplyr::group_by(variable) |> 
              dplyr::summarise(rmse = sqrt(sum((value - Yb)**2)/ dplyr::n()),
                        value = -1,
                        Yb = 20),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("\nPredicted potato tuber yield difference to reference treatment [t/ha]") +
  ylab("BLUP potato tuber yield difference to reference treatment [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq")),
                        formula = y ~ x, size = 6) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))


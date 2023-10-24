############## START SETUP
prj_path <- agvise::setup_project("RWA_potato", "agworkflows")
################ SETUP END

ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blub.RDS"))

############################################################################################
# 8. Alternative approach: Predict yield directly with Random Forest a la Jordan & Camilla #
############################################################################################


#Prepare dataset...
dsp <- ds |>
  dplyr::mutate(Y = blup) |>
  dplyr::group_by(TLID, N, P, K) |>
  dplyr::summarise(Y = mean(Y)) |>
  dplyr::mutate(treat = ifelse(N>75 & P>30 & K>50, "ref", "other")) |>
  dplyr::group_by(TLID) |>
  dplyr::mutate(refY = mean(ifelse(treat == "ref", Y, NA), na.rm=TRUE),
         dY = refY - Y) |>
  dplyr::ungroup() |>
  dplyr::filter(treat != "ref") |>
  dplyr::select(-treat, -Y, -refY) |>
  dplyr::join(sdt) |>
  dplyr::join(ds_ref) |>
  dplyr::join(tdt) |>
  na.omit()

#First model that includes district and refY
RF0 <- randomForest::randomForest(dY ~ ., subset(dsp, select=-c(TLID, expCode)), importance=TRUE, ntree=200)
sqrt(RF0$mse[length(RF0$mse)])
RF0

#Simpler model without district and refY
RF1 <- randomForest::randomForest(dY ~ ., subset(dsp, select=-c(TLID, expCode, refY, district)), importance=TRUE, ntree=200)
sqrt(RF1$mse[length(RF1$mse)])
RF1

predRFs <- NULL

#Obtain yield estimates using LOOCV:
for(i in unique(dsp$TLID)){
  
  dsp_train <- subset(dsp[dsp$TLID != i,], select = -c(TLID, expCode))
  dsp_valid <- subset(dsp[dsp$TLID == i,], select = -c(TLID, expCode))
  
  #RF0 <- randomForest(dY ~ ., dsp_train, ntree=500)
  #RF1 <- randomForest(dY ~ ., subset(dsp_train, select = -c(district, refY)), ntree=500)
  
  RF0 <- ranger::ranger(formula = dY ~ ., data = dsp_train, num.trees = 200) 
  RF1 <- ranger::ranger(formula = dY ~ ., data = subset(dsp_train, select = -c(district, refY)), num.trees = 200) 

  predRFs <- rbind(predRFs, data.frame(TLID = i,
                                       subset(dsp_valid, select=c(N, P, K, dY)),
                                       #dYp0 = predict(RF0, dsp_valid),
                                       #dYp1 = predict(RF1, dsp_valid)
                                       dYp0 = predict(RF0, dsp_valid)$predictions,
                                       dYp1 = predict(RF1, dsp_valid)$predictions
                                       ))
  
}

predRFsl <- predRFs |>
  dplyr::gather(variable, value, dYp0:dYp1) |>
  dplyr::mutate(variable = plyr::mapvalues(variable, from = c("dYp0", "dYp1"), to = c("With refY", "without refY")))

#Visualize yield effects
ggplot(predRFsl, aes(x = value, y = dY)) + 
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = predRFsl |> 
              group_by(variable) |> 
              summarise(rmse = sqrt(sum((value - dY)**2)/n()),
                        value = -1,
                        dY = 20),
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


dsp <- ds |>
  join(sdt) |>
  join(ds_ref) |>
  join(tdt) |>
  dplyr::select(-c(expCode, FDID, lat, lon, season, plantingDate, harvestDate, treat, N100:K100, TY))

fr <- data.frame("N" = seq(0, 180, 1),
                 "P" = 22,
                 "K" = 42)

res <- NULL
run <- 0

for(tlid in sample(unique(INS[INS$expCode != "IFDC",]$TLID), 20)){
  
  run <- run+1
  print(run)
  
  sp <- INS[INS$TLID == tlid, c("N_pred", "P_pred", "K_pred")]
  ay <- as.numeric(INS[INS$TLID == tlid,]$refY) * 10 * 1000 * 0.21
  yp <- runQUEFTS(nut_rates = fr,
                  supply = as.numeric(sp),
                  crop = "Potato",
                  Ya = ay,
                  SeasonLength = 120) / 1000 / 0.21
  
  dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) |> na.omit()
  dsp_prdct <- dsp[dsp$TLID == tlid,] |>
    dplyr::select(-c(N, P, K, blup)) |>
    unique() |>
    cross_join(fr)
  
  #RF0 <- randomForest(blup ~ ., dsp_train, ntree=500)
  RF0 <- ranger(formula = blup ~ ., data = dsp_train, num.trees = 200) 
  
  #ypRF <- predict(RF0, dsp_prdct)
  ypRF = predict(RF0, dsp_prdct)$predictions
  
  
  res <- rbind(res, data.frame(TLID = tlid,
                               fr,
                               yp = yp,
                               ypRF = ypRF))
  
}
res |>
  filter(grepl("RwaSIS", TLID) | grepl("RS", TLID)) |>
  ggplot()+
  geom_point(aes(x = N, y=yp))+
  geom_point(aes(x = N, y=ypRF), colour="blue")+
  geom_point(data=ds[ds$TLID %in% res$TLID & ds$P == 22 & ds$K == 42 & (grepl("RwaSIS", ds$TLID) | grepl("RS", ds$TLID)),], 
             aes(x=N, y=blup), colour="red", size = 4)+
  facet_wrap(~TLID) + 
  xlab("\nN fertilizer application rate (kg N/ha)") +
  ylab("Potato tuber yield [t/ha]\n")+
  theme_gray() +
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))


fr <- data.frame("N" = 51,
                 "P" = 0:50,
                 "K" = 42)

res <- NULL
run <- 0

for(tlid in sample(unique(INS[INS$expCode != "IFDC",]$TLID), 20)){
  
  run <- run+1
  print(run)
  
  sp <- INS[INS$TLID == tlid, c("N_pred", "P_pred", "K_pred")]
  ay <- as.numeric(INS[INS$TLID == tlid,]$refY) * 10 * 1000 * 0.21
  yp <- runQUEFTS(nut_rates = fr,
                  supply = as.numeric(sp),
                  crop = "Potato",
                  Ya = ay,
                  SeasonLength = 120) / 1000 / 0.21
  
  dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) |> na.omit()
  dsp_prdct <- dsp[dsp$TLID == tlid,] |>
    dplyr::select(-c(N, P, K, blup)) |>
    unique() |>
    cross_join(fr)
  
  #RF0 <- randomForest(blup ~ ., dsp_train, ntree=500)
  RF0 <- ranger(formula = blup ~ ., data = dsp_train, num.trees = 200) 
  
  #ypRF <- predict(RF0, dsp_prdct)
  ypRF = predict(RF0, dsp_prdct)$predictions
  
  res <- rbind(res, data.frame(TLID = tlid,
                               fr,
                               yp = yp,
                               ypRF = ypRF))
  
}

res |>
  filter(grepl("RwaSIS", TLID) | grepl("RS", TLID)) |>
  ggplot()+
  geom_point(aes(x = P, y=yp))+
  geom_point(aes(x = P, y=ypRF), colour="blue")+
  geom_point(data=ds[ds$TLID %in% res$TLID & round(ds$N) == 51 & ds$K == 42 & (grepl("RwaSIS", ds$TLID) | grepl("RS", ds$TLID)),], 
             aes(x=P, y=blup), colour="red", size = 4)+
  facet_wrap(~TLID) + 
  xlab("\nP fertilizer application rate (kg P/ha)") +
  ylab("Potato tuber yield [t/ha]\n")+
  theme_gray() +
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))


fr <- data.frame("N" = 51,
                 "P" = 22,
                 "K" = 0:90)

res <- NULL
run <- 0

for(tlid in sample(unique(INS[INS$expCode != "IFDC",]$TLID), 20)){
  
  run <- run+1
  print(run)
  
  sp <- INS[INS$TLID == tlid, c("N_pred", "P_pred", "K_pred")]
  ay <- as.numeric(INS[INS$TLID == tlid,]$refY) * 10 * 1000 * 0.21
  yp <- runQUEFTS(nut_rates = fr,
                  supply = as.numeric(sp),
                  crop = "Potato",
                  Ya = ay,
                  SeasonLength = 120) / 1000 / 0.21
  
  dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) |> na.omit()
  dsp_prdct <- dsp[dsp$TLID == tlid,] |>
    dplyr::select(-c(N, P, K, blup)) |>
    unique() |>
    cross_join(fr)
  
  #RF0 <- randomForest(blup ~ ., dsp_train, ntree=500)
  RF0 <- ranger(formula = blup ~ ., data = dsp_train, num.trees = 200) 
  
  #ypRF <- predict(RF0, dsp_prdct)
  ypRF = predict(RF0, dsp_prdct)$predictions
  
  res <- rbind(res, data.frame(TLID = tlid,
                               fr,
                               yp = yp,
                               ypRF = ypRF))
  
}

res |>
  filter(grepl("RwaSIS", TLID) | grepl("RS", TLID)) |>
  ggplot()+
  geom_point(aes(x = K, y=yp))+
  geom_point(aes(x = K, y=ypRF), colour="blue")+
  geom_point(data=ds[ds$TLID %in% res$TLID & round(ds$N) == 51 & round(ds$P) == 22 & (grepl("RwaSIS", ds$TLID) | grepl("RS", ds$TLID)),], 
             aes(x=K, y=blup), colour="red", size = 4)+
  facet_wrap(~TLID) + 
  xlab("\nK fertilizer application rate (kg P/ha)") +
  ylab("Potato tuber yield [t/ha]\n")+
  theme_gray() +
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))


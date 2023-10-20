############## START SETUP
prj_path <- agvise::setup_project("RWA_potato", "agworkflows")
################ SETUP END


#################################################
# 7. Predict yield and compare different models #
#################################################

py <- NULL
ni <- 0

for(i in unique(INS$TLID)){
  
  print(paste0(round(ni/length(unique(INS$TLID))*100), "% complete"))
  ni <- ni+1
  
  dsi <- ds[ds$TLID == i,]
  fri <- data.frame("N" = dsi$N,
                    "P" = dsi$P,
                    "K" = dsi$K)
  
  #different supply estimates:
  sqi <- INS[INS$TLID == i, c("N_base_supply", "P_base_supply", "K_base_supply")]
  spi <- INS[INS$TLID == i, c("N_pred", "P_pred", "K_pred")]
  sni <- INS %>% dplyr::select(N_base_supply, P_base_supply, K_base_supply) %>% 
    summarise(across(everything(), list(median)))
  
  ayi <- as.numeric(INS[INS$TLID == i,]$refY) * 10 * 1000 * 0.21
  
  #yield predicted using reverse Quefts calculated supply
  yqi <- runQUEFTS(nut_rates = fri,
                   supply = as.numeric(sqi),
                   crop = "Potato",
                   Ya = ayi,
                   SeasonLength = 120)
  
  #yield predicted using supply obtained from predictions by RF
  ypi <- runQUEFTS(nut_rates = fri,
                   supply = as.numeric(spi),
                   crop = "Potato",
                   Ya = ayi,
                   SeasonLength = 120)
  
  #yield predicted by a naive model using median values of NPK supply across all TLIDs:
  yni <- runQUEFTS(nut_rates = fri,
                   supply = as.numeric(sni),
                   crop = "Potato",
                   Ya = ayi,
                   SeasonLength = 120)
  
  py <- rbind(py, data.frame(TLID = i,
                             N = fri$N,
                             P = fri$P,
                             K = fri$K,
                             Yq = yqi / 1000 / 0.21, #yield predicted using revQUEFTS supply
                             Yp = ypi / 1000 / 0.21, #yield predicted using RF predicted supply
                             Yn = yni / 1000 / 0.21, #yield predicted using median nutrient supply
                             Yb = dsi$blup,          #yield blup
                             Yo = dsi$TY))           #yield observed
}


py %>% left_join(ds %>% dplyr::select(TLID, expCode, season) %>% unique()) %>%
  #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") %>%
  mutate(refY = ifelse(N > 75 & P > 30 & K > 50, "Reference treatment", "Other treatments")) %>%
  ggplot(aes(x = Yp, y = Yb)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  #stat_poly_line(se = F) +
  #stat_poly_eq(aes(label = after_stat(eq.label)), size=6) +
  #stat_poly_eq(label.y = 0.9, size = 6) +
  #facet_wrap(~refY) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
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
  

pyr <- py %>%
  gather(variable, value, Yq:Yo) %>%
  group_by(TLID, N, P, K, variable) %>%
  summarise(value = mean(value)) %>%
  mutate(treat = ifelse(N>75 & P>30 & K>50, "ref", "other")) %>%
  group_by(TLID, variable) %>%
  mutate(refY = mean(ifelse(treat == "ref", value, NA), na.rm=TRUE),
         dY = refY - value) %>%
  filter(treat != "ref") %>%
  dplyr::select(-treat, -value, -refY) %>%
  spread(variable, dY) %>%
  gather(variable, value, c(Yq, Yp, Yo, Yn)) %>%
  mutate(variable = mapvalues(variable, 
                              from = c("Yq", "Yp", "Yn"),
                              to = c("supply from reverse QUEFTS", "supply by RF prediction", "simple medians for supply"))) %>%
  ungroup()


pyr %>%
  filter(variable != "Yo") %>%
  #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") %>%
  mutate(variable = as.character(variable)) %>%
  ggplot(aes(x = value, y = Yb)) + 
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = pyr %>% 
              filter(variable != "Yo") %>% 
              group_by(variable) %>% 
              summarise(rmse = sqrt(sum((value - Yb)**2)/n()),
                        value = -1,
                        Yb = 20),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("\nPredicted potato tuber yield difference to reference treatment [t/ha]") +
  ylab("BLUP potato tuber yield difference to reference treatment [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 6) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))


############################################################################################
# 8. Alternative approach: Predict yield directly with Random Forest a la Jordan & Camilla #
############################################################################################


#Prepare dataset...
dsp <- ds %>%
  mutate(Y = blup) %>%
  group_by(TLID, N, P, K) %>%
  summarise(Y = mean(Y)) %>%
  mutate(treat = ifelse(N>75 & P>30 & K>50, "ref", "other")) %>%
  group_by(TLID) %>%
  mutate(refY = mean(ifelse(treat == "ref", Y, NA), na.rm=TRUE),
         dY = refY - Y) %>%
  ungroup() %>%
  filter(treat != "ref") %>%
  dplyr::select(-treat, -Y, -refY) %>%
  join(sdt) %>%
  join(ds_ref) %>%
  join(tdt) %>%
  na.omit()

#First model that includes district and refY
RF0 <- randomForest(dY ~ ., subset(dsp, select=-c(TLID, expCode)), importance=TRUE, ntree=200)
sqrt(RF0$mse[length(RF0$mse)])
RF0

#Simpler model without district and refY
RF1 <- randomForest(dY ~ ., subset(dsp, select=-c(TLID, expCode, refY, district)), importance=TRUE, ntree=200)
sqrt(RF1$mse[length(RF1$mse)])
RF1

predRFs <- NULL

#Obtain yield estimates using LOOCV:
for(i in unique(dsp$TLID)){
  
  dsp_train <- subset(dsp[dsp$TLID != i,], select = -c(TLID, expCode))
  dsp_valid <- subset(dsp[dsp$TLID == i,], select = -c(TLID, expCode))
  
  #RF0 <- randomForest(dY ~ ., dsp_train, ntree=500)
  #RF1 <- randomForest(dY ~ ., subset(dsp_train, select = -c(district, refY)), ntree=500)
  
  RF0 <- ranger(formula = dY ~ ., data = dsp_train, num.trees = 200) 
  RF1 <- ranger(formula = dY ~ ., data = subset(dsp_train, select = -c(district, refY)), num.trees = 200) 

  predRFs <- rbind(predRFs, data.frame(TLID = i,
                                       subset(dsp_valid, select=c(N, P, K, dY)),
                                       #dYp0 = predict(RF0, dsp_valid),
                                       #dYp1 = predict(RF1, dsp_valid)
                                       dYp0 = predict(RF0, dsp_valid)$predictions,
                                       dYp1 = predict(RF1, dsp_valid)$predictions
                                       ))
  
}

predRFsl <- predRFs %>%
  gather(variable, value, dYp0:dYp1) %>%
  mutate(variable = mapvalues(variable, from = c("dYp0", "dYp1"), to = c("With refY", "without refY")))

#Visualize yield effects
ggplot(predRFsl, aes(x = value, y = dY)) + 
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = predRFsl %>% 
              group_by(variable) %>% 
              summarise(rmse = sqrt(sum((value - dY)**2)/n()),
                        value = -1,
                        dY = 20),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("\nPredicted potato tuber yield difference to reference treatment [t/ha]") +
  ylab("BLUP potato tuber yield difference to reference treatment [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 6) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))


dsp <- ds %>%
  join(sdt) %>%
  join(ds_ref) %>%
  join(tdt) %>%
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
  
  dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) %>% na.omit()
  dsp_prdct <- dsp[dsp$TLID == tlid,] %>%
    dplyr::select(-c(N, P, K, blup)) %>%
    unique() %>%
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
res %>%
  filter(grepl("RwaSIS", TLID) | grepl("RS", TLID)) %>%
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
  
  dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) %>% na.omit()
  dsp_prdct <- dsp[dsp$TLID == tlid,] %>%
    dplyr::select(-c(N, P, K, blup)) %>%
    unique() %>%
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

res %>%
  filter(grepl("RwaSIS", TLID) | grepl("RS", TLID)) %>%
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
  
  dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) %>% na.omit()
  dsp_prdct <- dsp[dsp$TLID == tlid,] %>%
    dplyr::select(-c(N, P, K, blup)) %>%
    unique() %>%
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

res %>%
  filter(grepl("RwaSIS", TLID) | grepl("RS", TLID)) %>%
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


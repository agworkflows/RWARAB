############## START SETUP
prj_path <- agvise::setup_project("RWA_potato", "agworkflows")
################ SETUP END

#############################
# 5. Running reverse QUEFTS #
#############################

supply <- NULL
for(i in unique(ds$TLID)){
  
  print(i)
  
  #subsetting and preparing data for revQUEFTS:
  dsi <- ds[ds$TLID == i,]
  names(dsi)[names(dsi) == "blup"] <- "Y" #Aim is to explain variation in the BLUP yields
  dsi$Y <- dsi$Y * 1000 * 0.21 #converting to kg DM/ha, assuming 79% moisture content
  
  #attainable yield is set to 20% above yield obtained with high NPK rate:
  Yai <- mean(dsi[dsi$N > 75 & dsi$P > 30 & dsi$K > 50,]$Y) * 1.2
  
  #at least 3 rows of data are needed + attainable yield:
  if(length(unique(dsi$treat)) > 2 & !is.na(Yai)){
    
    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = "Potato")
    print(si)
    supply <- rbind(supply, data.frame(TLID = i,
                                       Ya = Yai,
                                       N_base_supply = si[1],
                                       P_base_supply = si[2],
                                       K_base_supply = si[3]))
  }
}


#Alternative: run in parallel on multiple CPUs
#create a function to run revQUEFTS optimisation:
calculate_supply <- function(TLID){
  
  print(TLID)
  
  #subsetting and preparing data for revQUEFTS:
  dsi <- ds[ds$TLID == i,]
  names(dsi)[names(dsi) == "blup"] <- "Y"
  dsi$Y <- dsi$Y * 1000 * 0.21 #converting to kg DM/ha, assuming 79% moisture content
  
  #attainable yield is set to 20% above yield obtained with high NPK rate:
  Yai <- mean(dsi[dsi$N > 75 & dsi$P > 30 & dsi$K > 50,]$Y) * 1.2
  
  #at least 3 rows of data are needed + attainable yield:
  if(length(unique(dsi$treat)) > 2 & !is.na(Yai)){
    
    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = "Potato")
    print(si)
    
    supply <- data.frame(TLID = i,
                         Ya = Yai,
                         N_base_supply = si[1],
                         P_base_supply = si[2],
                         K_base_supply = si[3])
  }
  
  return(supply)
  
}

cls <- parallel::makePSOCKcluster(8)
doParallel::registerDoParallel(cls)
supply <- foreach(i = unique(ds$TLID)) %do% {calculate_supply(TLID = i)}
stopCluster(cls)
supply <- do.call(rbind, supply)

saveRDS(supply,   "compiled_potato_fertiliser_trial_calculated_supply_afterlmer_sqrttf.RDS")
supply <- readRDS("compiled_potato_fertiliser_trial_calculated_supply_afterlmer_sqrttf.RDS")

INS <- supply %>%
  #adding lats and lons and data source:
  left_join(ds %>% dplyr::select(TLID, lat, lon, expCode, season) %>% unique()) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  #setting negative values to zero and maximal values to 750:
  mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x)),
         #across(c(N_base_supply:K_base_supply), ~ ifelse(.x > 400, 400, .x)),
         #N_base_supply = ifelse(N_base_supply > 90, 90, N_base_supply),
         P_base_supply = ifelse(P_base_supply > 1000, 1000, P_base_supply),
         #K_base_supply = ifelse(K_base_supply > 105, 105, K_base_supply)
         ) %>%
  mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
  #remove incomplete rows:
  na.omit()

#Create plot to demonstrate ranges in supply by expCode and season combinations:
INS %>%
  gather(variable, value, N_base_supply:K_base_supply) %>%
  mutate(variable = factor(variable, levels = c("N_base_supply", "P_base_supply", "K_base_supply")),
         variable = revalue(variable, c("N_base_supply" = "N",
                                        "P_base_supply" = "P",
                                        "K_base_supply" = "K")),
         season = ifelse(grepl("A", season), "A", "B")) %>%
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



########################################################
# 6. Predict indigenous nutrient supply from soil data #
########################################################

#Soil information for prediction model:
sdt <- readRDS("Soil_PointData_trial.RDS") %>%
  #dplyr::select(-ID) %>%
  rename(TLID = ID,
         lon = longitude,
         lat = latitude,
         province = NAME_1,
         district = NAME_2) %>%
  #removing soil variables with unlikely predictive value:
  dplyr::select(-c(fe_top, fe_bottom, B_0_30, Cu_0_30, Mn_0_30))

sdt2 <- readRDS("compiled_potato_fertiliser_trial_soildata.RDS") %>%
  dplyr::filter(TLID == "SATLRW475382409484") %>%
  dplyr::select(-c(ID, wpg2_top, wpg2_bottom)) %>%
  dplyr::select(-c(fe_top, fe_bottom, B_0_30, Cu_0_30, Mn_0_30)) %>%
  rename(lon = x, lat = y) %>%
  mutate(province = "Iburengerazuba",
         district = "Rubavu")

sdt <- rbind(sdt, sdt2) %>%
  dplyr::select(-c(lat, lon))

#Productivity class:
ds_ref <- ds %>%
  #select treatments with high nutrient rates (Increased NPK for RS-PFR-1, NPK_all for IFDC, NPK11 for SA-VAL-1):
  filter(N > 75, P > 30, K > 50) %>%
  group_by(expCode, TLID) %>%
  summarise(refY = median(TY)) %>%
  mutate(refY = cut(refY, c(-Inf, 10, 20, 30, 40, Inf), labels = c("Very low", "Low", "Medium", "High", "Very high")))

#Topography and EAZ data:
tdt <- readRDS("Topography_AEZ_trial.RDS") %>%
  rename(TLID = ID,
         lon = longitude,
         lat = latitude,
         alt = altitude,
         AEZ = Names_AEZs,
         Province = NAME_1,
         District = NAME_2) %>%
  dplyr::select(-c(AEZs_no, lat, lon, Province, District)) %>%
  mutate(AltClass2 = cut(alt, breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf)),
         #AEZ = ifelse(AEZ == "Central plateau", "Congo-Nile watershed divide", AEZ)
         )

#Add predictors to INS dataset:
INS <- INS %>% 
  left_join(sdt) %>%
  left_join(ds_ref) %>%
  left_join(tdt) %>%
  na.omit()

#INS$P_base_supply_bin <- as.factor(ifelse(INS$P_base_supply < 50, "limiting", "non-limiting"))
#INS$K_base_supply_bin <- as.factor(ifelse(INS$K_base_supply < 200, "limiting", "non-limiting"))

#create subset with predictors only:
ins <- INS %>%
  filter(expCode != "IFDC") %>%
  dplyr::select(-c(TLID, lon, lat, expCode, season, Ya,
                   #season_AB, #season could be considered or dropped
                   #refY #reference yield class could be considered or dropped
                   ))

set.seed(666)
start <- Sys.time()
RF_N <- randomForest(log(N_base_supply) ~ ., subset(ins, select = -c(P_base_supply, K_base_supply)), 
                     importance=TRUE, ntree=1000)

RF_P <- randomForest(log(P_base_supply) ~ ., subset(ins, select = -c(N_base_supply, K_base_supply)),
                     importance=TRUE, ntree=1000)

RF_K <- randomForest(log(K_base_supply) ~ ., subset(ins, select = -c(N_base_supply, P_base_supply)),
                     importance=TRUE, ntree=1000)

end <- Sys.time()
t_randomForest <- end - start

require(ranger)
require(Metrics)

start <- Sys.time()
RF_N_ranger <- ranger(formula = log(N_base_supply) ~ ., 
                      data = subset(ins, select = -c(P_base_supply, K_base_supply)), 
                      num.trees=1000)
RF_P_ranger <- ranger(formula = log(P_base_supply) ~ ., 
                      data = subset(ins, select = -c(N_base_supply, K_base_supply)), 
                      num.trees=1000)
RF_K_ranger <- ranger(formula = log(K_base_supply) ~ ., 
                      data = subset(ins, select = -c(N_base_supply, P_base_supply)), 
                      num.trees=1000)
end <- Sys.time()
t_ranger <- end - start

rmse(ins$N_base_supply, exp(predict(RF_N_ranger, ins)$predictions))
rmse(ins$N_base_supply, exp(predict(RF_N, ins)))

rmse(ins$P_base_supply, exp(predict(RF_P_ranger, ins)$predictions))
rmse(ins$P_base_supply, exp(predict(RF_P, ins)))

rmse(ins$K_base_supply, exp(predict(RF_K_ranger, ins)$predictions))
rmse(ins$K_base_supply, exp(predict(RF_K, ins)))

#variable importance plots:
varImpPlot(RF_N)
varImpPlot(RF_P)
varImpPlot(RF_K)

#variance explained:
RF_N$rsq[1000]
RF_P$rsq[1000]
RF_K$rsq[1000]

#rmse
sqrt(RF_N$mse[1000])
sqrt(RF_P$mse[1000])
sqrt(RF_K$mse[1000])


#Leave-one-out cross-validation:
preds <- NULL
run <- 0

for(i in unique(INS$TLID)){
  
  print(paste0(round(run/length(unique(INS$TLID))*100), "% complete"))
  
  ins_train <- subset(INS[INS$TLID != i,], select = -c(TLID, lon, lat, expCode, season, Ya))
  ins_valid <- subset(INS[INS$TLID == i,], select = -c(TLID, lon, lat, expCode, season, Ya))
  
  RF_N <- randomForest(log(N_base_supply) ~ ., subset(ins_train, select = -c(P_base_supply, K_base_supply)), 
                       importance=TRUE, ntree=500, mtry = 15)
  
  RF_P <- randomForest(log(P_base_supply) ~ ., subset(ins_train, select = -c(N_base_supply, K_base_supply)),
                       importance=TRUE, ntree=500, mtry = 15)
  
  RF_K <- randomForest(log(K_base_supply) ~ ., subset(ins_train, select = -c(N_base_supply, P_base_supply)),
                       importance=TRUE, ntree=500, mtry = 15)
  
  preds <- rbind(preds, data.frame(TLID = i,
                                   N_pred = exp(predict(RF_N, ins_valid)),
                                   P_pred = exp(predict(RF_P, ins_valid)),
                                   K_pred = exp(predict(RF_K, ins_valid))))
  run <- run + 1
  
}

#Leave-one-out cross-validation using ranger (much faster with minimal increase in RMSE):
preds <- NULL
run <- 0

for(i in unique(INS$TLID)){
  
  print(paste0(round(run/length(unique(INS$TLID))*100), "% complete"))
  
  ins_train <- subset(INS[INS$TLID != i,], select = -c(TLID, lon, lat, expCode, season, Ya))
  ins_valid <- subset(INS[INS$TLID == i,], select = -c(TLID, lon, lat, expCode, season, Ya))
  
  RF_N <- ranger(formula = log(N_base_supply) ~ ., 
                        data = subset(ins_train, select = -c(P_base_supply, K_base_supply)), 
                        num.trees=500)
  
  RF_P <- ranger(formula = log(P_base_supply) ~ ., 
                 data = subset(ins_train, select = -c(N_base_supply, K_base_supply)), 
                 num.trees=500)
  
  RF_K <- ranger(formula = log(K_base_supply) ~ ., 
                 data = subset(ins_train, select = -c(N_base_supply, P_base_supply)), 
                 num.trees=500)
  
  preds <- rbind(preds, data.frame(TLID = i,
                                   N_pred = exp(predict(RF_N, ins_valid)$predictions),
                                   P_pred = exp(predict(RF_P, ins_valid)$predictions),
                                   K_pred = exp(predict(RF_K, ins_valid)$predictions)))
  run <- run + 1
  
}

saveRDS(preds, "LOOCV_predictions_NPK_base_supply_afterlmer.RDS")
preds <- readRDS("LOOCV_predictions_NPK_base_supply_afterlmer.RDS")

INS <- INS %>% left_join(preds)

pINS <- INS %>%
  dplyr::select(TLID, N_base_supply, P_base_supply, K_base_supply, N_pred, P_pred, K_pred) %>%
  gather(variable, value, -TLID) %>%
  mutate(nutrient = substr(variable, 1, 1),
         nutrient = factor(nutrient, levels = c("N", "P", "K")),
         variable = ifelse(grepl("pred", variable), "RandomForest", "revQUEFTS")) %>%
  spread(variable, value) %>%
  left_join(ds %>% dplyr::select(TLID, expCode, season) %>% unique()) %>%
  mutate(season = ifelse(grepl("A", season), "A", "B")) %>%
  #mutate(RandomForest = ifelse(nutrient == "N" & RandomForest >= 120, NA, 
  #                             ifelse(nutrient == "P"& RandomForest >= 30, NA,
  #                                    ifelse(nutrient == "K" & RandomForest >= 150, NA, RandomForest)))) %>%
  na.omit()
  


#plot demonstrating reverse QUEFTS calculated N, P and K supply versus RF predictions using LOOCV:
ggplot(pINS, aes(x = RandomForest, y = revQUEFTS)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(fill = season, shape=expCode), size = 3) + 
  scale_shape_manual(values = 21:23) +
  scale_fill_manual(values = c("grey90", "grey50"))+
  facet_wrap(~nutrient, nrow=1, scales="free") +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6,
                        label.y = .975) +
  geom_text(data = pINS %>% 
              group_by(nutrient) %>% 
              summarise(rmse = sqrt(sum((revQUEFTS - RandomForest)**2)/n()),
                        RandomForest = 0) %>%
              mutate(revQUEFTS = c(200, 200, 200)*0.95),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlim(0, NA)+
  ylim(0, 200)+
  xlab("\nRandom Forest predicted soil nutrient supply [kg/ha]")+
  ylab("Reverse QUEFTS calculated supply [kg/ha]\n") +
  theme_gray() +
  theme(strip.text = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        legend.position = "none",
        axis.title = element_text(size = 15, face="bold"))

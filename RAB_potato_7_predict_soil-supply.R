### SETUP
prj_path <- wow::init("RWA_potato", "agworkflows")

## Predict indigenous nutrient supply from soil data #

ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blup.RDS"))
supply <- readRDS(file.path(prj_path, "data/intermediate/compiled_potato_fertiliser_trial_calculated_supply_afterlmer_sqrttf.RDS"))

## not clear where these came from
sdt <- readRDS(file.path(prj_path, "data/intermediate/Soil_PointData_trial.RDS"))
sdt2 <- readRDS(file.path(prj_path, "data/intermediate/compiled_potato_fertiliser_trial_soildata.RDS"))
tdt <- readRDS(file.path(prj_path, "data/intermediate/Topography_AEZ_trial.RDS"))

#Soil information for prediction model:
sdt <- sdt |>
  #dplyr::select(-ID) |>
  dplyr::rename(TLID = ID,
         lon = longitude,
         lat = latitude,
         province = NAME_1,
         district = NAME_2) |>
  #removing soil variables with unlikely predictive value:
  dplyr::select(-c(fe_top, fe_bottom, B_0_30, Cu_0_30, Mn_0_30))

sdt2 <- sdt2 |>
  dplyr::filter(TLID == "SATLRW475382409484") |>
  dplyr::select(-c(ID, wpg2_top, wpg2_bottom)) |>
  dplyr::select(-c(fe_top, fe_bottom, B_0_30, Cu_0_30, Mn_0_30)) |>
  dplyr::rename(lon = x, lat = y) |>
  dplyr::mutate(province = "Iburengerazuba", district = "Rubavu")

  sdt <- rbind(sdt, sdt2) |>
  dplyr::select(-c(lat, lon))

#Productivity class:
ds_ref <- ds |>
  #select treatments with high nutrient rates (Increased NPK for RS-PFR-1, NPK_all for IFDC, NPK11 for SA-VAL-1):
  dplyr::filter(N > 75, P > 30, K > 50) |>
  dplyr::group_by(expCode, TLID) |>
  dplyr::summarise(refY = median(TY)) |>
  dplyr::mutate(refY = cut(refY, c(-Inf, 10, 20, 30, 40, Inf), labels = c("Very low", "Low", "Medium", "High", "Very high")))

saveRDS(ds_ref, file.path(prj_path, "data/intermediate/ds_ref.RDS"))


#Topography and EAZ data:
tdt <- tdt |>
  dplyr::rename(TLID = ID,
         lon = longitude,
         lat = latitude,
         alt = altitude,
         AEZ = Names_AEZs,
         Province = NAME_1,
         District = NAME_2) |>
  dplyr::select(-c(AEZs_no, lat, lon, Province, District)) |>
  dplyr::mutate(AltClass2 = cut(alt, breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf)),
         #AEZ = ifelse(AEZ == "Central plateau", "Congo-Nile watershed divide", AEZ)
         )

INS <- supply |>
  #adding lats and lons and data source:
  dplyr::left_join(ds |> dplyr::select(TLID, lat, lon, expCode, season) |> unique(), by="TLID") |>
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

#Add predictors to INS dataset:
INS <- INS |> 
  dplyr::left_join(sdt, by="TLID") |>
  dplyr::left_join(ds_ref, by=c("TLID", "expCode")) |>
  dplyr::left_join(tdt, by="TLID") |>
  na.omit()

#INS$P_base_supply_bin <- as.factor(ifelse(INS$P_base_supply < 50, "limiting", "non-limiting"))
#INS$K_base_supply_bin <- as.factor(ifelse(INS$K_base_supply < 200, "limiting", "non-limiting"))

#create subset with predictors only:
ins <- INS |>
  dplyr::filter(expCode != "IFDC") |>
  dplyr::select(-c(TLID, lon, lat, expCode, season, Ya,
                   #season_AB, #season could be considered or dropped
                   #refY #reference yield class could be considered or dropped
                   ))



#RF_N_ranger <- ranger::ranger(formula = log(N_base_supply) ~ ., 
#                      data = subset(ins, select = -c(P_base_supply, K_base_supply)), 
#                      num.trees=1000)
#RF_P_ranger <- ranger::ranger(formula = log(P_base_supply) ~ ., 
#                      data = subset(ins, select = -c(N_base_supply, K_base_supply)), 
#                      num.trees=1000)
#RF_K_ranger <- ranger::ranger(formula = log(K_base_supply) ~ ., 
#                      data = subset(ins, select = -c(N_base_supply, P_base_supply)), 
#                      num.trees=1000)

#rmse <- function(obs, prd) sqrt(mean((obs-prd)^2))
#rmse(ins$N_base_supply, exp(predict(RF_N_ranger, ins)$predictions))
#rmse(ins$P_base_supply, exp(predict(RF_P_ranger, ins)$predictions))
#rmse(ins$K_base_supply, exp(predict(RF_K_ranger, ins)$predictions))


#Leave-one-out cross-validation ???
#prediction using ranger (much faster with minimal increase in RMSE):

prd <- lapply(INS$TLID, function(i) {
  
	ins_train <- subset(INS[INS$TLID != i,], select = -c(TLID, lon, lat, expCode, season, Ya))
	ins_valid <- subset(INS[INS$TLID == i,], select = -c(TLID, lon, lat, expCode, season, Ya))
  
	RF_N <- ranger::ranger(formula = log(N_base_supply) ~ ., 
                        data = subset(ins_train, select = -c(P_base_supply, K_base_supply)), 
                        num.trees=500)
  
	RF_P <- ranger::ranger(formula = log(P_base_supply) ~ ., 
                 data = subset(ins_train, select = -c(N_base_supply, K_base_supply)), 
                 num.trees=500)
  
	RF_K <- ranger::ranger(formula = log(K_base_supply) ~ ., 
                 data = subset(ins_train, select = -c(N_base_supply, P_base_supply)), 
                 num.trees=500)
  
	data.frame(TLID = i, N_pred = exp(predict(RF_N, ins_valid)$predictions),
                         P_pred = exp(predict(RF_P, ins_valid)$predictions),
                         K_pred = exp(predict(RF_K, ins_valid)$predictions))
 
})

preds <- do.call(rbind, prd)
 

saveRDS(preds, file.path(prj_path, "data/intermediate/LOOCV_predictions_NPK_base_supply_afterlmer.RDS"))

INS <- INS |> dplyr::left_join(preds, by="TLID")
saveRDS(INS, file.path(prj_path, "data/intermediate/INS.RDS"))




##RF alternative 

# set.seed(666)
# RF_N <- randomForest::randomForest(log(N_base_supply) ~ ., subset(ins, select = -c(P_base_supply, K_base_supply)), importance=TRUE, ntree=1000)
# RF_P <- randomForest::randomForest(log(P_base_supply) ~ ., subset(ins, select = -c(N_base_supply, K_base_supply)),importance=TRUE, ntree=1000)
# RF_K <- randomForest::randomForest(log(K_base_supply) ~ ., subset(ins, select = -c(N_base_supply, P_base_supply)), importance=TRUE, ntree=1000)

# rmse(ins$N_base_supply, exp(predict(RF_N, ins)))
# rmse(ins$P_base_supply, exp(predict(RF_P, ins)))
# rmse(ins$K_base_supply, exp(predict(RF_K, ins)))

# #variable importance plots:
# randomForest::varImpPlot(RF_N)
# randomForest::varImpPlot(RF_P)
# randomForest::varImpPlot(RF_K)

# #variance explained:
# RF_N$rsq[1000]
# RF_P$rsq[1000]
# RF_K$rsq[1000]

# #rmse
# sqrt(RF_N$mse[1000])
# sqrt(RF_P$mse[1000])
# sqrt(RF_K$mse[1000])

# #Leave-one-out cross-validation:
# preds <- NULL
# run <- 0

#for(i in unique(INS$TLID)){
#  print(paste0(round(run/length(unique(INS$TLID))*100), "% complete"))
#  
#  ins_train <- subset(INS[INS$TLID != i,], select = -c(TLID, lon, lat, expCode, season, Ya))
#  ins_valid <- subset(INS[INS$TLID == i,], select = -c(TLID, lon, lat, expCode, season, Ya))
#  
#  RF_N <- randomForest(log(N_base_supply) ~ ., subset(ins_train, select = -c(P_base_supply, K_base_supply)), 
#                       importance=TRUE, ntree=500, mtry = 15)
#  
#  RF_P <- randomForest(log(P_base_supply) ~ ., subset(ins_train, select = -c(N_base_supply, K_base_supply)),
#                       importance=TRUE, ntree=500, mtry = 15)
#  
#  RF_K <- randomForest(log(K_base_supply) ~ ., subset(ins_train, select = -c(N_base_supply, P_base_supply)),
#                       importance=TRUE, ntree=500, mtry = 15)
#  
#  preds <- rbind(preds, data.frame(TLID = i,
#                                   N_pred = exp(predict(RF_N, ins_valid)),
#                                   P_pred = exp(predict(RF_P, ins_valid)),
#                                   K_pred = exp(predict(RF_K, ins_valid))))
#  run <- run + 1
#  
#}


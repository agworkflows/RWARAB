### SETUP
prj_path <- wow::init("RWA_potato", "agworkflows")

# Alternative: Predict yield directly with Random Forest
ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blup.RDS"))
ds_ref <- readRDS(file.path(prj_path, "/data/intermediate/ds_ref.RDS"))
INS <- readRDS(file.path(prj_path, "data/intermediate/INS.RDS"))

sdt <- readRDS(file.path(prj_path, "data/intermediate/Soil_PointData_trial.RDS"))
#sdt2 <- readRDS(file.path(prj_path, "data/intermediate/compiled_potato_fertiliser_trial_soildata.RDS"))
tdt <- readRDS(file.path(prj_path, "data/intermediate/Topography_AEZ_trial.RDS"))

colnames(sdt)[colnames(sdt) == "ID"] <- "TLID"

colnames(tdt)[colnames(tdt) == "ID"] <- "TLID"
tdt <- tdt[, c("TLID", "altitude", "slope", "TPI", "TRI")]


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
  plyr::join(sdt, by="TLID") |>
  plyr::join(ds_ref, by="TLID") |>
  plyr::join(tdt, by="TLID") |>
  na.omit()

dsp <- dsp[, -match(c("NAME_1", "expCode"), names(dsp))] 

##First model that includes district and refY
#RF0 <- randomForest::randomForest(dY ~ ., dsp, importance=TRUE, ntree=200)
#sqrt(RF0$mse[length(RF0$mse)])
#RF0

##Simpler model without district and refY
#RF1 <- randomForest::randomForest(dY ~ ., subset(dsp, select=-c(refY, NAME_2)), importance=TRUE, ntree=200)
#sqrt(RF1$mse[length(RF1$mse)])
#RF1

#Obtain yield estimates using LOOCV:

predRFs <- lapply(unique(dsp$TLID), function(i) {
  
  dsp_train <- dsp[dsp$TLID != i,]
  dsp_valid <- dsp[dsp$TLID == i,]
  
  #RF0 <- randomForest(dY ~ ., dsp_train, ntree=500)
  #RF1 <- randomForest(dY ~ ., subset(dsp_train, select = -c(district, refY)), ntree=500)
  
  RF0 <- ranger::ranger(formula = dY ~ ., data = dsp_train, num.trees = 200) 
  RF1 <- ranger::ranger(formula = dY ~ ., data = subset(dsp_train, select = -c(NAME_2, refY)), num.trees = 200) 

  data.frame(TLID = i,
            subset(dsp_valid, select=c(N, P, K, dY)),
            #dYp0 = predict(RF0, dsp_valid),
            #dYp1 = predict(RF1, dsp_valid)
            dYp0 = predict(RF0, dsp_valid)$predictions,
            dYp1 = predict(RF1, dsp_valid)$predictions)
})

predRFs <- do.call(rbind, predRFs)
saveRDS(predRFs, file.path(prj_path, "/data/intermediate/predRFs.RDS"))

dsp <- ds |>
  plyr::join(sdt, by="TLID") |>
  plyr::join(ds_ref, by=c("TLID", "expCode")) |>
  plyr::join(tdt, by="TLID") |>
  dplyr::select(-c(expCode, FDID, lat, lon, season, plantingDate, harvestDate, treat, N100:K100, TY))


set.seed(999)
s <- sort(sample(unique(INS[INS$expCode != "IFDC",]$TLID), 20))

leafrat <- 0.17
stemrat <- 0.14
qmod <- Rquefts::quefts(crop=Rquefts::quefts_crop("potato"))

# why not use all combinations?
#fr <- grid.expand(N=0:180, P=0:50, K=0:90)
fr <- data.frame(N=0:180, P=22, K= 42)
fr <- rbind(fr, data.frame(N=51, P=0:50, K=42))
fr <- rbind(fr, data.frame(N=51, P=22, K=0:90))

sp <- INS[, c("TLID", "N_pred", "P_pred", "K_pred", "refY")]
sp$refY <- as.integer(sp$refY) * 10 * 1000 * 0.21

n <- length(s)
qd <- data.frame(TLID=rep(s, each=nrow(fr)), N=rep(fr$N, n), P=rep(fr$P, n), K=rep(fr$K, n))
qd <- merge(sp, qd, "TLID")

qd$yp = Rquefts::batch(qmod, qd[,c("N_pred", "P_pred", "K_pred")], qd[,c("N", "P", "K")], 
		qd$refY, leafrat, stemrat) / 210

pfun <- function(tlid) {
	dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) |> na.omit()
	dsp_prdct <- dsp[dsp$TLID == tlid,] |>
	dplyr::select(-c(N, P, K, blup)) |>
	unique() |>
	dplyr::cross_join(fr)
	RF0 <- ranger::ranger(formula = blup ~ ., data = dsp_train, num.trees = 200) 
	predict(RF0, dsp_prdct)$predictions
}  

res <- lapply(s, pfun)

qd <- qd[order(qd$TLID, qd$N), ]
qd$ypRF <- do.call(c, res)

saveRDS(qd, file.path(prj_path, "/data/intermediate/qd.RDS"))



require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lmer)
require(lmerTest)
require(MuMIn)
require(ggpmisc)
require(foreach)
require(doParallel)
require(sf)

#TODO: indicate functions to source

#######################################
# 1. Get the potato data from SAnDMan #
#######################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_ONA_data.R")
#downloading the data
# wd <- "D:/workspace/SAnDMan"
# setwd(wd)
# creds <- scan(paste0(wd, "/pws.txt"), what = "character")
user <- creds[1]
pw   <- creds[2]

#get the list of all datasets of user...
dss <- findONAdatasets(user = user, pw = pw)

#download and decompose the assign field/trial/plot data:
id <- dss[dss$id_string == "Assign_FDTLPO",]$id
ad <- getONAdata(user = user, pw = pw, id = id) 
ad <- decomposeONAdata(ad)

#get the field identifiers
af <- ad[[1]] %>%
  filter(grepl("FD", entity)) %>%
  dplyr::select(FDID2_new, FD_name_new, FD_owner, HHID, lat, lon) %>%
  rename(FDID2 = FDID2_new,
         FD_name = FD_name_new)

#get the trial identifiers
at <- ad[[3]] %>%
  join(ad[[1]] %>% dplyr::select(L1, entity, season, plantingDate, expCode)) %>%
  filter(grepl("TL", entity),
         L2 == "trial") %>%
  dplyr::select(TLID2_new, TL_name_new, season, plantingDate, expCode) %>%
  mutate(plantingDate = as.Date(plantingDate, format="%Y-%m-%d")) %>%
  rename(TLID2 = TLID2_new,
         TL_name = TL_name_new)

#download and decompose the potato plot level data:
id <- dss[dss$id_string == "Measure_Potato_PO",]$id
pd <- getONAdata(user = user, pw = pw, id = id) 
pd <- decomposeONAdata(pd)

#get the potato plot yield data and merge with trial and field identifiers:
ds1 <- pd[[3]] %>% #plot level data
  filter(!is.na(tubersFW) | !is.na(tubersMarketableFW)) %>%
  left_join(pd[[1]] %>% dplyr::select(L1, projectCode, FDID2, TLID2, today, start)) %>% #field level data
  mutate(harvestDate = as.Date(today, format="%Y-%m-%d"),
         start = as.POSIXct(gsub("\\+.*","", start), format="%Y-%m-%dT%H:%M:%S", tz="UTC")) %>%
  dplyr::select(projectCode, FDID2, TLID2, POID2, POID2_label, start, harvestDate, plotLength, plotWidth, nrPlants, tubersFW, tubersMarketableFW) %>%
  left_join(af) %>%
  left_join(at)

#extracting treatment from label
ds1 <- ds1 %>% 
  mutate(treat = sub("_[^_]+$", "", POID2_label),
         treat = gsub("_rep1", "", treat),
         treat = gsub("_rep2", "", treat),
         treat = gsub("_repA", "", treat),
         treat = gsub("_repB", "", treat),
         plotSize = as.numeric(plotLength) * as.numeric(plotWidth),
         tubersFW = as.numeric(tubersFW),
         tubersMarketableFW = as.numeric(tubersMarketableFW),
         plantingDate = as.Date(plantingDate, format = "%Y-%m-%d")) %>%
  filter(treat != "",
         expCode != "RS-PLR-1") #removing lime trials without varying NPK rates

#correcting season entries
ds1 <- ds1 %>%
  mutate(season = ifelse(season %in% c("2222B", "B2022", "2022b", "2020B", "2022"), "2022B", season),
         season = ifelse(season == "2020A", "2021A", season))

#correcting plotsize and calculating yield
ds1 <- ds1 %>% 
  mutate(plotSize = abs(plotSize),
         plotSize = ifelse(plotSize>500, plotSize/100, plotSize),
         plotSize = ifelse(plotSize>50, plotSize/10, plotSize)) %>%
  group_by(TLID2) %>%
  mutate(plotSize = median(plotSize)) %>%
  group_by(POID2) %>%
  filter(start == max(start)) %>% #only taking the last observation per POID
  mutate(n = n()) %>%
  filter(n == 1) %>% #drop all plots that have more than one yield observation
  ungroup() %>%
  mutate(TY = ifelse(is.na(tubersFW), tubersMarketableFW, tubersFW)/plotSize*10,
         TY = ifelse(POID2 == "SAPORW756633027058", TY/10, TY)) %>% #correcting entry without decimal separator
  left_join(read.csv("D:/workspace/SAnDMan/potato_trials_nutrient_rates.csv")) %>%
  rename(FDID = FDID2,
         TLID = TLID2) %>%
  dplyr::select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY) %>%
  as.data.frame()

#adding and replacing plant and harvest dates from records by RAB staff for RS-PFR-1:
phd <- read.csv("D:/workspace/RwaSIS/potato_trials_with_yield_data_2023-04-14_RwaSIS_PFR.csv") %>%
  mutate(plantingDate_FB = as.Date(Planting.date, format="%d/%m/%Y"),
         harvestDate_FB = as.Date(Harvest.date, format="%d/%m/%Y")) %>%
  rename(FDID = FDID2,
         TLID = TLID2) %>%
  dplyr::select(FDID, TLID, plantingDate_FB, harvestDate_FB)
  
ds1 <- ds1 %>% left_join(phd) %>%
  mutate(plantingDate = if_else(is.na(plantingDate_FB), plantingDate, plantingDate_FB),
         harvestDate = if_else(is.na(harvestDate_FB), harvestDate, harvestDate_FB)) %>%
  dplyr::select(-c(plantingDate_FB, harvestDate_FB)) %>%
  #replace impossible harvest dates by the planting date + median duration of trials
  mutate(harvestDate = if_else(is.na(plantingDate) | (as.numeric(harvestDate - plantingDate) < 150 & as.numeric(harvestDate - plantingDate) > 90), harvestDate, plantingDate + median(harvestDate - plantingDate)))
  
#########################################
# 2. Preparing the RwaSIS season 1 data #
#########################################

ds2 <- read.csv("D:/workspace/RwaSIS/rwasis-potato-fertiliser-all-data.csv")

ds2 <- ds2 %>%
  filter(ds2$season == "2022A") %>% #removing the SAnDMan data which are 2022B data in ds1 for RS_PFR-1
  rename(lon = gps_lon,
         lat = gps_lat,
         treat = treatment,
         N = nfert_kgha,
         P = pfert_kgha,
         K = kfert_kgha,
         TY = yield_tha,
         FDID = farm_id) %>%
  mutate(expCode = "RS-PFR-1",
         FDID = paste0("RwaSIS_", FDID),
         TLID = FDID,
         plantingDate = as.Date(planting_date, format="%Y-%m-%d"),
         harvestDate = as.Date(harvest_date, format="%Y-%m-%d")) %>%
  mutate(harvestDate = if_else(is.na(plantingDate) | (as.numeric(harvestDate - plantingDate) < 150 & as.numeric(harvestDate - plantingDate) > 90), harvestDate, plantingDate + median(harvestDate - plantingDate))) %>%
  dplyr::select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY)

#####################################
# 3. Preparing the IFDC potato data #
#####################################

ds3 <- read.csv("D:/workspace/RwaSIS/IFDC_Rwanda potato 2014B season data subset.csv")
ds3_nutrates <- read.csv("D:/workspace/RwaSIS/IFDC_Rwanda potato 2014B season treat nutrates.csv")
ds3 <- ds3 %>%
  gather(treat, TY, control:all_redK) %>%
  mutate(season = "2014B",
         expCode = "IFDC",
         FDID = paste0("IFDC_", siteNr),
         TLID = FDID, 
         plantingDate = NA,
         harvestDate = NA) %>%
  join(ds3_nutrates) %>%
  dplyr:: select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY)

ds3[ds3$TLID == "IFDC_3",]$lon <- ds3[ds3$TLID == "IFDC_3",]$lon - 1 #wrong GPS entry 

#########################################
# 4. Fit lmer to eliminate random error #
#########################################

ds <- rbind(ds1, ds2, ds3)
setwd("D:/workspace/RwaSIS/")
#ds <- read.csv("compiled_potato_fertiliser_trial_data.csv")
write.csv(ds, "compiled_potato_fertiliser_trial_data.csv", row.names = FALSE)
saveRDS(ds, "compiled_potato_fertiliser_trial_data.RDS")

#plot showing yield ranges by experiment and season:
ds %>%
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

#plot showing variation in yield as affected by NPK rate by experiment and season:
ds %>%
  gather(nutrient, rate, N:K) %>%
  mutate(nutrient = factor(nutrient, levels=c("N", "P", "K"))) %>%
  ggplot(aes(rate, TY)) + 
  geom_point(alpha=.33, shape=16) +
  facet_grid(nutrient ~ expCode+season) + 
  xlab("\nFertilizer nutrient application rate [kg/ha]") +
  ylab("Observed tuber yield [kg/ha]\n") +
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))

#map with trial locations:
rwshp0 <- st_read("D:/workspace/shapefiles/rwa_adm_2006_nisr_wgs1984_20181002_shp/rwa_adm0_2006_NISR_WGS1984_20181002.shp")
rwshp1 <- st_read("D:/workspace/shapefiles/rwa_adm_2006_nisr_wgs1984_20181002_shp/rwa_adm1_2006_NISR_WGS1984_20181002.shp")
rwshp2 <- st_read("D:/workspace/shapefiles/rwa_adm_2006_nisr_wgs1984_20181002_shp/rwa_adm2_2006_NISR_WGS1984_20181002.shp")
rwshp3 <- st_read("D:/workspace/shapefiles/rwa_adm_2006_nisr_wgs1984_20181002_shp/rwa_adm3_2006_NISR_WGS1984_20181002.shp")
rwshp4 <- st_read("D:/workspace/shapefiles/rwa_adm_2006_nisr_wgs1984_20181002_shp/rwa_adm4_2006_NISR_WGS1984_20181002.shp")
rwlake <- st_read("D:/workspace/shapefiles/rwalakes/RWA_Lakes_NISR.shp")
rwAEZ <- st_read("AEZ_DEM_Dissolve.shp")
rwAEZ <- rwAEZ[rwAEZ$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]

ggplot()+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, aes(fill = Names_AEZs)) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  #geom_sf(data = rwshp3[rwshp3$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.2, color = "white", fill=NA) + 
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
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

#########################################
# 4. Fit lmer to eliminate random error #
#########################################

#create variables to deal with scale issues:
ds$N100 <- ds$N/100
ds$P100 <- ds$P/100
ds$K100 <- ds$K/100

#base model with independent parabolic response curves, fixed season effect, and random TL intercepts:
fit0 <- lmer(sqrt(TY) ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + season + (1|TLID), data=ds)
anova(fit0)
r.squaredGLMM(fit0)

#updated model allowing fixed two- and three-way interactions between N, P and K: 
fit1 <- update(fit0, . ~ . + N100:P100 + N100:K100 + P100:K100 + N100:P100:K100)
anova(fit1, fit0)
anova(fit1)
r.squaredGLMM(fit1)

#updated model adding random slopes:
fit2 <- update(fit1, . ~ . +(0 + N100|TLID) +(0 + P100|TLID) +(0 + K100|TLID))
anova(fit2, fit1)
anova(fit2)
r.squaredGLMM(fit2) 

ds$blup <- predict(fit2, ds)**2

#plot showing relationship between observations (with random error) and BLUPs (without random error)
ggplot(ds, aes(x = blup, y = TY)) + 
  geom_point(alpha=.33, shape=16) +
  geom_abline(intercept = 0, slope = 1) +
  stat_poly_line(formula = y ~ x, se = F) +
  stat_poly_eq(use_label(c("eq", "R2")),
               formula = y ~ x, size = 6)+
  xlab("\nBLUP tuber yield [t/ha]") +
  ylab("Observed tuber yield [t/ha]\n") +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14))

#plot illustrating that the elimination of random error results in more meaningful structure in yield response:
ds %>%
  gather(variable, value, c(TY, blup)) %>%
  group_by(TLID, variable) %>%
  mutate(refY = ifelse(N > 75 & P > 30 & K > 50, value, NA),
         refY = mean(refY, na.rm=TRUE),
         dY = refY - value,
         variable = factor(variable, levels=c("TY", "blup")),
         variable = mapvalues(variable,
                              from = c("TY", "blup"),
                              to = c("Raw observations", "BLUPs"))) %>%
  filter(!(N > 75 & P > 30 & K > 50)) %>%
  ggplot(aes(x = refY, y = dY)) + 
  #geom_point(alpha=.33, shape=16) + 
  geom_point(aes(shape = variable)) +
  scale_shape_manual(values = c(3, 16)) +
  facet_wrap(~variable) + 
  #facet_grid(expCode ~ variable) +
  geom_hline(yintercept = 0) +
  xlab("\nYield in reference treatment [t/ha]") +
  ylab("Yield difference relative to reference treatment [t/ha]\n") +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"),
        legend.position = "none")

ds %>%
  filter(TLID %in% sample(unique(ds$TLID), 12, replace = F)) %>%
  ggplot(aes(x = treat, y = blup)) +
  geom_point(size = 3) + 
  geom_point(aes(y = TY), shape = 3, size = 3) +
  facet_wrap(~TLID, scales = "free_x") + 
  ylab("Potato tuber yield [t/ha]\n") +
  theme_gray()+
  theme(axis.title.y = element_text(size = 14, face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))


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


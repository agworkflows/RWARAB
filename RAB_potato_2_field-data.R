############## START SETUP
prj_path <- wow::init("RWA_potato", "agworkflows")


### get the data
# SAnDMan data (created with RAB_potato_1_ONA.R)
ds1 <- readRDS(file.path(prj_path, "data/intermediate/SAnDMan_potato_fieldData.RDS"))

### the data used in this script are are not public 
### you need to put these files in the "$rootdir$/RAB/data/raw" folder
# RwaSIS season 1 data

raw_path <- file.path(prj_path, "data/raw")
ds2 <- readRDS(file.path(raw_path, "RwaSIS_potato_2022A_fieldData.RDS"))

#ds2 <- read.csv(file.path(raw_path, "rwasis-potato-fertiliser-all-data.csv"))

# processed IFDC potato data
#ds3 <- readRDS(file.path(raw_path, "IFDC_potato_2014B_fieldData.RDS"))
ds3 <- read.csv(file.path(raw_path, "IFDC_Rwanda potato 2014B season data subset.csv"))
ds3_nutrates <- read.csv(file.path(raw_path, "IFDC_Rwanda potato 2014B season treat nutrates.csv"))

nut_rates <- read.csv(file.path(raw_path, "RwaSIS_potato_trials_nutrient_rates.csv"))
phd <- read.csv(file.path(raw_path, "RwaSIS_potato_trials_with_yield_data_2023-04-14_RwaSIS_PFR.csv"))


#correcting season entries
ds1 <- ds1 |>
  dplyr::mutate(season = ifelse(season %in% c("2222B", "B2022", "2022b", "2020B", "2022"), "2022B", season),
         season = ifelse(season == "2020A", "2021A", season))

#correcting plotsize and calculating yield
ds1 <- ds1 |> 
  dplyr::mutate(plotSize = abs(plotSize),
         plotSize = ifelse(plotSize>500, plotSize/100, plotSize),
         plotSize = ifelse(plotSize>50, plotSize/10, plotSize)) |>
  dplyr::group_by(TLID2) |>
  dplyr::mutate(plotSize = median(plotSize)) |>
  dplyr::group_by(POID2) |>
  #only taking the last observation per POID
  dplyr::filter(start == max(start)) |> 
  dplyr::mutate(n = dplyr::n()) |>
  #drop all plots that have more than one yield observation
  dplyr::filter(n == 1) |> 
  dplyr::ungroup() |>
  dplyr::mutate(TY = ifelse(is.na(tubersFW), tubersMarketableFW, tubersFW)/plotSize*10,
  #correcting entry without decimal separator
		TY = ifelse(POID2 == "SAPORW756633027058", TY/10, TY)) |>
  dplyr::left_join(nut_rates) |>
  dplyr::rename(FDID = FDID2, TLID = TLID2) |>
  dplyr::select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY) |>
  as.data.frame()

#adding and replacing plant and harvest dates from records by RAB staff for RS-PFR-1:
phd <- phd |>
  dplyr::mutate(plantingDate_FB = as.Date(Planting.date, format="%d/%m/%Y"),
         harvestDate_FB = as.Date(Harvest.date, format="%d/%m/%Y")) |>
  dplyr::rename(FDID = FDID2, TLID = TLID2) |>
  dplyr::select(FDID, TLID, plantingDate_FB, harvestDate_FB)
  
ds1 <- ds1 |> dplyr::left_join(phd) |>
  dplyr::mutate(plantingDate = dplyr::if_else(is.na(plantingDate_FB), plantingDate, plantingDate_FB),
         harvestDate = dplyr::if_else(is.na(harvestDate_FB), harvestDate, harvestDate_FB)) |>
  dplyr::select(-c(plantingDate_FB, harvestDate_FB)) |>
  #replace impossible harvest dates by the planting date + median duration of trials
  dplyr::mutate(harvestDate = dplyr::if_else(is.na(plantingDate) | (as.numeric(harvestDate - plantingDate) < 150 & as.numeric(harvestDate - plantingDate) > 90), harvestDate, plantingDate + median(harvestDate - plantingDate)))
  
#########################################
# 2. Preparing the RwaSIS season 1 data #
#########################################

ds2 <- ds2 |>
  #removing the SAnDMan data which are 2022B data in ds1 for RS_PFR-1
  dplyr::filter(ds2$season == "2022A") |> 
  dplyr::rename(lon = gps_lon,
         lat = gps_lat,
         treat = treatment,
         N = nfert_kgha,
         P = pfert_kgha,
         K = kfert_kgha,
         TY = yield_tha,
         FDID = farm_id) |>
  dplyr::mutate(expCode = "RS-PFR-1",
         FDID = paste0("RwaSIS_", FDID),
         TLID = FDID,
         plantingDate = as.Date(planting_date, format="%Y-%m-%d"),
         harvestDate = as.Date(harvest_date, format="%Y-%m-%d")) |>
  dplyr::mutate(harvestDate = dplyr::if_else(is.na(plantingDate) | (as.numeric(harvestDate - plantingDate) < 150 & as.numeric(harvestDate - plantingDate) > 90), harvestDate, plantingDate + median(harvestDate - plantingDate))) |>
  dplyr::select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY)

#####################################
# 3. Preparing the IFDC potato data #
#####################################

ds3 <- ds3 |>
  tidyr::gather(treat, TY, control:all_redK) |>
  dplyr::mutate(season = "2014B",
         expCode = "IFDC",
         FDID = paste0("IFDC_", siteNr),
         TLID = FDID, 
         plantingDate = NA,
         harvestDate = NA) |>
  dplyr::left_join(ds3_nutrates) |>
  dplyr:: select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY)

ds3[ds3$TLID == "IFDC_3",]$lon <- ds3[ds3$TLID == "IFDC_3",]$lon - 1 #wrong GPS entry 


### 4. Combining all datasets ###

ds <- rbind(ds1, ds2, ds3)
saveRDS(ds, file.path(prj_path, "data/intermediate/compiled_potato_fertiliser_trial_data.RDS"))


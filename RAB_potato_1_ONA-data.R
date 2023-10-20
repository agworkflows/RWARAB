
############## START SETUP
prj_path <- agvise::setup_project("RWA_potato", "agworkflows")
################ SETUP END

fraw <- file.path(prj_path, "raw/SAnDMan.RDS")
if (!file.exists(fraw)) {
	# pwds <- "D:/workspace/SAnDMan/pws.txt"
	creds <- scan(pwds, what = "character")
	user <- creds[1]
	pw   <- creds[2]
	#get the list of all datasets of user...
	#dss <- agvise::findONAdata(user = user, pw = pw)
	#download and decompose the assign field/trial/plot data:
	id <- dss[dss$id_string == "Assign_FDTLPO",]$id
	ad <- agvise::getONAdata(user = user, pw = pw, id = id) 
	saveRDS(ad, fraw)
} 

ad <- readRDS(fraw)
ad <- agvise::decomposeONAdata(ad)


#get the field identifiers
af <- ad[[1]] |>
  filter(grepl("FD", entity)) |>
  dplyr::select(FDID2_new, FD_name_new, FD_owner, HHID, lat, lon) |>
  rename(FDID2 = FDID2_new,
         FD_name = FD_name_new)

#get the trial identifiers
at <- ad[[3]] |>
  join(ad[[1]] |> dplyr::select(L1, entity, season, plantingDate, expCode)) |>
  filter(grepl("TL", entity),
         L2 == "trial") |>
  dplyr::select(TLID2_new, TL_name_new, season, plantingDate, expCode) |>
  mutate(plantingDate = as.Date(plantingDate, format="%Y-%m-%d")) |>
  rename(TLID2 = TLID2_new,
         TL_name = TL_name_new)

#download and decompose the potato plot level data:
id <- dss[dss$id_string == "Measure_Potato_PO",]$id
pd <- getONAdata(user = user, pw = pw, id = id) 
pd <- decomposeONAdata(pd)

#get the potato plot yield data and merge with trial and field identifiers:
ds1 <- pd[[3]] |> #plot level data
  filter(!is.na(tubersFW) | !is.na(tubersMarketableFW)) |>
  left_join(pd[[1]] |> dplyr::select(L1, projectCode, FDID2, TLID2, today, start)) |> #field level data
  mutate(harvestDate = as.Date(today, format="%Y-%m-%d"),
         start = as.POSIXct(gsub("\\+.*","", start), format="%Y-%m-%dT%H:%M:%S", tz="UTC")) |>
  dplyr::select(projectCode, FDID2, TLID2, POID2, POID2_label, start, harvestDate, plotLength, plotWidth, nrPlants, tubersFW, tubersMarketableFW) |>
  left_join(af) |>
  left_join(at)

outfile <- file.path(prj_path, "intermediate/SAnDMan_potato_fieldData.RDS"))
saveRDS(ds1, outfile)


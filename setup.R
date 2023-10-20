
## add a rootdir for each computer that is used
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { 
	rootdir <- "d:/agwise"
} else { 
	# cglabs
	rootdir <- "~/agwise"
}
project <- "RWA_potato"

### do not edit
prj_path <- file.path(rootdir, project)
remotes::install_github("reagro/agvise", quiet=TRUE)
agvise::setup_folders(prj_path) 


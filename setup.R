## each agvise workflow must have a script like this

## add a rootdir for each computer that is used
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { 
	rootdir <- "d:/agwise"
} else if (system('hostname', TRUE) == "XYZ") { 
	rootdir <- "c:/workspace/agwise"
} else { 
	# cglabs
	rootdir <- "~/agwise"
}

project <- "RWA_potato"


### do not edit below this line
##geodata_path(geodata_dir)
prj_path <- file.path(rootdir, project)

remotes::install_github("reagro/agvise", quiet=TRUE)

#agvise::install_pkgs(c("randomForest"))


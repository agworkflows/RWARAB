## each agvise workflow must have a script like this

## add a rootdir for each computer that is used
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { 
	rootdir <- "d:/agwise"
	geodata_dir <- rootdir
} else if (system('hostname', TRUE) == "XYZ") { 
	rootdir <- "c:/workspace/agwise"
	geodata_dir <- rootdir
} else { 
	# cglabs
	rootdir <- "~/agwise"
	geodata_dir <- rootdir
}

project <- "RWA_potato"

#must have this line 
prj_path <- file.path(rootdir, project)

#set the default geodata path
geodata::geodata_path(geodata_dir)

#get the last version of agvise 
remotes::install_github("reagro/agvise", quiet=TRUE)

#make sure required packages are installed
agvise::install_pkgs(c("randomForest"))


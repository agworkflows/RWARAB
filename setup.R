## each agvise workflow must have a script like this

## add a rootdir for each computer that is used
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { 
	rootdir <- "d:/agwise"
	geodata_dir <- rootdir
} else if (system('hostname', TRUE) == "XYZ") { 
	rootdir <- "c:/workspace/agwise"
	geodata_dir <- rootdir
} else { 
	rootdir <- "."
	geodata_dir <- "."
}

project <- "RWA_potato"

#must have this line 
prj_path <- file.path(rootdir, project)

#make sure required packages are installed
wow::install_pkgs(c("geodata", "ranger", "randomForest", "lme4", "dplyr"))
wow::install_pkgs("Rquefts", update=TRUE)

#set the default geodata path
geodata::geodata_path(geodata_dir)



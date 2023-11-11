## each agvise workflow must have a script like this

initialize <- function(...) {

	## add a rootdir for each computer that is used
	if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { 
		rootdir <- "d:/agwise"
	} else if (system('hostname', TRUE) == "XYZ") { 
		rootdir <- "c:/workspace/agwise"
	} else { 
		rootdir <- "."
	}

	project <- "RWA_potato"

	#must have this line 
	prj_path <- file.path(rootdir, project)
	dir.create(prj_path, FALSE, TRUE)

	#make sure required packages are installed
	wow::install_pkgs(c("geodata", "ranger", "randomForest", "lme4", "dplyr"))
	wow::install_pkgs("Rquefts", update=TRUE)

	prj_path 
}

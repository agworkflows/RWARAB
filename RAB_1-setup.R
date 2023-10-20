
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { 
	rootdir <- "d:/agwise"
} else { 
	# cglabs
	rootdir <- "~/agwise"
}

project <- "RWARAB"
prj_path <- file.path(rootdir, project)

remotes::install_github("reagro/agvise")
agvise::create_project(prj_path) 


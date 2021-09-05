package_load = function(load_basic = TRUE){
	# Package names
	basic_packages <- c("R6", "DBI", "arules", "arulesViz", "fcaR", "BiocManager", "hasseDiagram", "colorspace",
						"tidyverse", "shiny", "shinyjs", "shinybusy","shinythemes", "gotop", "DT")
	all_packages <- c(basic_packages,"RMariaDB", "RMySQL", "pool", "correlationfunnel", "wordcloud", "wordcloud2", "tm","shinymanager", 
					  "textclean", "anytime", "stringr", "stringi", "jsonlite", "RJSONIO", "tictoc", "RColorBrewer")
	
	# Install packages not yet installed
	installed_packages <- all_packages %in% rownames(installed.packages())
	if(any(installed_packages == FALSE)) install.packages(all_packages[!installed_packages])
	if(isFALSE("Rgraphviz" %in% rownames(installed.packages()))) BiocManager::install("Rgraphviz") 
	
	# Packages loading
	if(isTRUE(load_basic)){
		packages <- basic_packages
	}
	else{
		packages <- all_packages
	}
	
	invisible(lapply(packages, library, character.only = TRUE))
}
suppressMessages(package_load())
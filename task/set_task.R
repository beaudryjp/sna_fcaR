script_name <- "task.R"
get_os <- function(){
	sysinf <- Sys.info()
	if (!is.null(sysinf)){
		os <- sysinf['sysname']
		if (os == 'Darwin')
			os <- "osx"
	} else { ## mystery machine
		os <- .Platform$OS.type
		if (grepl("^darwin", R.version$os))
			os <- "osx"
		if (grepl("linux-gnu", R.version$os))
			os <- "linux"
	}
	tolower(os)
}

if(get_os() == "windows"){
	install.packages("taskscheduleR")
	library(taskscheduleR)
	myscript <- paste0("C:/Users/jeanp/shiny/app/task/", script_name)
	taskscheduler_create(taskname = script_name, rscript = myscript, schedule = "MINUTE", starttime = "00:00", modifier = (12 * 60))
} else if (get_os() == "linux"){
	install.packages("cronR")
	library(cronR)
	myscript <- paste0("/srv/shiny-server/", script_name)
	cron_add(cmd, frequency = '0 */12 * * *', id = script_name, description = 'Every 12h')   
}
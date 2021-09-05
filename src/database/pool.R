Database <- R6Class(
	
	classname = "Database",
	
	private = list(
		
		connection = NULL,
		result = NULL,
		file = NULL,
		database = NULL,
		
		set_config = function(data){
			file <- ""
			db <- ""
			if (data == "twitter") {
				file <- "conf/sna_twitter.cnf"
				db <- "sna_twitter"
			}
			else {
				file <- "conf/sna_reddit.cnf"
				db <- "sna_reddit"
			}
			private$file <- file
			private$database <- db
		}
	),
	
	public = list(
		
		initialize = function(data) {
			stopifnot(is.character(data))
			if (data != "reddit" && data != "twitter") stop()
			
			private$set_config(data)
			private$connection <- pool::dbPool(RMariaDB::MariaDB(), default.file = private$file, group = private$database)
		},
		
		select = function(query) {
			result <- dbGetQuery(private$connection, query)
			private$close()
			return(result)
		},
		
		insert = function(query) {
			dbExecute(private$connection, query)
			private$close()
		},
		
		exists = function(query){
			result <- nrow(self$select(query)) > 0
			private$close()
			return(result)
		},
		
		close = function(){
			pool::poolClose(private$connection)
		}
		
	)
	
)
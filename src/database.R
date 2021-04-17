Database <- R6Class(
	
	classname = "Database",
	
	private = list(
		
		connection = NULL,
		result = NULL,
		file = NULL,
		database = NULL,
		
		clear_result = function() {
			dbClearResult(private$result)
		},
		
		disconnect = function() {
			dbDisconnect(private$connection)
		}
		
	),
	
	public = list(
		
		initialize = function(data) {
			stopifnot(is.character(data))
			if (data != "reddit" && data != "twitter") stop()
			f <- ""
			d <- ""
			if (data == "twitter") {
				f <- paste(c(getwd(), "/conf/", "sna_twitter.cnf"), collapse = "")
				d <- "sna_twitter"
			}
			else {
				f <- paste(c(getwd(), "/conf/", "sna_reddit.cnf"), collapse = "")
				d <- "sna_reddit"
			}
			private$file <- f
			private$database <- d
			private$connection <- dbConnect(RMariaDB::MariaDB(),
											default.file = private$file,
											group = private$database)
		},
		
		select = function(query) {
			private$result <- dbSendQuery(private$connection, query)
			rows <- dbFetch(private$result)
			private$clear_result()
			private$disconnect()
			rows
		},
		
		insert = function(query) {
			private$result <- dbSendQuery(private$connection, query)
			private$clear_result()
			private$disconnect()
		}
		
	)
	
)
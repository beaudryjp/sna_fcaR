SidebarSearch <- function(id, data){
	field <- paste0(id, "_")
	
	sidebarPanel(
		h3("Search Parameters"),
		hr(),
		selectizeInput(inputId = paste0(field, "search"), label = data$label, choices = data$choices, options = list(maxOptions = SHINY_SUBREDDIT_MAX_OPT)),
		radioButtons(paste0(field, "filter_by_date"), "Filter by date:",
					 c("Date in which the post was added to the DB" = "date_added",
					   "Date in which the post was created" = "date_created"), selected = "date_created"),
		conditionalPanel(
			paste0("input.", paste0(field, "filter_by_date"), " == 'date_created'"),
			dateRangeInput(paste0(field, "date_created"), "Specify the date range:",
						   start = DPICK_1ST_DATE,
						   end = DPICK_2ND_DATE,
						   language = DPICK_LANG,
						   weekstart = DPICK_WEEKSTART,
						   format = DPICK_FORMAT)
		),
		conditionalPanel(
			paste0("input.", paste0(field, "filter_by_date"), " == 'date_added'"),
			#"input.filter_by_date == 'date_added'",
			dateRangeInput(paste0(field, "date_added"), "Specify the date range:",
						   start = DPICK_1ST_DATE,
						   end = DPICK_2ND_DATE,
						   language = DPICK_LANG,
						   weekstart = DPICK_WEEKSTART,
						   format = DPICK_FORMAT)
		),
		
		numericInput(paste0(field, "limit"), paste0("Number of posts to obtain: (min = ", DB_LIMIT_MIN, ", max = ", DB_LIMIT_MAX, ")"), 
					 DB_LIMIT_DEF, min = DB_LIMIT_MIN , max = DB_LIMIT_MAX),
		actionButton(paste0(field, "submit_search"), "Search data"), 
		width = SHINY_SIDEBAR_WIDTH
	)
}

SidebarAPI <- function(){
	sidebarPanel(
		radioButtons("get_data_api", "Select search type:",
					 c("Obtain top posts for each subreddits (slow)" = "post_auto_populate",
					   "Obtain posts by specifiying parameters" = "post_search_by"), selected = "post_search_by"),
		hr(),
		conditionalPanel(
			"input.get_data_api == 'post_auto_populate'",
			h3("Obtain top N posts from each subreddit"),
			hr(),
			selectizeInput(inputId = "select_subs", "Select a Subreddit", choices = NULL, multiple = TRUE, options = list(maxOptions = SHINY_SUBREDDIT_MAX_OPT)),
			selectizeInput(inputId = "listing_populate", label = "Select Listing", choices = NULL),
			selectizeInput(inputId = "timeframe_populate", label = "Select Timeframe", choices = NULL),
			checkboxInput("include_comments_populate", "Obtain comments for each post (if true the process is slower)", FALSE),
			numericInput("limit_posts_populate", 
						 paste0("Number of posts to obtain: (min = ", POPULATE_LIMIT_MIN, ", max = ", POPULATE_LIMIT_MAX, ")"), 
						 POPULATE_LIMIT_DEF, min = POPULATE_LIMIT_MIN , max = POPULATE_LIMIT_MAX),
			actionButton("populate_get", "Get data")
		),
		conditionalPanel( 
			"input.get_data_api == 'post_search_by'",
			h3("Search Parameters"),
			hr(),
			selectizeInput(inputId = "subreddits_api", label = "Select Subreddit", choices = NULL, options = list(maxOptions = SHINY_SUBREDDIT_MAX_OPT)),
			selectizeInput(inputId = "listing", label = "Select Listing", choices = NULL),
			selectizeInput(inputId = "timeframe", label = "Select Timeframe", choices = NULL),
			numericInput("number_posts_api", paste0("Number of posts to obtain: (min = ", API_LIMIT_MIN, ", max = ", API_LIMIT_MAX, ")"), 
						 API_LIMIT_DEF, min = API_LIMIT_MIN , max = API_LIMIT_MAX),
			actionButton("submit_get", "Get data")
		),
		width = SHINY_SIDEBAR_WIDTH
	)
}
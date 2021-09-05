DataTab <- function(id){
	ns <- NS(id)
	field = paste0(id, "_")
	
	tabsetPanel( 
		id = "data_tabs",
		tabPanel(
			id = paste0(field, "tab_summary"), 
			
			"Data Information & Manipulation",
			
			h3("Summary"),
			tabPanel("Data Info", verbatimTextOutput(ns("summary"))),
			
			h3("Table"),
			tabPanel("Output", DT::dataTableOutput(ns("result_output"))),
			
			h3("Data normalization"),
			tabPanel("Normalization (code)", verbatimTextOutput(ns("data_normalization"))),
			
			h3("Binary matrix (first 50 rows)"),
			downloadButton(ns("download_matrix"),"Download complete matrix as csv"),
			tabPanel("Binary matrix", DT::dataTableOutput(ns("binary_matrix"))),
			
			PostSummaryPlots(id)
		),
		
		TextMiningTab(id)
	)
}

PostSummaryPlots <- function(id){
	field <- paste0(id, "_")
	if(id == SHINY_POST_ID){
		tabPanel(
			id = paste0(field, "plots"),
			
			"Plots",
			
			h3("Plot distrubtion of the parameters"),
			h4("Plot by upvote_ratio"),
			tabPanel("Plot", plotOutput(paste0(field, "plot_upvoteratio"))),
			
			h4("Plot by score"),
			tabPanel("Plot", plotOutput(paste0(field, "plot_score"))),
			
			
			h4("Plot by top domains"),
			tabPanel("Plot", plotOutput(paste0(field, "plot_domains"))),
			
			br(), br(), br(), br(), br(), br(),
			
			
			h4("Plot by top subreddits"),
			tabPanel("Plot", plotOutput(paste0(field, "plot_subreddits")))	
		)
		
	}
}

TextMiningTab <- function(id){
	field <- paste0(id, "_")
	
	if(id != SHINY_AWARD_ID){
		ns <- NS(id)
		
		tabPanel(
			id = paste0(field, "tab_tm"), 
			
			"Text Mining",
			
			h3("Unique users"),
			tabPanel("Unique users", verbatimTextOutput(ns("uniq_users"))),
			
			h3("Top posts by user"),
			tabPanel("Posts by user", DT::dataTableOutput(ns("top"))),
			
			h3("Top posts over the mean"),
			plotOutput(ns("top_overmean"), height = SHINY_PLOT_HEIGHT),
			
			h3("Wordcloud with terms"),
			plotOutput(ns("wordcloud"), height = SHINY_PLOT_HEIGHT),
			
			tabPanel(
				id = paste0(field, "tm_corterms"),
				
				h3("Find correlated terms"),
				plotOutput(ns("correlatedterms"), height = SHINY_PLOT_HEIGHT)
			),
			tabPanel(
				id = paste0(field, "tm_corterms_none"),
				h3("No correlated terms have been found in the dataset"),
			)
			
			
		)	
	} else{
		tabPanel(id = paste0(field, "tm"), "Empty")
	}
	
}
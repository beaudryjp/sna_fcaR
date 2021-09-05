ETLTab <- function(id){
	ns <- NS(id)
	field = paste0(id, "_")
	
	mainPanel(
		id = "main_data_extraction", 
		
		tabPanel(
			id = paste0(field, "summary"), 
			
			"ETL",
			
			h2("Result Output"),
			tabPanel("Output", verbatimTextOutput(ns("api_result"))),
			
			h2("Table"),
			tabPanel("Output table", DT::dataTableOutput(ns("api_table"))),
		)
	)
	
}
ETLServer <- function(id, data){
	moduleServer(
		id,
		function(input, output, session){
			
			data$created <- format(data$created, " %Y-%m-%d %H:%M")
			data$date_added <- format(data$date_added, " %Y-%m-%d %H:%M")
			
			output$api_result <- renderPrint({
				print(str(data))
				print(data)
			})
			
			output$api_table = DT::renderDataTable({
				DT::datatable(data, 
							  plugins = "ellipsis",
							  options = list(
							  	autoWidth = TRUE,
							  	scrollX = TRUE,
							  	pageLength = SHINY_DT_DEF_PAGELENGTH,
							  	lengthMenu = SHINY_DT_DEF_LENGTHMENU,
							  	rownames = TRUE,
							  	dom = SHINY_DT_BUTTONS_POS,
							  	buttons = list(list(extend = "copy"), 
							  				   list(extend = "csv", filename = get_filename()), 
							  				   list(extend = "excel", filename = get_filename())
							  	),
							  	columnDefs = list(
							  		list(
							  			targets = c(2,3),
							  			render = JS("$.fn.dataTable.render.ellipsis( 135, false )")
							  		),
							  		list(
							  			width = SHINY_DT_DEF_WIDTH, 
							  			targets = "_all"
							  		)
							  	)
							  ))
			}, server = FALSE)
			
			
		}
	)   
}
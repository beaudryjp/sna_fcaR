DataServer <- function(id, data, data_matrix, commands, targets.text = NULL, targets.visible = NULL){
	moduleServer(
		id,
		function(input, output, session){
			
			output$result_output = DT::renderDataTable({
				DT::datatable(data, 
							  extensions = 'Buttons', 
							  plugins = "ellipsis",
							  options = list(
							  	autoWidth = TRUE,
							  	scrollX = TRUE,
							  	pageLength = SHINY_DT_DEF_PAGELENGTH,
							  	lengthMenu = SHINY_DT_DEF_LENGTHMENU,
							  	dom = SHINY_DT_BUTTONS_POS,
							  	buttons = list(list(extend = "copy"), 
							  				   list(extend = "csv", filename = get_filename()), 
							  				   list(extend = "excel", filename = get_filename())
							  	),
							  	columnDefs = list(
							  		list(
							  			targets = targets.text,
							  			render = JS("$.fn.dataTable.render.ellipsis( 135, false )")
							  		),
							  		list(
							  			targets = targets.visible,
							  			visible = FALSE
							  		),
							  		list(
							  			targets = "_all",
							  			width = SHINY_DT_DEF_WIDTH
							  		)
							  	)
							  ))
			})
			
			output$summary <- renderPrint({
				print("str(data)")
				str(data)
				print("summary(data)")
				summary(data)
			})
			
			output$data_normalization <- renderPrint({
				cat(paste(commands, "\n\n", sep=""))
				
			})
			
			output$binary_matrix = DT::renderDataTable({
				DT::datatable(data_matrix[1:50,],
							  callback = JS("$('div.dwnld').append($('#download_matrix'));"),
							  options = list(
							  	scrollX = TRUE,
							  	columnDefs = list(list(width = SHINY_DT_DEF_WIDTH, targets = "_all")),
							  	pageLength = SHINY_DT_DEF_PAGELENGTH,
							  	lengthMenu = SHINY_DT_DEF_LENGTHMENU,
							  	rownames = TRUE,
							  	dom = 'B<"dwnld">frtip'
							  ))
				
			})
			
			output$download_matrix <- downloadHandler(
				filename = function() {
					paste0(get_filename(), ".csv") 
				},
				content = function(file) {
					write.csv(data_matrix, file)
				}
			)
			
			
		}
	)   
}
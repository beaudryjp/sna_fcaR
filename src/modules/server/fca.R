FCAServer <- function(id, data){
	moduleServer(
		id,
		function(input, output, session){
			transactions <- as(data, "transactions");
			rules <- apriori(transactions, parameter = list(support = 0.5, confidence = 1, maxlen = 5));
			
			
			## SUMMARY START
			
			output$information <- renderPrint({
				print("transactions")
				print(transactions)
				print("rules")
				print(rules)
				print("inspect(head(rules))")
				arules::inspect(head(rules))
			})
			
			rules.non_redundant <- rules[!is.redundant(rules)]
			rules.significant <- rules[is.significant(rules, transactions)]
			
			rules_lift <- arules::sort(rules.non_redundant, by="lift")
			rules_support <- arules::sort(rules.non_redundant, by="support")
			
			output$rules_non_redundant <- renderPrint({
				print("rules.non_redundant <- rules[!is.redundant(rules)]")
				print(rules.non_redundant)
				print("inspect(head(rules.non_redundant))")
				arules::inspect(head(rules.non_redundant))
			})
			
			## SUMMARY END
			
			transactions_reduced <- transactions(as(items(rules.non_redundant), "ngCMatrix"))
			
			fc <- FormalContext$new(transactions_reduced)
			print(fc$to_latex())
			
			output$download_fca_latex <- downloadHandler(
				filename = function() {
					paste0(get_filename(), "_fca.latex") 
				},
				content = function(file) {
					write.csv(fc$to_latex(), file)
				}
			)
			
			#fc$implications$add(rules.non_redundant)
			
			
			## CONCEPTS START
			
			shinyjs::showElement(paste0(id, "_tab_tm"))
			show_modal_spinner()
			
			output$plot_initial <- renderPlot({
				fc$plot()
			}, height = SHINY_PLOT_HEIGHT * 2)
			
			
			fc$find_concepts()
			
			output$concept_find <- renderPrint({
				print("head(fc$concepts)")
				head(fc$concepts)
			})
			
			output$concept_one <- renderPrint({
				print("fc$concepts[3]")
				fc$concepts[3]
			})
			
			output$concept_summary <- renderPrint({
				print("fc$concepts$support()")
				fc$concepts$support()
			})
			
			output$plot_concepts <- renderPlot({
				fc$concepts$plot(object_names = F)
			})
			
			idx <- which(fc$concepts$support() > 0.2)
			sublattice <- fc$concepts$sublattice(idx)
			
			output$concept_sublattice <- renderPrint({
				print("idx <- which(fc$concepts$support() > 0.2)")
				print("sublattice <- fc$concepts$sublattice(idx)")
				print("sublattice")
				sublattice
			})
			
			output$plot_sublattice <- renderPlot({
				sublattice$plot(object_names = F)
			})
			
			## CONCEPTS END
			
			remove_modal_spinner()
			shinyjs::showElement(paste0(id, "_tab_concepts"))
			show_modal_spinner()
			
			## IMPLICATIONS START
			
			fc$find_implications()
			output$implication_find <- renderPrint({
				print("fc$find_implications()")
			})
			
			fc$implications$apply_rules(rules = c("reduction", "composition", "generalization",  "simplification"), reorder = TRUE)
			
			output$implication_remove <- renderPrint({
				print("fc$implications$apply_rules(rules = c('reduction', 'composition', 'generalization',  'simplification'))")
			})
			
			
			output$implication_cardinality <- renderPrint({
				print("fc$implications$cardinality()")
				fc$implications$cardinality()
			})
			
			
			set <- Set$new(attributes = fc$attributes)
			output$implication_set_attributes <- renderPrint({
				print("set <- Set$new(attributes = fc$attributes)")
			})
			
			set$assign(attributes = "subreddit-worldnews=1", values = 1)
			output$implication_assign_field <- renderPrint({
				print("set$assign(`subreddit__worldnews` = 1)")
			})
			
			output$implication_compute_intent <- renderPrint({
				print("fc$intent(set)")
				fc$intent(set)
			})
			
			closed_set <- fc$closure(set)
			output$implication_compute_closure <- renderPrint({
				print("closed_set <- fc$closure(set)")
				closed_set
			})
			
			output$implication_closure_closed <- renderPrint({
				print("fc$is_closed(closed_set)")
				fc$is_closed(closed_set)
			})
			
			recommendation <- fc$implications$recommend(set, attribute_filter = fc$attributes)
			output$implication_recommend <- renderPrint({
				print("fc$implications$recommend(set, attribute_filter = fc$attributes)")
				print(recommendation)
				print("recommendation[which(recommendation %in% c(1))]")
				recommendation[which(recommendation %in% c(1))]
			})
			
			## IMPLICATIONS END
			
			remove_modal_spinner()
			shinyjs::showElement(paste0(id, "_tab_arules"))
			show_modal_spinner()
			
			## ARULES START
			
			fc_to_rules <- fc$implications$to_arules()
			output$export_rules <- renderPrint({
				print("fc_to_rules <- fc$implications$to_arules()")
			})
			
			fc_lift <- arules::sort(fc_to_rules, by="lift")
			fc_support <- arules::sort(fc_to_rules, by="support")
			# fc_non_redundant <- fc_to_rules[!is.redundant(fc_to_rules)]
			# fc_significant <- fc_to_rules[is.significant(fc_to_rules, transactions)]
			
			output$lift_table = DT::renderDataTable({
				lift <- as(fc_lift, "data.frame")
				lift <- lift %>% 
					slice(1:30) %>%
					mutate_if(is.numeric, round, digits = 4)
				DT::datatable(lift, options = list(
					scrollX = TRUE,
					columnDefs = list(list(width = SHINY_DT_DEF_WIDTH, targets = "_all")),
					pageLength = SHINY_DT_DEF_PAGELENGTH,
					lengthMenu = SHINY_DT_DEF_LENGTHMENU,
					rownames = TRUE,
					dom = SHINY_DT_BUTTONS_POS,
					buttons = list(list(extend = "copy"),
								   list(extend = "csv", filename = get_filename()),
								   list(extend = "excel", filename = get_filename())
					)
				))
				
			}, server = FALSE)
			
			output$lift_scatter_plot <- renderPlot({
				plot(fc_lift[1:30], col=sequential_hcl(10, "Burg"))
			})
			
			output$lift_graph_plot <- renderPlot({
				plot(fc_lift[1:30], method="graph", col=sequential_hcl(2, "Burg"))
			})
			
			
			output$support_table = DT::renderDataTable({
				support <- as(fc_support, "data.frame")
				support <- support %>% 
					slice(1:30) %>%
					mutate_if(is.numeric, round, digits = 4)
				DT::datatable(support, options = list(
					scrollX = TRUE,
					columnDefs = list(list(width = SHINY_DT_DEF_WIDTH, targets = "_all")),
					pageLength = SHINY_DT_DEF_PAGELENGTH,
					lengthMenu = SHINY_DT_DEF_LENGTHMENU,
					rownames = TRUE,
					dom = SHINY_DT_BUTTONS_POS,
					buttons = list(list(extend = "copy"),
								   list(extend = "csv", filename = get_filename()),
								   list(extend = "excel", filename = get_filename())
					)
				))
				
			}, server = FALSE)
			
			output$support_scatter_plot <- renderPlot({
				plot(fc_support[1:30], col=sequential_hcl(2, "Burg"))
			})
			
			output$support_graph_plot <- renderPlot({
				plot(fc_support[1:30], method="graph")
			})
			
			
			output$nonredundant_table = DT::renderDataTable({
				rules_df <- as(rules.non_redundant, "data.frame")
				#rules_df <- rules_df %>% filter(count >= 2)
				DT::datatable(rules_df, options = list(
					scrollX = TRUE,
					columnDefs = list(list(width = SHINY_DT_DEF_WIDTH, targets = "_all")),
					pageLength = SHINY_DT_DEF_PAGELENGTH,
					lengthMenu = SHINY_DT_DEF_LENGTHMENU,
					rownames = TRUE,
					dom = SHINY_DT_BUTTONS_POS,
					buttons = list(list(extend = "copy"),
								   list(extend = "csv", filename = get_filename()),
								   list(extend = "excel", filename = get_filename())
					)
				))
				
			}, server = FALSE)
			
			output$nonredundant_plot <- renderPlot({
				plot(rules.non_redundant, col=sequential_hcl(2, "Burg"))
			})
			
			
			output$significant_table = DT::renderDataTable({
				rules_sig_df  <- as(head(rules.significant, 100), "data.frame")
				DT::datatable(rules_sig_df, options = list(
					scrollX = TRUE,
					columnDefs = list(list(width = SHINY_DT_DEF_WIDTH, targets = "_all")),
					pageLength = SHINY_DT_DEF_PAGELENGTH,
					lengthMenu = SHINY_DT_DEF_LENGTHMENU,
					rownames = TRUE,
					dom = SHINY_DT_BUTTONS_POS,
					buttons = list(list(extend = "copy"),
								   list(extend = "csv", filename = get_filename()),
								   list(extend = "excel", filename = get_filename())
					)
				))
			}, server = FALSE)
			
			output$significant_plot <- renderPlot({
				plot(head(rules.significant, 100), method="graph")
			})
			
			## ARULES END
			remove_modal_spinner()
		}
	)   
}
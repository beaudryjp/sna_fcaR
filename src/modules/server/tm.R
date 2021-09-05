TMServer <- function(id, data, percentage = 0.5){
	moduleServer(
		id,
		function(input, output, session){
			
			show_modal_spinner()
			shinyjs::showElement(paste0(id, "_tab_tm"))
			
			output$uniq_users <- renderPrint({
				length(unique(data$username))
			})
			
			
			output$top = DT::renderDataTable({
				top_users <- data %>%
					filter(username != c('AutoModerator', '[deleted]')) %>%
					group_by(username) %>%
					summarise(count = n()) %>%
					arrange(desc(count)) %>%
					head(30)
				DT::datatable(top_users, options = list(
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
			
			terms <- freq_terms_from_data(data$title)
			dtm <- terms[[1]]
			freq.dt <- terms[[2]]
			freq.mean <- mean(freq.dt$freq)
			over.mean <- freq.dt[freq.dt$freq > freq.mean,]
			over.mean <- over.mean %>%
				arrange(desc(freq)) %>%
				head(30) %>%
				mutate(term = reorder(term, freq))
			
			output$top_overmean <- renderPlot({
				over.mean %>%
					ggplot(aes(term, freq)) +
					geom_col(show.legend = TRUE) +
					labs(x=NULL, y="Frequency") +
					coord_flip()+
					ggtitle("Most frequent terms")
			})
			
			output$wordcloud <- renderPlot({
				wordcloud::wordcloud(freq.dt$term, freq.dt$freq, scale=c(5, 0.5),
									 max.words=500, random.order=FALSE,
									 rot.per=0.1, use.r.layout=TRUE,
									 colors=RColorBrewer::brewer.pal(6, "Dark2"))
			})
			
			
			top.words <- freq.dt %>% arrange(desc(freq)) %>% head(7)
			
			terms.df <- find_correlated_terms(dtm, top.words$term, percentage)
			if(nrow(terms.df) > 0){
				shinyjs::showElement(paste0(id, "_tm_corterms"))
				output$correlatedterms <- renderPlot({
				terms.df %>% 
					group_by(lhs) %>% 
					top_n(5, cor) %>%
					ungroup() %>%
					mutate(rhs = reorder(rhs, cor)) %>%
					ggplot(aes(rhs, cor, fill=lhs)) +
					geom_col(show.legend = F) +
					coord_flip() +
					facet_wrap(~lhs,ncol=2, scales="free") +
					ggtitle("Top words & their correlation") +
					labs(x=NULL, y="Correlation")
			})
			} else{
				shinyjs::hideElement(paste0(id, "_tm_corterms"))
				shinyjs::showElement(paste0(id, "_tm_corterms_none"))
			}
			
			
			remove_modal_spinner()
		}
	)   
}
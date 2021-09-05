# Main server for the shiny app
shinyServer(function(input, output, session) {
	hideElement("main_data_extraction")
	hideElement("main_posts")
	hideElement("main_comments")
	hideElement("main_awards")
	hideElement("post_tab_tm")
	hideElement("post_tab_summary")
	hideElement("post_tab_concepts")
	hideElement("post_tab_implications")
	hideElement("post_tab_arules")
	hideElement("comm_tab_tm")
	hideElement("comm_tab_summary")
	hideElement("comm_tab_concepts")
	hideElement("comm_tab_implications")
	hideElement("comm_tab_arules")
	hideElement("award_tab_tm")
	hideElement("award_tab_summary")
	hideElement("award_tab_concepts")
	hideElement("award_tab_implications")
	hideElement("award_tab_arules")
	hideElement(paste0(SHINY_POST_ID, "_tm_corterms_none"))
	hideElement(paste0(SHINY_AWARD_ID, "_tm_corterms_none"))
	hideElement(paste0(SHINY_COMMENT_ID, "_tm_corterms_none"))
	
	# SHINY AUTH - SERVER
	# res_auth <- secure_server(
	# 	check_credentials = check_credentials(
	# 		"data/credentials.sqlite",
	# 		passphrase = "passphrase_wihtout_keyring"
	# 	)
	# )

	# output$auth_output <- renderPrint({
	# 	reactiveValuesToList(res_auth)
	# })
	
	subreddits <- subreddit.get_all()

	# api sidebar selects
	updateSelectizeInput(session = session, inputId = "subreddits_api", choices = c("popular", subreddits$name), server = TRUE, selected = "popular")
	updateSelectizeInput(session = session, inputId = "select_subs", choices = c("All", subreddits$name), server = TRUE, selected = c("worldnews", "news", "askreddit"))
	updateSelectizeInput(session = session, inputId = "listing", choices = LISTINGS, server = TRUE, selected = "top")
	updateSelectizeInput(session = session, inputId = "timeframe", choices = TIMEFRAMES, server = TRUE, selected = "day")
	updateSelectizeInput(session = session, inputId = "listing_populate", choices = LISTINGS, server = TRUE, selected = "top")
	updateSelectizeInput(session = session, inputId = "timeframe_populate", choices = TIMEFRAMES, server = TRUE, selected = "month")
	
	# search sidebar selects
	updateSelectizeInput(session = session, inputId = "post_search", choices = c("All", subreddits$name), server = TRUE, selected = "All")
	
	
	output$currentTime <- renderText({
		invalidateLater(as.integer(60000), session)
		format(Sys.time(), "%Y-%m-%d %H:%M")
	})
	
	observeEvent(c(input$navigation_menu, input$post_tabs, input$award_tabs, input$comment_tabs, input$data_tabs), {
		show_modal_spinner() 
		Sys.sleep(0.5)
		remove_modal_spinner() 
	})
	
	
	observeEvent(input$populate_get, {
		showElement("main_data_extraction")
		show_modal_spinner() 
		if("All" %in% input$select_subs){
			ETLServer(SHINY_ETL_ID, subreddit.auto_populate(input$limit_posts_populate, input$listing_populate, input$timeframe_populate, input$include_comments_populate))

		} else{
			ETLServer(SHINY_ETL_ID, subreddit.populate_from_subreddits(input$select_subs, input$limit_posts_populate, input$listing_populate, input$timeframe_populate, input$include_comments_populate) )
		}
		remove_modal_spinner()
	})
	
	observeEvent(input$submit_get, {
		showElement("main_data_extraction")
		show_modal_spinner()
		ETLServer(SHINY_ETL_ID, post.get_by_listing(input$subreddits_api, input$listing, input$number_posts_api, input$timeframe, TRUE))
		remove_modal_spinner()
	})
	
	## POSTS
	observeEvent(input$post_submit_search, {
		showElement("main_posts")
		show_modal_spinner()
		
		limit <- input$post_limit
		if(limit > DB_LIMIT_MAX){
			limit <- DB_LIMIT_MAX
		}
		where <- c()
		order <- "post.created"
		date.start <- input$post_date_created[1]
		date.end <- input$post_date_created[2]
		if(input$post_search != "All"){
			where <- c(where, sqlInterpolate(ANSI(), "subreddit.name = ?subreddit", subreddit = input$post_search))
		}
		if(input$post_filter_by_date == "date_added"){
			order <- "post.date_added" 
			date.start <- input$post_date_added[1]
			date.end <- input$post_date_added[2]
		}
		where <- c(where, paste0("date(" , order , ") >= ?start AND date(" , order , ") <= ?end"))
		
		sql = paste(DB_BASE_POSTS, paste("WHERE", paste(where, collapse = " AND ")), "ORDER BY", order, "DESC LIMIT", limit, ";", sep = " ")
		query <- sqlInterpolate(ANSI(), sql, start = date.start, end = date.end)
		data <- db.fetch_all(query)

		data$created <- format(as.POSIXct(data$created), " %Y-%m-%d %H:%M")
		data$date_added <- format(as.POSIXct(data$date_added), " %Y-%m-%d %H:%M")
		data$upvote_ratio <- round(data$upvote_ratio, 3)

		#data_dt <- data %>% select(-id, -selftext, -subreddit_id, -user_id)
		data_df <- data %>% select(upvote_ratio, total_awards_received, score, domain, subreddit)
		data_df[sapply(data_df, is.integer)] <- lapply(data_df[sapply(data_df, is.integer)], as.numeric)
		data_df[sapply(data_df, is.character)] <- lapply(data_df[sapply(data_df, is.character)], as.factor)
		data_df[is.na(data_df)] <- 0
		data_df <- discretizeDF(data_df, default = list(method = "interval", breaks = 3, labels = c("Low", "Medium", "High")))


		top_n_domains <- data_df %>%
			select(domain) %>%
			group_by(domain) %>%
			summarise(n = n()) %>%
			filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
			add_row(domain = "other", n=0) %>%
			mutate_if(is.character,
					  str_replace_all, pattern = "self", replacement = "subreddit")

		top_n_subreddits <- data_df %>%
			select(subreddit) %>%
			group_by(subreddit) %>%
			summarise(n = n()) %>%
			filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
			add_row(subreddit = "other", n = 0)

		cols <- c(paste("upvote_ratio", names(table(data_df$upvote_ratio)), sep = "-"),
				  #paste("ups", names(table(data_df$ups)), sep = "-"),
				  paste("total_awards_received", names(table(data_df$total_awards_received)), sep = "-"),
				  paste("score", names(table(data_df$score)), sep = "-"),
				  paste("domain", top_n_domains$domain, sep = "-"),
				  paste("subreddit", top_n_subreddits$subreddit, sep = "-"))

		data_matrix <- create_binary_matrix(cols, data_df)

		commands <- c("data$upvote_ratio <- round(data$upvote_ratio, 3)",
					  "data$created <- format(data$created, '%Y-%m-%d %H:%M')",
					  "data$date_added <- format(data$date_added, '%Y-%m-%d %H:%M')",
					  "data_df <- posts %>% select(upvote_ratio, total_awards_received, score, domain, subreddit)",
					  "data_df[sapply(data_df, is.integer)] <- lapply(data_df[sapply(data_df, is.integer)], as.numeric)",
					  "data_df[sapply(data_df, is.character)] <- lapply(data_df[sapply(data_df, is.character)], as.factor)",
					  "data_df[is.na(data_df)] <- 0",
					  "data_df <- discretizeDF(data_df, default = list(method = 'interval', breaks = 3, labels = c('Low', 'Medium', 'High')))",
					  "top_n_domains <- data_df %>%
				select(domain) %>%
				group_by(domain) %>%
				summarise(n = n()) %>%
				filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
				add_row(domain = 'other', n=0) %>%
				mutate_if(is.character,
						  str_replace_all, pattern = 'self', replacement = 'subreddit')",
				"top_n_subreddits <- data_df %>%
				select(subreddit) %>%
				group_by(subreddit) %>%
				summarise(n = n()) %>%
				filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
				add_row(subreddit = 'other, n = 0)",
				"cols <- c(paste('upvote_ratio', names(table(data_df$upvote_ratio)), sep = '-'),
					  paste('total_awards_received', names(table(data_df$total_awards_received)), sep = '-'),
					  paste('score', names(table(data_df$score)), sep = '-'),
					  paste('domain', top_n_domains$domain, sep = '-'),
					  paste('subreddit', top_n_subreddits$subreddit, sep = '-'))",
				"data_matrix <- create_binary_matrix(cols, data_df)"
		)

		DataServer(SHINY_POST_ID, data, data_matrix, commands, c(1,2), c(1, 13, 14))

		output$post_plot_upvoteratio <- renderPlot({
			ggplot(as.data.frame(data_df)) +
				geom_bar(aes(x = upvote_ratio), stat = 'count')
		})

		output$post_plot_score <- renderPlot({
			ggplot(as.data.frame(data_df)) +
				geom_bar(aes(x = score), stat = 'count')
		})

		output$post_plot_domains <- renderPlot({
			data_df %>%
				select(domain) %>%
				group_by(domain) %>%
				summarise(n = n()) %>%
				arrange(desc(n)) %>%
				top_n(8) %>%
				ggplot() +
				geom_bar(aes(x = domain, y = n), stat = 'identity')
		}, height = 500)

		output$post_plot_subreddits <- renderPlot({
			data_df %>%
				select(subreddit) %>%
				group_by(subreddit) %>%
				summarise(n = n()) %>%
				arrange(desc(n)) %>%
				top_n(8) %>%
				ggplot() +
				geom_bar(aes(x = subreddit, y = n), stat = 'identity')
		}, height = 500)

		remove_modal_spinner()
		showElement("post_tab_summary")

		## TM Tab

		TMServer(SHINY_POST_ID, data)

		## FCA TAB

		FCAServer(SHINY_POST_ID, data_matrix)
		
		
	})
	
	## COMMENTS
	observeEvent(input$comment_submit_search, {
		showElement("main_comments")
		show_modal_spinner()
		
		limit <- input$comment_limit
		if(limit > DB_LIMIT_MAX){
			limit <- DB_LIMIT_MAX
		}
		where <- c()
		order <- "comment.score"
		date.start <- input$comment_date_created[1]
		date.end <- input$comment_date_created[2]
		date.field <- "post.created"
		
		if(input$comment_search == "User Karma"){
			order <- "user.total_karma"
		} else if(input$comment_search == "Date created"){
			order <- "post.created"
		}
		
		if(input$comment_filter_by_date == "date_added"){
			date.field <- "post.date_added"
			date.start <- input$comment_date_added[1]
			date.end <- input$comment_date_added[2]
		}
		
		where <- c(where, paste0("date(" , date.field , ") >= ?start AND date(" , date.field , ") <= ?end"))
		sql = paste(DB_BASE_COMMENTS,paste("WHERE", paste(where, collapse = " AND ")), "ORDER BY", order,"DESC LIMIT", limit, ";", sep = " ")
		query <- sqlInterpolate(ANSI(), sql, start = date.start, end = date.end)
		data <- db.fetch_all(query)

		data[is.na(data)] <- 0
		data_df <- data %>% select(-id, -username, -title)
		data_df$user_is_employee <- as.logical(data_df$user_is_employee)
		data_df$user_is_mod <- as.logical(data_df$user_is_mod)
		data_df$user_is_gold <- as.logical(data_df$user_is_gold)
		data_df[sapply(data_df, is.integer)] <- lapply(data_df[sapply(data_df, is.integer)], as.numeric)
		data_df[sapply(data_df, is.character)] <- lapply(data_df[sapply(data_df, is.character)], as.factor)
		
		data_df <- discretizeDF(data_df, default = list(method = "interval", breaks = 3, labels = c("Low", "Medium", "High")))
		
		top_n_subreddits <- data_df %>%
			select(subreddit) %>%
			group_by(subreddit) %>%
			summarise(n = n()) %>%
			filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
			add_row(subreddit = "other", n = 0)
		
		cols <- c(paste("score", names(table(data_df$score)), sep = "-"),
				  paste("user_total_karma", names(table(data_df$user_total_karma)), sep = "-"),
				  paste("user_is_employee", names(table(data_df$user_is_employee)), sep = "-"),
				  paste("user_is_mod", names(table(data_df$user_is_mod)), sep = "-"),
				  paste("user_is_gold", names(table(data_df$user_is_gold)), sep = "-"),
				  paste("subreddit", top_n_subreddits$subreddit, sep = "-"))
		
		data_matrix <- create_binary_matrix(cols, data_df);
		
		commands <- c("data[is.na(data)] <- 0",
					  "data_df <- data %>% select(-id, -username, -title)",
					  "data_df$user_is_employee <- as.logical(data_df$user_is_employee)",
					  "data_df$user_is_mod <- as.logical(data_df$user_is_mod)",
					  "data_df$user_is_gold <- as.logical(data_df$user_is_gold)",
					  "data_df[sapply(data_df, is.integer)] <- lapply(data_df[sapply(data_df, is.integer)], as.numeric)",
					  "data_df[sapply(data_df, is.character)] <- lapply(data_df[sapply(data_df, is.character)], as.factor)",
					  "data_df <- discretizeDF(data_df, default = list(method = 'interval', breaks = 3, labels = c('Low', 'Medium', 'High')))",
					  "top_n_domains <- data_df %>%
				select(domain) %>%
				group_by(domain) %>%
				summarise(n = n()) %>%
				filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
				add_row(domain = 'other', n=0) %>%
				mutate_if(is.character,
						  str_replace_all, pattern = 'self', replacement = 'subreddit')",
				"cols <- c(paste('score', names(table(data_df$score)), sep = '-'),
					  paste('user_total_karma', names(table(data_df$user_total_karma)), sep = '-'),
					  paste('user_is_employee', names(table(data_df$user_is_employee)), sep = '-'),
					  paste('user_is_mod', names(table(data_df$user_is_mod)), sep = '-'),
					  paste('user_is_gold', names(table(data_df$user_is_gold)), sep = '-'),
					  paste('subreddit', top_n_subreddits$subreddit, sep = '-'))",
				"data_matrix <- create_binary_matrix(cols, data_df)"
		)
		
		
		DataServer(SHINY_COMMENT_ID, data, data_matrix, commands, 3, 1)
		
		remove_modal_spinner()
		showElement("comment_tab_summary")
		
		## TM Tab
		
		TMServer(SHINY_COMMENT_ID, data, 0.9)
		
		## FCA TAB
		
		FCAServer(SHINY_COMMENT_ID, data_matrix)
		
		
	})
	
	## AWARDS
	observeEvent(input$award_submit_search, {
		shinyjs::hide(selector = "#data_tabs li a[data-value=Empty]")
		showElement("main_awards")
		show_modal_spinner()
		
		limit <- input$award_limit
		if(limit > DB_LIMIT_MAX){
			limit <- DB_LIMIT_MAX
		}
		where <- c()
		order <- "quantity"
		date.start <- input$award_date_created[1]
		date.end <- input$award_date_created[2]
		date.field <- "post.created"
		if(input$award_search == "Cost"){
			order <- "cost"
		}
		
		if(input$award_filter_by_date == "date_added"){
			date.field <- "post.date_added"
			date.start <- input$award_date_added[1]
			date.end <- input$award_date_added[2]

		}
		where <- c(where, paste0("date(" , date.field , ") >= ?start AND date(" , date.field , ") <= ?end"))
		
		sql = paste(DB_BASE_AWARDS, paste("WHERE ", paste(where, collapse = " AND ")), "GROUP BY subreddit, coin ORDER BY", order,"DESC LIMIT", limit, ";", sep = " ")
		query <- sqlInterpolate(ANSI(), sql, start = date.start, end = date.end)
		data <- db.fetch_all(query)

		data[is.na(data)] <- 0
		data_df <- discretizeDF(data, default = list(method = "interval", breaks = 3, labels = c("Low", "Medium", "High")))
		
		data_df[sapply(data_df, is.character)] <- lapply(data_df[sapply(data_df, is.character)], as.factor)
		
		top_n_subreddits <- data %>% 
			select(subreddit) %>% 
			group_by(subreddit) %>% 
			summarise(n = n()) %>% 
			arrange(desc(n)) %>% 
			filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
			add_row(subreddit = "other", n = 0)
		
		top_n_coins <- data %>% 
			select(coin) %>% 
			mutate(coin = escape_strings(coin, FALSE)) %>%
			group_by(coin) %>% 
			summarise(n = n()) %>% 
			arrange(desc(n)) %>% 
			filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
			add_row(coin = "other", n = 0)
		
		cols <- c(paste("subreddit", top_n_subreddits$subreddit, sep = "-"),
				  paste("coin", top_n_coins$coin, sep = "-"),
				  paste("quantity", names(table(data_df$quantity)), sep = "-"),
				  paste("cost", names(table(data_df$cost)), sep = "-"))
		
		data_matrix <- create_binary_matrix(cols, data_df);
		
		commands <- c("data[is.na(data)] <- 0",
					  "data_df[sapply(data_df, is.character)] <- lapply(data_df[sapply(data_df, is.character)], as.factor)",
					  "data_df <- discretizeDF(data_df, default = list(method = 'interval', breaks = 3, labels = c('Low', 'Medium', 'High')))",
					  "top_n_domains <- data_df %>%
				select(domain) %>%
				group_by(domain) %>%
				summarise(n = n()) %>%
				filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
				add_row(domain = 'other', n=0) %>%
				mutate_if(is.character,
						  str_replace_all, pattern = 'self', replacement = 'subreddit')",
				"top_n_subreddits <- data_df %>%
				select(subreddit) %>%
				group_by(subreddit) %>%
				summarise(n = n()) %>%
				filter(n > quantile(n, SHINY_FREQ_PERCENTAGE)) %>%
				add_row(subreddit = 'other, n = 0)",
				"cols <- c(paste('subreddit', top_n_subreddits$subreddit, sep = '-'),
					  paste('coin', top_n_coins$coin, sep = '-'),
					  paste('quantity', names(table(data_df$quantity)), sep = '-'),
					  paste('cost', names(table(data_df$cost)), sep = '-'))",
				"data_matrix <- create_binary_matrix(cols, data_df)"
		)
		
		DataServer(SHINY_AWARD_ID, data, data_matrix, commands)
		
		remove_modal_spinner()
		showElement("award_tab_summary")
		
		## FCA TAB
		
		FCAServer(SHINY_AWARD_ID, data_matrix)
	})
	
})
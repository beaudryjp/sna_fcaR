
# STRUCTURES ###################################### 

### Returns the dataframe structure for awards
### @parameters: none
### @returns: dataframe
df_award = function(){
	data <- data.frame(post_id = character(), coin_id = character(), quantity = integer())
	return(data)
}

### Returns the dataframe structure for coins
### @parameters: none
### @returns: dataframe
df_coin = function(){
	data <- data.frame(id = character(), name = character(), description = character(),
					   coin_price = integer(), coin_reward = integer())
	return(data)
}

# DATABASE OPERATIONS ###################################### 

### Insert awards from a post into the database
### @parameters: awards = dataframe
### @returns: none
award.insert_in_db = function(data){
	values <- paste0("('", data$post_id, "','", data$coin_id, "',", data$quantity, ")", collapse = ", ")
	query <- paste0("INSERT IGNORE INTO awards_post VALUES ", values, ";")
	# database$insert(query)
	db.insert(query)
}

### Insert coins into the database
### @parameters: coin = dataframe
### @returns: none
coin.insert_in_db = function(data){
	values <- paste0("('", data$id, "','", 
					 data$name, "','", 
					 data$description, "',", 
					 data$coin_price, ",", 
					 data$coin_reward,")", 
					 collapse = ", ")
	query <- paste0("INSERT IGNORE INTO coin VALUES ", values, ";")
	# database$insert(query)
	db.insert(query)
}

# DATA MANIPULATION ###################################### 

### Insert awards from a corresponding post
### @parameters: data = dataframe
### @returns: none
award.process = function(data){
	new_data <- data[data$post_exists == FALSE,]
	if(length(new_data$all_awardings) > 0){
		coin <- df_coin()
		award_post <- df_award()
		for(row in 1:length(new_data$all_awardings)){
			if(!is.null(nrow(new_data[row, ]$all_awardings[[1]]))  && nrow(new_data[row, ]$all_awardings[[1]]) > 0){
				award_post <- award_post %>%
					add_row(post_id = new_data[row, ]$id,
							coin_id = new_data$all_awardings[[row]]$id,
							quantity = new_data$all_awardings[[row]]$count)
				coin <- coin %>%
					add_row(id = new_data$all_awardings[[row]]$id,
							name = gsub("'","" , new_data$all_awardings[[row]]$name ,ignore.case = TRUE),
							description = escape_strings(new_data$all_awardings[[row]]$description),
							coin_price = new_data$all_awardings[[row]]$coin_price,
							coin_reward = new_data$all_awardings[[row]]$coin_reward)
			}
			
		}
		if(nrow(coin) > 0) coin.insert_in_db(coin %>% distinct())
		if(nrow(award_post) > 0) award.insert_in_db(award_post)
	}
	
}

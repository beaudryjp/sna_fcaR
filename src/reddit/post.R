
# STRUCTURES ###################################### 

### Get post dataframe
### @parameters: data = dataframe
### @returns: dataframe
df_post <- function(data){
    ratio <- 0
    if(!is.null(data$upvote_ratio))
        ratio <- data$upvote_ratio
    post <- data.frame(id = data$id, 
                       title = data$title, 
                       upvote_ratio = ratio, 
                       ups = data$ups, 
                       total_awards_received = data$total_awards_received, 
                       score = data$score, 
                       created = anytime::anytime(data$created), 
                       date_added = Sys.time(),
                       #created = date_to_timestamp(data$created),
                       #date_added = date_to_timestamp(Sys.time()),
                       permalink = data$permalink,
                       url = data$url,
                       domain = data$domain, 
                       subreddit_id = data$subreddit_id, 
                       author = data$author,
                       author_id = data$author_id,
                       subreddit = data$subreddit)
    return(post)
}


###################################### SEARCHES ######################################  


### Get posts by subreddit & listing
### @parameters: subreddit = string, listing = string, limit = integer, timeframe = string, ins_comment = boolean
### @returns: dataframe
post.get_by_listing <- function(subreddit, listing, limit=25, timeframe, ins_comments = TRUE){
    if(tolower(listing) == "hot" || tolower(listing) == "rising")
        url <- paste(c("https://www.reddit.com/r/", subreddit, "/", listing, ".json", "?limit=", limit), collapse = "")
    else
        url <- paste(c("https://www.reddit.com/r/", subreddit, "/", listing, ".json", "?limit=", limit, "&t=", timeframe), collapse = "")
    print(paste0("Time: ", Sys.time()))
    print(paste0("Getting ", limit ," posts from the subreddit ", subreddit, " with the options: listing=", listing, ", timeframe=", timeframe))
    print(paste0("URL: ", url))
    return(post.get_data(url, ins_comments))
}


### Get posts by search query using JSON
### @parameters: query = string, subreddit = string, listing = string, limit = integer, timeframe = string, ins_comment = boolean
### @returns: dataframe
post.find_by_query  <- function(query, subreddit, listing, limit=25, timeframe, restrict = FALSE, ins_comments = TRUE){
    url <- paste(c("https://www.reddit.com/r/", subreddit, "/search/.json", "?q=", query, "&restrict_sr=", restrict, "&sort=", listing, "&t=", timeframe, "&limit=", limit), collapse = "")
    print(paste0("Time: ", Sys.time()))
    print(paste0("Getting ", limit ," posts from the subreddit ", subreddit, " with the options: listing=", listing, ", timeframe=", timeframe))
    print(paste0("URL: ", url))
    return(post.get_data(url, ins_comments))
}


### post.find_by_query_alt()
### @parameters: query = string, regex = string, subrreddit = string, sort = string
### @returns: dataframe
### Get posts by search query using RedditExtractor.reddit_urls()
# post.find_by_query_alt <- function(query, regex="", subrreddit=NA, sort="relevance"){
#   data <- reddit_urls(search_terms = query, regex_filter = regex, subreddit = subrreddit,
#                       cn_threshold = 0, page_threshold = 1, sort_by = sort,
#                       wait_time = 2)
# }



###################################### DATA COLLECTION ######################################  


### Get posts from a specified URL using JSON
### @parameters: url = string, ins_comments = boolean
### @returns: dataframe
post.get_data <- function(url, ins_comments){
    json = get_json(url)
    if(length(json) > 0){
        data <- data.frame(json$data$children$data)
        if(nrow(data) > 0){
            if(!("selftext" %in% colnames(data))) data$selftext <- ""
            if(!("author_fullname" %in% colnames(data))) data$author_fullname <- "t2_t1234"
            data <- data %>% 
                mutate(title = escape_strings(title), 
                       selftext = escape_strings(selftext), 
                       url = gsub("'","" , url , ignore.case = TRUE)) %>%
                mutate(url = gsub("\\\\", "/", url))
            
            data$author_id <- sapply(strsplit(data$author_fullname,"_"), `[`, 2)
            data$author_id <- data$author_id %>% replace_na("t1234")
            
            data$subreddit_id <- mapply(subreddit.get_id, data$subreddit, data$subreddit_subscribers)
            
            data$post_exists <- sapply(data$id, function(x) post.check_if_exists(x))
            false_values <- length(data$post_exists[data$post_exists == F])
            data_all <- df_post(data)
            print(paste0("Getting information from authors: ", paste0(head(data$author), collapse = ", "), ", ..."))
            
            if(false_values == nrow(data)){
                post.get_user_info(data)
                post.insert_in_db(data_all)
                award.process(data)
                if(isTRUE(ins_comments)) comment.process(data)
            }
            else if(!all(data$post_exists)){
                data1 <- split(data, data$post_exists)
                post_exists <- df_post(data1[[2]])
                post_not_exists <- df_post(data1[[1]])
                data_all <- rbind(post_not_exists, post_exists)
                
                post.get_user_info(post_not_exists)
                post.insert_in_db(post_not_exists)
                award.process(data)
                if(isTRUE(ins_comments)) comment.process(post_not_exists)
            } 
            
            return(data_all)
        }
    }
    
}


### Get user information from the post
### @parameters: post = dataframe
### @returns: none
post.get_user_info <- function(data){
    total_rows <- nrow(data)
    for(row in 1:total_rows){
        info <- user.get_info(data[row,]$author)
        info$id <- data[row,]$author_id
        user.insert_in_db(info)
    }
}


###################################### DATABASE OPERATIONS ######################################  


### Check if posts exists in database
### @parameters: id = string
### @returns: TRUE or FALSE
post.check_if_exists <- function(id){
    query <- paste("SELECT id FROM post WHERE id='",tolower(id),"';", sep="")
    # return(database$exists(query))
    return(db.exists(query))
}


### Updates a post text
### @parameters: id = string, text = string
### @returns: none
post.update_description <- function(id, text){
    query <- paste0("UPDATE post SET selftext = '", text ,"' WHERE id = '", id ,"';")
    # database$insert(query)
    db.insert(query)
}


### Insert post into the database
### @parameters: post = dataframe
### @returns: none
post.insert_in_db <- function(data){
    values <- paste0("('", 
                     data$id, "', '", 
                     data$title, "', '',",   
                     data$upvote_ratio, ", ",
                     data$ups, ", ",
                     data$total_awards_received, ", ",
                     data$score, ", '",
                     data$created, "', '",
                     data$date_added, "', '",
                     data$permalink, "', '",
                     data$url, "', '",
                     data$domain, "', ",
                     data$subreddit_id, ", '",
                     data$author_id,"')", collapse = ", ")
    query <- paste0("INSERT IGNORE INTO post VALUES ", values, ";")
    # database$insert(query)
    db.insert(query)
}


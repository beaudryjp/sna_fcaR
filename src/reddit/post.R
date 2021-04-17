source(paste0(getwd(), "/src/", "general.R"))
source(paste0(getwd(), "/src/reddit/", "user.R"))


###################################### SEARCHES ######################################  


### post.get_by_listing()
### Get posts by subreddit & listing
post.get_by_listing <- function(subreddit, listing, limit=25, timeframe, ins_comments = TRUE){
  if(tolower(listing) == "hot" || tolower(listing) == "rising")
    url <- paste(c("https://www.reddit.com/r/", subreddit, "/", listing, ".json", "?limit=", limit), 
                 collapse = "")
  else
    url <- paste(c("https://www.reddit.com/r/", subreddit, "/", listing, ".json", "?limit=", limit, "&t=", timeframe), 
                 collapse = "")
  print(paste0("Getting ", limit ," posts from the subreddit ", subreddit))
  print(paste0("URL: ", url))
  return(post.get_data(url, ins_comments))
}


### post.find_by_query_alt()
### Get posts by search query using JSON
post.find_by_query  <- function(query, subreddit, listing, limit=25, timeframe, restrict = FALSE, ins_comments = TRUE){
  url <- paste(c("https://www.reddit.com/r/", subreddit, "/search/.json", "?q=", query, "&restrict_sr=", restrict, 
                 "&sort=", listing, "&t=", timeframe, "&limit=", limit), collapse = "")
  print(paste0("Getting ", limit ," posts from the subreddit ", subreddit))
  print(paste0("URL: ", url))
  return(post.get_data(url, ins_comments))
}


### post.find_by_query_alt()
### Get posts by search query using RedditExtractor.reddit_urls()
# post.find_by_query_alt <- function(query, regex="", subrreddit=NA, sort="relevance"){
#   data <- reddit_urls(search_terms = query, regex_filter = regex, subreddit = subrreddit,
#                       cn_threshold = 0, page_threshold = 1, sort_by = sort,
#                       wait_time = 2)
# }



###################################### DATA COLLECTION ######################################  

### post.get_comments_api()
### Get comments from a specified post
### Based on function RedditExtractorR.reddit_content(), modified to decrease the limit
post.get_comments_api <- function(URL, wait_time = 2, sortby = "top", limit = 50){
  
  if(is.null(URL) | length(URL)==0 | !is.character(URL)){stop("invalid URL parameter")}
  
  # setting up a function for extraction of comment specific information:
  GetAttribute  = function(node,feature){
    Attribute   = node$data[[feature]]
    replies     = node$data$replies
    reply.nodes = if (is.list(replies)) replies$data$children else NULL
    return(list(Attribute, lapply(reply.nodes,function(x){GetAttribute(x,feature)})))  
  }
  
  get.structure = function(node, depth=0) {
    if(is.null(node)) {return(list())}
    filter     = is.null(node$data$author)
    replies     = node$data$replies
    reply.nodes = if (is.list(replies)) replies$data$children else NULL
    return(list(paste0(filter," ",depth), lapply(1:length(reply.nodes), function(x) get.structure(reply.nodes[[x]], paste0(depth, "_", x)))))
  }
  
  # setting up the data frame
  data_extract = data.frame(id               = numeric(),
                            structure        = character(),
                            post_date        = as.Date(character()),
                            comm_date        = as.Date(character()),
                            num_comments     = numeric(),
                            subreddit        = character(),
                            upvote_prop      = numeric(),
                            post_score       = numeric(),
                            author           = character(),
                            user             = character(),
                            comment_score    = numeric(),
                            comment          = character(),
                            title            = character(),
                            post_text        = character(),
                            link             = character(),
                            domain           = character(),
                            URL              = character())
  
  
  for(i in seq(URL)){
    
    if(!grepl("^https?://(.*)",URL[i])) URL[i] = paste0("https://www.",gsub("^.*(reddit\\..*$)","\\1",URL[i]))
    if(!grepl("\\?ref=search_posts$",URL[i])) URL[i] = paste0(gsub("/$","",URL[i]),"/?ref=search_posts")
    
    X        = paste0(gsub("\\?ref=search_posts$","",URL[i]),".json?sort=", sortby,",&limit=",limit,"")
    #print(X)
    raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)),error = function(e) NULL)
    
    # try again if it fails
    if(is.null(raw_data)){
      Sys.sleep(min(1,wait_time))
      raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)),error = function(e) NULL)
    }
    
    if(is.null(raw_data)==FALSE){
      
      # extracting comment specific information:
      meta.node     = raw_data[[1]]$data$children[[1]]$data
      main.node     = raw_data[[2]]$data$children
      
      if(min(length(meta.node),length(main.node))>0){
        
        structure     = unlist(lapply(1:length(main.node), function(x) get.structure(main.node[[x]], x)))
        
        TEMP          =          data.frame(id               = NA,
                                            structure        = gsub("FALSE ","",structure[!grepl("TRUE",structure)]),
                                            post_date        = format(as.Date(as.POSIXct(meta.node$created_utc,origin="1970-01-01")),"%d-%m-%y"),
                                            comm_date        = format(as.Date(as.POSIXct(unlist(lapply(main.node, function(x){GetAttribute(x,"created_utc")})),
                                                                                         origin="1970-01-01")),"%d-%m-%y"),
                                            num_comments     = meta.node$num_comments,
                                            subreddit        = ifelse(is.null(meta.node$subreddit),"UNKNOWN",meta.node$subreddit),
                                            upvote_prop      = meta.node$upvote_ratio,
                                            post_score       = meta.node$score,
                                            author           = meta.node$author,
                                            user             = unlist(lapply(main.node, function(x){GetAttribute(x,"author")})),
                                            comment_score    = unlist(lapply(main.node, function(x){GetAttribute(x,"score")})),
                                            comment          = unlist(lapply(main.node, function(x){GetAttribute(x,"body")})),
                                            title            = meta.node$title,
                                            post_text        = meta.node$selftext,
                                            link             = meta.node$url,
                                            domain           = meta.node$domain,
                                            URL              = URL[i],
                                            stringsAsFactors = FALSE)
        
        TEMP$id = 1:nrow(TEMP)
        
        if(dim(TEMP)[1]>0 & dim(TEMP)[2]>0) data_extract = rbind(TEMP,data_extract)
        else print(paste("missed",i,":",URL[i]))
        
      }
      
    }
    
    Sys.sleep(min(2,wait_time))
  }
  
  
  return(data_extract)
}


### post.get_data()
### Get posts from a specified URL using JSON
post.get_data <- function(url, ins_comments){
  #print(url)
  #print(paste0("Getting posts from ", url))
  json = tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(error_condition) {
    return(list())
  })
  ### Check if the URL is valid, if a subreddit has been deleted it will fail to get a valid URL
  if(length(json) > 0){
    data <- data.frame(json$data$children$data)
    if(nrow(data) > 0){
      ### Escape text
      
      # if(is.null(data$selftext)) { data$selftext <- "" }
      # if(is.null(data$author_fullname)) { data$author_fullname <- "t2_t1234" }
      if(!("selftext" %in% colnames(data))) data$selftext <- ""
      if(!("author_fullname" %in% colnames(data))) data$author_fullname <- "t2_t1234"
      data <- data %>% 
        mutate(title = escape_strings(title), 
               selftext = escape_strings(selftext), 
               url = gsub("'","" , url , ignore.case = TRUE)) %>%
        mutate(url = gsub("\\\\", "/", url))
      ### Obtain the user id from the author
      ### If the author has been deleted and there is only one post, then the column author_fullname isn't present
      ### If there are several authors and all them have been deleted, then the column author_fullname isn't present
      data$author_id <- sapply(strsplit(data$author_fullname,"_"), `[`, 2)
      data$author_id <- data$author_id %>% replace_na("t1234")
      ### Obtain the subreddit id 
      data$subreddit_id <- mapply(subreddit.get_id, data$subreddit, data$subreddit_subscribers)
      #data$subreddit_id <- sapply(data$subreddit, function(x) subreddit.get_id(x))
      ### Check if the post exists
      data$post_exists <- sapply(data$id, function(x) post.check_if_exists(x))
      ### If the post already exists don't import it again, we separate the non existing from the dataframe
      ### 1st check
      ### If the amount of posts that dont exist are equal to the rows of the dataframe 
      ### it means none are present in the DB, so there is no need to split them
      ### 2nd check
      ### If in the column post_exists everything is TRUE then the posts are already present in the DB, 
      false_values <- length(data$post_exists[data$post_exists == F])
      data_all <- post.get_df(data)
      print(paste0("Getting information from authors: ", paste0(head(data$author), collapse = ", "), ", ..."))
      
      if(false_values == nrow(data)){
        
        post_get_user_info(data)
        post.insert_in_db(data_all)
        post.get_awards(data)
        if(isTRUE(ins_comments)) post.get_comments(data)
      }
      else if(!all(data$post_exists)){
        data1 <- split(data, data$post_exists)
        post_exists <- post.get_df(data1[[2]])
        post_not_exists <- post.get_df(data1[[1]])
        ### We return all the posts in the correct structure
        data_all <- rbind(post_not_exists, post_exists)
        ### Get the information from the author & insert it in the DB
        post_get_user_info(post_not_exists)
        post.insert_in_db(post_not_exists)
        post.get_awards(data)
        if(isTRUE(ins_comments)) post.get_comments(post_not_exists)
      } 
      
      return(data_all)
    }
  }
  
}


### post.get_df()
### Get post dataframe
post.get_df <- function(data){
  ratio <- 0
  if(!is.null(data$upvote_ratio))
    ratio <- data$upvote_ratio
  post <- data.frame(id = data$id, 
                     title = data$title, 
                     upvote_ratio = ratio, 
                     ups = data$ups, 
                     total_awards_received = data$total_awards_received, 
                     score = data$score, 
                     created = anytime(data$created), 
                     date_added = Sys.time(),
                     permalink = data$permalink,
                     url = data$url,
                     domain = data$domain, 
                     subreddit_id = data$subreddit_id, 
                     author = data$author,
                     author_id = data$author_id,
                     subreddit = data$subreddit)
  return(post)
}


### post_get_user_info()
### Get user information from the post
post_get_user_info <- function(post){
  #print(post)
  # sapply(post$author, function(x){
  #   print(paste0("Getting information from author ", x))
  #   info <- user.get_info(x)
  #   info$id <- post$author_id
  #   print(info)
  #   user.insert_in_db(info)
  # })
  for(row in 1:nrow(post)){
    #print(paste0("Getting information from author ", post[row,]$author))
    info <- user.get_info(post[row,]$author)
    info$id <- post[row,]$author_id
    #print(info)
    user.insert_in_db(info)
  }
}



###################################### DATABASE OPERATIONS ######################################  


### post.get_comments()
### Insert comments from a corresponding post
post.get_comments <- function(data){
  #print(data)
  for(row in 1:nrow(data)){
    #print(data[row,])
    url <- paste(c("https://www.reddit.com/r/", data[row,]$subreddit, "/comments/", data[row,]$id, "/"), collapse="")
    print(paste0("Gettings comments from ", url))
    #df <- reddit_content(url)
    df <- post.get_comments_api(url)
    comments <- df[, c("structure", "comm_date", "comment_score", "comment", "URL", "user", "post_text")]
    comments <- comments[comments$user != "AutoModerator",]
    #print(nrow(comments))
    if(nrow(comments) > 0){
      comments <- comments %>%
        mutate(comm_date = as.Date(comm_date, format= "%d-%m-%y"), 
               post_id = strsplit(URL, "/")[[1]][7], 
               comment = escape_strings(comment),
               author = user)
      print(paste0("Getting information from users: ", paste0(head(comments$user), collapse = ", "), ", ..."))
      comments$user_id <- sapply(comments$user, function(x){
        info <- user.get_info(x)
        #print(info)
        user.insert_in_db(info)
        info$id
      })
      #post_get_user_info(comments)
      
      post.update_description(comments[1,]$post_id, escape_strings(comments[1,]$post_text))
      
      values <- paste0("('", 
                       comments$post_id, "', '", 
                       comments$user_id, "', '", 
                       comments$structure, "', '",  
                       comments$comm_date, "', ",
                       comments$comment_score, ", '",
                       comments$comment, "')", collapse = ", ")
      #print(values)
      query <- paste0("INSERT IGNORE INTO comment VALUES ", values, ";")
      db.insert(query)
    }
    
  }
}


### post.get_awards()
### Insert awards from a corresponding post
post.get_awards <- function(data){
  
  new_data <- data[data$post_exists == FALSE,]
  if(length(new_data$all_awardings) > 0){
    coin <- data.frame(id = character(), name = character(), description = character(),
                       coin_price = integer(), coin_reward = integer())
    award_post <- data.frame(post_id = character(), coin_id = character(), quantity = integer())
    for(row in 1:length(new_data$all_awardings)){
      # print(row)
      # print(new_data[row, ]$id)
      if(!is.null(nrow(new_data[row, ]$all_awardings[[1]]))  &&
         nrow(new_data[row, ]$all_awardings[[1]]) > 0){
        #if(isFALSE(new_data[row, ]$post_exists) && nrow(new_data[row, ]$all_awardings[[1]]) > 0){
        #print(new_data[row, ])
        #print(new_data$all_awardings[[row]]$id)
        #print(new_data$all_awardings[[row]]$count)
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


### post.check_if_exists()
### Check if posts exists in database
post.check_if_exists <- function(id){
  query <- paste("SELECT id FROM post WHERE id='",tolower(id),"';", sep="")
  return(db.check_if_exists(query))
}


### post.check_if_exists()
### Updates a post text
post.update_description <- function(id, text){
  query <- paste0("UPDATE post SET selftext = '", text ,"' WHERE id = '", id ,"';")
  db.insert(query)
}


### post.insert_in_db()
### Insert post into the database
post.insert_in_db <- function(post){
  #str(post)
  values <- paste0("('", 
                   post$id, "', '", 
                   post$title, "', '',",   
                   post$upvote_ratio, ", ",
                   post$ups, ", ",
                   post$total_awards_received, ", ",
                   post$score, ", '",
                   post$created, "', '",
                   post$date_added, "', '",
                   post$permalink, "', '",
                   post$url, "', '",
                   post$domain, "', ",
                   post$subreddit_id, ", '",
                   post$author_id,"')", collapse = ", ")
  query <- paste0("INSERT IGNORE INTO post VALUES ", values, ";")
  #print(values)
  #print(query)
  db.insert(query)
}


### award.insert_in_db()
### Insert awards from a post into the database
award.insert_in_db <- function(awards){
  values <- paste0("('", awards$post_id, "','", awards$coin_id, "',", awards$quantity, ")", collapse = ", ")
  query <- paste0("INSERT IGNORE INTO awards_post VALUES ", values, ";")
  db.insert(query)
  #print(query)
}


### coin.insert_in_db()
### Insert coins into the database
coin.insert_in_db <- function(coin){
  #coin <- data.frame(data$id, data$name, data$description, data$coin_price, data$coin_reward)
  #coin_post <- data.frame(post_id = post, coin_id = data$id, quantity = data$count)
  values <- paste0("('", 
                   coin$id, "','", 
                   coin$name, "','", 
                   coin$description, "',", 
                   coin$coin_price, ",", 
                   coin$coin_reward,")", 
                   collapse = ", ")
  query <- paste0("INSERT IGNORE INTO coin VALUES ", values, ";")
  db.insert(query)
}



###################################### GRAPHS ######################################  


### post.construct_graph()
### Construct a graph from a post using RedditExtractor.user_network()
post.construct_graph <- function(url){
  user <- user_network(url, include_author = TRUE, agg = TRUE)
  user$plot
}


### post.construct_graph_alt()
### Construct a graph from a post using ggnet2
post.construct_graph_alt <- function(url){
  data = reddit_content(url)
  data <- data[-which(data[,10] == "[deleted]"),]
  data$grp_com <- as.factor(sapply(strsplit(data[,2],"_"), `[`, 1))
  graph <- construct_graph(data, plot=F)
  g_network <- asNetwork(graph)
  y = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]
  ggnet2(g, label = c("user"), color="grp_com", 
         palette = y, alpha = 0.5,  edge.alpha = 0.5, label.size = 2, 
         legend.size = 6, legend.position = "bottom")
}




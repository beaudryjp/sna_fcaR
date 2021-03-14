source(paste0(getwd(), "/src/", "reddit_general.R"))
source(paste0(getwd(), "/src/", "reddit_user.R"))

# post_get_comments <- function(post_id, permalink){
#   base_url = reddit_get_url("comments")
#   url <- paste(c(base_url, permalink), collapse="");
#   comments <- reddit_content(url)
#   user <- user_get_info(comments$user)
#   data <- data.frame()
# }

### post_get_comments()
### Get comments from a specified post
### Based on function RedditExtractorR.reddit_content(), modified to decrease the limit
post_get_comments <- function(URL, wait_time = 2, sortby = "top", limit = 100){
  
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

### post_get_data()
### Get posts from a specified URL using JSON
post_get_data <- function(url){
  #print(url)
  print(paste0("Getting posts from ", url))
  json <- jsonlite::fromJSON(url)
  data <- data.frame(json$data$children$data)
  data <- data %>%
    mutate(title = escape_strings(title), selftext = escape_strings(selftext))
  data$author_id <- sapply(strsplit(data$author_fullname,"_"), `[`, 2)
  data$subreddit_id <- sapply(data$subreddit, function(x) subreddit_get_id(x))
  sapply(data$author, function(x){
    print(paste0("Getting information from author ", x))
    info <- user_get_info(x)
    user_insert_db(info)
  })
  
  coin <- data.frame(id = character(), name = character(), description = character(), 
                     coin_price = integer(), coin_reward = integer())
  award_post <- data.frame(post_id = character(), coin_id = character(), quantity = integer())
  # print(data$id)
  # print(data$title)
  # print(data$upvote_ratio)
  # print(data$total_awards_received)
  # print(data$score)
  # print(data$created)
  # print(data$permalink)
  # print(data$url)
  # print(data$domain)
  # print(data$subreddit)
  # print(data$author_id)
  post <- data.frame(id = data$id, 
                     title = data$title, 
                     upvote_ratio = data$upvote_ratio, 
                     ups = data$ups, 
                     total_awards_received = data$total_awards_received, 
                     score = data$score, 
                     created = anytime(data$created), 
                     permalink = data$permalink,
                     url = data$url,
                     domain = data$domain, 
                     subreddit_id = data$subreddit_id, 
                     author_id = data$author_id)
  #str(post)
  
  for(row in 1:length(data$all_awardings)){
    #print(data[row, ]$id)
    #print(data$all_awardings[[row]]$id)
    award_post <- award_post %>%
      add_row(post_id = data[row, ]$id,
              coin_id = data$all_awardings[[row]]$id,
              quantity = data$all_awardings[[row]]$count)
    coin <- coin %>%
      add_row(id = data$all_awardings[[row]]$id,
              name = gsub("'","" , data$all_awardings[[row]]$name ,ignore.case = TRUE),
              description = escape_strings(data$all_awardings[[row]]$description),
              coin_price = data$all_awardings[[row]]$coin_price,
              coin_reward = data$all_awardings[[row]]$coin_reward)
  }
  coin_insert_db(coin %>% distinct())
  award_insert_db(award_post %>% distinct())
  post_insert_db(post)
  #post
  post_insert_comments(post)
  return(post)
}

### post_get_listing()
### Get posts by subreddit & listing
post_get_listing <- function(subreddit, listing, limit=25, timeframe){
  base_url = reddit_get_url("")
  url <- paste(c(base_url, subreddit, "/", listing, ".json", "?limit=", limit, "&t=", timeframe), collapse = "")
  post_get_data(url)
}


### post_find_by_query()
### Get posts by search query using RedditExtractor.reddit_urls()
post_find_by_query <- function(query, regex="", subrreddit=NA, sort="relevance"){
  data <- reddit_urls(search_terms = query, regex_filter = regex, subreddit = subrreddit,
                      cn_threshold = 0, page_threshold = 1, sort_by = sort,
                      wait_time = 2)
}

### post_find_by_query_alt()
### Get posts by search query using JSON
post_find_by_query_alt  <- function(query, subreddit, listing, limit=25, timeframe, restrict){
  base_url <- reddit_get_url("")
  url <- paste(c(base_url, subreddit, "/search/.json", "?q=", query, "&restrict_sr=", restrict, 
                 "&sort=", listing, "&t=", timeframe, "&limit=", limit), collapse = "")
  post_get_data(url)
}

### post_insert_comments()
### Insert comments from a corresponding post
post_insert_comments <- function(data){
  base_url = reddit_get_url("comments")
  print("Getting information from the users in the comments.")
  for(row in 1:nrow(data)){
    #print(data[row,])
    url <- paste(c(base_url, data[row,]$permalink), collapse="")
    print(url)
    #df <- reddit_content(url)
    df <- post_get_comments(url)
    comments <- df[, c("structure", "comm_date", "comment_score", "comment", "URL", "user", "post_text")]
    comments <- comments %>%
      mutate(comm_date = as.Date(comm_date, format= "%d-%m-%y"), 
             post_id = strsplit(URL, "/")[[1]][7], 
             comment = escape_strings(comment))
    comments$user_id <- sapply(comments$user, function(x){ 
      info <- user_get_info(x)
      user_insert_db(info)
      info$id
    })
    
    db_settings <- mariadb_get_settings("reddit")
    database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
    post_text <- escape_strings(comments[1,]$post_text)
    post_id <- comments[1,]$post_id
    query <- paste0("UPDATE post SET selftext = '", post_text ,"' WHERE id = '", post_id ,"';")
    rs_insert <- dbSendQuery(database, query)
    dbClearResult(rs_insert)
    dbDisconnect(database)
    
    db_settings <- mariadb_get_settings("reddit")
    database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
    values <- paste0("('", 
                     comments$post_id, "', '", 
                     comments$user_id, "', '", 
                     comments$structure, "', '",  
                     comments$comm_date, "', ",
                     comments$comment_score, ", '",
                     comments$comment, "')", collapse = ", ")
    #print(values)
    query <- paste0("INSERT IGNORE INTO comment VALUES ", values, ";")
    rs_insert <- dbSendQuery(database, query)
    dbClearResult(rs_insert)
    dbDisconnect(database)
  }
}

### post_insert_db()
### Insert post into the database
post_insert_db <- function(post){
  #str(post)
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  values <- paste0("('", 
                   post$id, "', '", 
                   post$title, "', '',",   
                   post$upvote_ratio, ", ",
                   post$ups, ", ",
                   post$total_awards_received, ", ",
                   post$score, ", '",
                   post$created, "', '",
                   post$permalink, "', '",
                   post$url, "', '",
                   post$domain, "', ",
                   post$subreddit_id, ", '",
                   post$author_id,"')", collapse = ", ")
  #print(values)
  query <- paste0("INSERT IGNORE INTO post VALUES ", values, ";")
  rs_insert <- dbSendQuery(database, query)
  dbClearResult(rs_insert)
  dbDisconnect(database)
}


### post_construct_graph()
### Construct a graph from a post using RedditExtractor.user_network()
post_construct_graph <- function(url){
  user <- user_network(url, include_author = TRUE, agg = TRUE)
  user$plot
}

### post_construct_graph_alt()
### Construct a graph from a post using ggnet2
post_construct_graph_alt <- function(url){
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

### award_insert_db()
### Insert awards from a post into the database
award_insert_db <- function(awards){
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  values <- paste0("('", awards$post_id, "','", awards$coin_id, "',", awards$quantity, ")", collapse = ", ")
  query <- paste0("INSERT IGNORE INTO awards_post VALUES ", values, ";")
  rs_insert <- dbSendQuery(database, query)
  dbClearResult(rs_insert)
  dbDisconnect(database)
}

### coin_insert_db()
### Insert coins into the database
coin_insert_db <- function(coin){
  #coin <- data.frame(data$id, data$name, data$description, data$coin_price, data$coin_reward)
  #coin_post <- data.frame(post_id = post, coin_id = data$id, quantity = data$count)
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  values <- paste0("('", 
                   coin$id, "','", 
                   coin$name, "','", 
                   coin$description, "',", 
                   coin$coin_price, ",", 
                   coin$coin_reward,")", 
                   collapse = ", ")
  query <- paste0("INSERT IGNORE INTO coin VALUES ", values, ";")
  rs_insert <- dbSendQuery(database, query)
  dbClearResult(rs_insert)
  dbDisconnect(database)
}

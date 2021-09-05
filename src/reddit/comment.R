
### Get comments from a specified post
### Based on function RedditExtractorR.reddit_content(), modified to decrease the limit
### @parameters: URL = string, wait_time = integer, sortby = string, limit = integer
### @returns: dataframe
comment.get <- function(URL, wait_time = 2, sortby = "top", limit = 50){
  
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


### Insert comments from a corresponding post
### @parameters: data = dataframe
### @returns: none
comment.process <- function(data){
  k <- 1
  for(row in 1:nrow(data)){
    url <- paste(c("https://www.reddit.com/r/", data[row,]$subreddit, "/comments/", data[row,]$id, "/"), collapse="")
    cat(paste0("\nPost #", k, ", Post ID: ", data[row, ]$id, ", Subreddit: ", data[row,]$subreddit, "\n"))
    print(paste0("Gettings comments from ", url))
    df <- comment.get(url)
    comments <- df[, c("structure", "comm_date", "comment_score", "comment", "URL", "user", "post_text")]
    comments <- comments[!(comments$user=="AutoModerator" | comments$user=="[deleted]") ,]
    comments <- comments[comments$comment != "[deleted]",]
    comments$comment <- lapply(comments$comment, escape_strings)
    if(nrow(comments) > 0){
      processed <- comments %>%
        mutate(comm_date = as.Date(comm_date, format= "%d-%m-%y"), 
               post_id = strsplit(URL, "/")[[1]][7], 
               author = user)
      print(paste0("Getting information from users: ", paste0(head(processed$user), collapse = ", "), ", ...\n\n"))
      processed$user_id <- sapply(processed$user, function(x){
        info <- user.get_info(x)
        user.insert_in_db(info)
        info$id
      })
      
      post.update_description(processed[1,]$post_id, escape_strings(processed[1,]$post_text))
      
      values <- paste0("('", processed$post_id, "', '", 
                       processed$user_id, "', '", 
                       processed$structure, "', '",  
                       processed$comm_date, "', ",
                       processed$comment_score, ", '",
                       processed$comment, "')", collapse = ", ")
      query <- paste0("INSERT IGNORE INTO comment VALUES ", values, ";")
      db.insert(query)
    }
    k = k + 1
  }
}

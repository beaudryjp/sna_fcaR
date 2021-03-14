source(paste0(getwd(), "/src/", "reddit_general.R"))

### user_get_info()
### Get information from a specified user
user_get_info <- function(user){
  #str(user)
  print(paste0("Getting information from user ", user))
  if(user == "[deleted]"){
    data <- data.frame(id = "t1234", name = "[deleted]") 
  }
  else{
    base_url = reddit_get_url("user")
    #print(base_url)
    url <- paste(c(base_url, user ,"/about.json"), collapse="")
    #print(url)
    json <- jsonlite::fromJSON(url)
    #json %>% tbl_df
    #if user doesnt exist it will return the following error Error in open.connection(con, \"rb\"): HTTP error 404.
    if(isFALSE(as.logical(json$data$is_suspended)) || length(as.logical(json$data$is_suspended))==0 ){
      pd <- ""
      if(length(json$data$subreddit) > 0){
        #pd <- sapply(json$data$subreddit$public_description, function(x) escape_strings(x))
        pd <- escape_strings(json$data$subreddit$public_description)
      }
      data <- data.frame(id = json$data$id, 
                         name = json$data$name, 
                         created = anytime(json$data$created), 
                         date_added = Sys.time(), 
                         total_karma = json$data$total_karma, 
                         comment_karma = json$data$comment_karma,
                         public_description = pd, 
                         is_employee = as.logical(json$data$is_employee), 
                         is_gold = as.logical(json$data$is_gold), 
                         is_mod = as.logical(json$data$is_mod),
                         is_suspended = FALSE) 
      #str(data)
      data
    }
    
  }
  
}

### user_insert_db()
### Insert user into the database
user_insert_db <- function(user){
  #user <- user_get_info(user_string)
  # print(user)
  # str(user)
  
  if(isFALSE(as.logical(user$is_suspended))){
    values <- ""
    query <- ""
    db_settings <- mariadb_get_settings("reddit")
    database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
    query <- paste("SELECT id FROM user WHERE id='", user$id,"';", sep="")
    rs_select = dbSendQuery(database,query)
    rows <- dbFetch(rs_select)
    dbClearResult(rs_select)
    dbDisconnect(database)
    # print(rows)
    # print(length(rows))
    # print(nrow(rows))
    #user doesnt exist
    if(nrow(rows) == 0){ 
      database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
      if(user$name == "[deleted]"){
        values <- paste0("('", user$id, "', '", user$name,"')", collapse = ", ") 
        query <- paste0("INSERT IGNORE INTO user (id, name) VALUES ", values, ";")
      }
      else{
        values <- paste0("('", 
                         user$id, "', '", 
                         user$name, "', '", 
                         user$created, "', '",  
                         user$date_added, "', ",
                         user$total_karma, ", ",
                         user$comment_karma, ", '",
                         user$public_description, "', ",
                         user$is_employee, ", ",
                         user$is_gold, ", ",
                         user$is_mod,")", collapse = ", ")
        query <- paste0("INSERT IGNORE INTO user VALUES ", values, ";")
      }
      
      #print(values)
      #print(query)
      rs_insert <- dbSendQuery(database, query)
      dbClearResult(rs_insert)
      dbDisconnect(database)
    }
  }
  
}

### user_get_comments()
### Get comments from a specified user
# user_get_comments <- function(user, sortby="top", limit=100){
#   base_url = reddit_get_url("user")
#   url <- paste(c(base_url, user ,"/comments/.json?sort=", sortby,"&limit=", limit), collapse="")
#   print(url)
#   json <- jsonlite::fromJSON(url)
#   data <- data.frame(json$data$children$data)
# }

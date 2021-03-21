source(paste0(getwd(), "/src/", "reddit_general.R"))

### subreddit_import_list()
### Import list of subreddits 
### Note: It's a not an updated list of subreddits & normally it's executed only once
subreddit_import_list <- function(){
  ### Get subreddits from file
  json <- jsonlite::fromJSON("data/subreddits_without_rslash.json")
  ### Transform the data into a ordered data.frame with unique values
  subreddits <- t(as.data.frame(json))
  subreddits <- cbind(row.names(subreddits), subreddits)
  subreddits <- as.data.frame(subreddits)
  subreddits <- subreddits %>%
    arrange(V1) %>%
    mutate(V1 = tolower(V1))
  subreddits <- subreddits[!duplicated(subreddits$V1),]
  colnames(subreddits) <- c("name", "subs")
  rownames(subreddits) <- c()
  ### Insert in DB
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  values <- paste0(apply(subreddits, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  query <- paste0("INSERT IGNORE INTO subreddit (name, subs) VALUES ", values, ";")
  rs_insert <- dbSendQuery(database, query)
  dbClearResult(rs_insert)
  dbDisconnect(database)
}

### subreddit_get_id()
### Get subreddit id
subreddit_get_id <- function(subreddit, subs=0){
  ### If subreddit doesnt exist, insert it in the DB
  # if(isFALSE(subreddit_check_if_exists(subreddit))){
  #   subreddit_insert_db(subrredit, subs)
  # }
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  query <- paste("SELECT id FROM subreddit WHERE name='",tolower(subreddit),"';", sep="")
  rs_select = dbSendQuery(database,query)
  rows <- dbFetch(rs_select)
  dbClearResult(rs_select)
  dbDisconnect(database)
  return(rows$id)
}

### subreddit_check_if_exists()
### Check if a subreddit exists
subreddit_check_if_exists <- function(name){
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  query <- paste("SELECT id FROM subreddit WHERE name='",tolower(name),"';", sep="")
  rs_select = dbSendQuery(database,query)
  rows <- dbFetch(rs_select)
  dbClearResult(rs_select)
  dbDisconnect(database)
  return(nrow(rows) > 0)
}

### subreddit_get_all()
### Get all subreddits
subreddit_get_all <- function(){
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  #query <- paste("SELECT name FROM subreddit WHERE id > 85;", sep="")
  query <- paste("SELECT name FROM subreddit;", sep="")
  rs_select = dbSendQuery(database,query)
  rows <- dbFetch(rs_select)
  dbClearResult(rs_select)
  dbDisconnect(database)
  if(length(rows) > 0)
    return(rows)
  else
    return(data.frame())
}

### subreddit_insert_db()
### Insert new subreddit
subreddit_insert_db <- function(subrredit, subs){
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  values <- paste0(",'",tolower(subrredit), "', ", subs,")", collapse = ", ")
  query <- paste0("INSERT IGNORE INTO subredit VALUES ", values, ";")
  rs_insert <- dbSendQuery(database, query)
  dbClearResult(rs_insert)
  dbDisconnect(database)
}

### subreddit_auto_populate()
### Gets a list of all the subreddits & gets the top 10 posts
subreddit_auto_populate <- function(limit = 10, listing = "top", tf = "month"){
  subreddits <- subreddit_get_all()
  #subreddits <- subreddits %>% head(160) %>% tail(60)
  posts <- lapply(subreddits$name, function(x) post_get_listing(x, listing, limit, tf))
}

source(paste0(getwd(), "/src/", "reddit_general.R"))

### subrredit_import_list()
### Import list of subreddits 
### Note: It's a not an updated list of subreddits & normally it's executed only once
subredit_import_list <- function(){
  #get subreddits from file
  json <- jsonlite::fromJSON("data/subreddits_without_rslash.json")
  subreddits <- t(as.data.frame(json))
  subreddits <- cbind(row.names(subreddits), subreddits)
  subreddits <- as.data.frame(subreddits)
  subreddits <- subreddits %>% 
    arrange(V1) %>%
    mutate(V1 = tolower(V1))
  subreddits <- cbind(1:(dim(subreddits)[1]), subreddits)
  colnames(subreddits) <- c("id", "name", "subscribers")
  rownames(subreddits) <- c()
  #import into database
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  values <- paste0(apply(subreddits, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  query <- paste0("INSERT IGNORE INTO subreddit VALUES ", values, ";")
  rs_insert <- dbSendQuery(database, query)
  dbClearResult(rs_insert)
  dbDisconnect(database)
}

### subreddit_get_id()
### Get subreddit id
subreddit_get_id <- function(subreddit, subs=0){
  db_settings <- mariadb_get_settings("reddit")
  database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
  query <- paste("SELECT id FROM subreddit WHERE name='",tolower(subreddit),"';", sep="")
  rs_select = dbSendQuery(database,query)
  rows <- dbFetch(rs_select)
  if(length(rows) == 0) subreddit_insert_db(subrredit, subs)
  dbClearResult(rs_select)
  dbDisconnect(database)
  if(length(rows) == 0){
    database <- dbConnect(RMariaDB::MariaDB(), default.file=db_settings$file, group=db_settings$db)
    query <- paste("SELECT id FROM subreddit WHERE name='",tolower(subreddit),"';", sep="")
    rs_select = dbSendQuery(database,query)
    rows <- dbFetch(rs_select)
    dbClearResult(rs_select)
    dbDisconnect(database)
    
  }
  return(rows$id)
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

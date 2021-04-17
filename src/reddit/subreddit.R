source(paste0(getwd(), "/src/", "general.R"))

###################################### DATA IMPORTS ######################################  


### subreddit_auto_populate()
### Gets a list of all the subreddits & gets the top 10 posts
subreddit.auto_populate <- function(limit = 10, listing = "top", tf = "month", comments = TRUE){
  subreddits <- subreddit.get_all()
  #subreddits <- subreddits %>% head(160) %>% tail(60)
  posts <- lapply(subreddits$name, function(x) post.get_by_listing(x, listing, limit, tf, comments))
}


### subreddit_import_list()
### Import list of subreddits 
### Note: It's a not an updated list of subreddits & normally it's executed only once
subreddit.import_list <- function(){
  ### Get subreddits from file
  json <- jsonlite::fromJSON("data/subreddits_without_rslash.json")
  ### Transform the data into a ordered data.frame with unique values
  df <- data.frame(name = names(json), subs = unlist(json))
  df <- df %>%
    arrange(name) %>%
    mutate(name = tolower(name))
  df <- df[!duplicated(df$name),]
  ### Insert in DB
  values <- paste0(apply(df, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  query <- paste0("INSERT IGNORE INTO subreddit (name, subs) VALUES ", values, ";")
  db.insert(query)
}



###################################### DATABASE OPERATIONS ######################################  


### subreddit_check_if_exists()
### Check if a subreddit exists
subreddit.check_if_exists <- function(name){
  query <- paste("SELECT id FROM subreddit WHERE name='",tolower(name),"';", sep="")
  return(db.check_if_exists(query))
}


### subreddit_get_id()
### Get subreddit id
subreddit.get_id <- function(subreddit, subs=0){
  #print(paste0(subreddit, " ", subs))
  if(!subreddit.check_if_exists(subreddit)) subreddit.insert_in_db(subreddit, subs)
  query <- paste("SELECT id FROM subreddit WHERE name='", tolower(subreddit), "';", sep = "")
  #print(query)
  return(db.fetch_all(query)$id)
}


### subreddit_get_all()
### Get all subreddits
subreddit.get_all <- function(){
  query <- paste("SELECT name FROM subreddit WHERE id > 524;", sep="")
  #query <- paste("SELECT name FROM subreddit;", sep = "")
  return(db.fetch_all(query))
}


### subreddit_insert_db()
### Insert new subreddit
subreddit.insert_in_db <- function(subrredit, subs){
  values <- paste0("('", tolower(subrredit), "', ", subs, ")", collapse = ", ")
  query <- paste0("INSERT IGNORE INTO subreddit (name, subs) VALUES ", values, ";")
  #print(query)
  db.insert(query)
}

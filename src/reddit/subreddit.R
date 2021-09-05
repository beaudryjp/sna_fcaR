
###################################### DATA IMPORTS ######################################  

### Gets a list of all the subreddits & gets the top 10 posts
### @parameters: limit = integer, listing = string, tf = string, comments = boolean
### @returns: dataframe
subreddit.auto_populate_all <- function(limit = 10, listing = "top", tf = "month", comments = TRUE){
  subreddits <- subreddit.get_all()
  #subreddits <- subreddits %>% head(15) %>% tail(10)
  posts <- lapply(subreddits$name, function(x){ post.get_by_listing(x, listing, limit, tf, comments)})
}

### Gets the top posts for the specified subreddits
### @parameters: limit = integer, listing = string, tf = string, comments = boolean
### @returns: dataframe
subreddit.populate_from_subreddits <- function(subreddits, limit = 10, listing = "top", tf = "month", comments = TRUE){
  result <- do.call("rbind", lapply(subreddits, function(x){ post.get_by_listing(x, listing, limit, tf, comments) }))
}


### Import list of subreddits 
### @parameters: none
### @returns: none
subreddit.import_list <- function(){
  json <- jsonlite::fromJSON("data/subreddits_without_rslash.json")
  df <- data.frame(name = names(json), subs = unlist(json))
  df <- df %>%
    arrange(name) %>%
    mutate(name = tolower(name))
  df <- df[!duplicated(df$name),]
  values <- paste0(apply(df, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  query <- paste0("INSERT IGNORE INTO subreddit (name, subs) VALUES ", values, ";")
  # database$insert(query)
  db.insert(query)
}


###################################### DATABASE OPERATIONS ######################################  


### Check if a subreddit exists
### @parameters: number = integer
### @returns: TRUE or FALSE
subreddit.check_if_exists <- function(name){
  query <- paste("SELECT id FROM subreddit WHERE name='",tolower(name),"';", sep="")
  # return(database$exists(query))
  return(db.exists(query))
}

### Get subreddit id
### @parameters: subrredit = string, subs = integer
### @returns: integer
subreddit.get_id <- function(subreddit, subs=0){
  if(!subreddit.check_if_exists(subreddit)) subreddit.insert_in_db(subreddit, subs)
  query <- paste("SELECT id FROM subreddit WHERE name='", tolower(subreddit), "';", sep = "")
  # return(database$select(query)$id)
  return(db.fetch_all(query)$id)
}

### Get all subreddits
### @parameters: number = integer
### @returns: dataframe
subreddit.get_all <- function(id = 0){
  query <- paste0("SELECT name FROM subreddit WHERE id > ", id," ORDER BY name ASC;")
  # return(database$select(query))
  return(db.fetch_all(query))
}

### Insert new subreddit
### @parameters: subrredit = string, subs = integer
### @returns: none
subreddit.insert_in_db <- function(subrredit, subs){
  values <- paste0("('", tolower(subrredit), "', ", subs, ")", collapse = ", ")
  query <- paste0("INSERT IGNORE INTO subreddit (name, subs) VALUES ", values, ";")
  # database$insert(query)
  return(db.insert(query))
}

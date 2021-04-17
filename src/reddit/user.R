source(paste0(getwd(), "/src/", "general.R"))

###################################### DATA COLLECTION ######################################  


### user.get_info()
### Get information from a specified user
user.get_info <- function(user){
  #print(user)
  ### If the user has been deleted there is no way of obtaining the data before the deletion
  ### So all the users which have been deleted will be classified as the same
  if(user == "[deleted]"){
    data <- data.frame(id = "t1234", name = "[deleted]") 
  }
  else if(user == "AutoModerator"){
    data <- data.frame(id = "6l4z3", name = "AutoModerator") 
  }
  else{
    #print(paste0("Getting information from user ", user))
    url <- paste(c("https://www.reddit.com/user/", user ,"/about.json"), collapse="")
    pd <- ""
    #print(url)
    ### if user doesnt exist it will return the following error Error in open.connection(con, \"rb\"): HTTP error 404.
    json = tryCatch({
      jsonlite::fromJSON(url)
    }, error = function(error_condition) {
      return(list())
    })
    if(length(json) > 0){
      if(isTRUE(as.logical(json$data$is_suspended))){
        data <- data.frame(id = stri_rand_strings(1, 10), 
                           name = user, 
                           created = NA, 
                           date_added = Sys.time(), 
                           total_karma = json$data$total_karma, 
                           comment_karma = 0,
                           public_description = "", 
                           is_employee = FALSE, 
                           is_gold = FALSE, 
                           is_mod = FALSE,
                           is_suspended = TRUE) 
      }
      else{
        
        if(length(json$data$subreddit) > 0) pd <- escape_strings(json$data$subreddit$public_description)
        
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
      }
      #print(data)
      return(data)
    }
    else{
      return(data.frame(id = "t1234", name = "[deleted]") )
    }
  }
  
}


### user.get_comments()
### Get comments from a specified user
# user.get_comments <- function(user, sortby="top", limit=100){
#   base_url = reddit_get_url("user")
#   url <- paste(c(base_url, user ,"/comments/.json?sort=", sortby,"&limit=", limit), collapse="")
#   print(url)
#   json <- jsonlite::fromJSON(url)
#   data <- data.frame(json$data$children$data)
# }


###################################### DATABASE OPERATIONS ######################################  


### user.insert_in_db()
### Insert user into the database
user.insert_in_db <- function(user){
  values <- ""
  query <- ""
  #print(user)
  ### Check if user exists, if TRUE do nothing
  if(isFALSE(user.check_if_exists(user$id)) && !is.null(user)){ 
    #print(user)
    if(user$name == "[deleted]" || user$name == "AutoModerator"){
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
    
    # print(values)
    # print(query)
    db.insert(query)
  }
  
}


### user.check_if_exists()
### Check if a user exists in the database
user.check_if_exists <- function(id){
  query <- paste("SELECT id FROM user WHERE id='", tolower(id),"';", sep="")
  return(db.check_if_exists(query))
}

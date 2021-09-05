
# STRUCTURES ###################################### 

### Returns the dataframe structure for a deleted user
### @parameters: none
### @returns: dataframe
df_deleted = function() {
  return(data.frame(id = "t1234", name = "[deleted]"))
}

### Returns the dataframe structure for the user AutoModerator
### @parameters: none
### @returns: dataframe
df_automod = function() {
  return(data.frame(id = "6l4z3", name = "AutoModerator"))
}

### Returns the dataframe structure for a suspended user
### @parameters: username = string, total_karma = integer
### @returns: dataframe
df_suspended = function(username, total_karma) {
  data <- data.frame(id = stringi::stri_rand_strings(1, 10),
                     name = username,
                     created = NA,
                     date_added = Sys.time(),
                     total_karma = total_karma,
                     comment_karma = 0,
                     public_description = "",
                     is_employee = FALSE,
                     is_gold = FALSE,
                     is_mod = FALSE,
                     is_suspended = TRUE)
  return(data)
}

### Returns the dataframe structure for an active user
### @parameters: json = dataframe, description = string
### @returns: dataframe
df_active = function(json, description) {
  data <- data.frame(id = json$id,
                     name = json$name,
                     created = anytime::anytime(json$created),
                     #created = format(as.Date(as.POSIXct(json$created_utc,origin="1970-01-01")),"%y-%m-%d"),
                     date_added = Sys.time(),
                     total_karma = json$total_karma,
                     comment_karma = json$comment_karma,
                     public_description = description,
                     is_employee = as.logical(json$is_employee),
                     is_gold = as.logical(json$is_gold),
                     is_mod = as.logical(json$is_mod),
                     is_suspended = FALSE)
  return(data)
}

###################################### DATA COLLECTION ######################################  


### Get information from a specified users
### @parameters: user = string
### @returns: dataframe
user.get_info <- function(user){
  if(!is.null(user)){
    if(user == "[deleted]"){
      return(df_deleted())
    }
    else if(user == "AutoModerator"){
      return(df_automod())
    }
    else{
      url <- paste(c("https://www.reddit.com/user/", user ,"/about.json"), collapse="")
      description <- ""
      json = get_json(url)
      if(length(json) > 0){
        if(isTRUE(as.logical(json$data$is_suspended))){
          return(df_suspended(user, json$data$total_karma))
        }
        else{
          if(length(json$data$subreddit) > 0){
            description <- escape_strings(json$data$subreddit$public_description)
          }
          
          return(df_active(json$data, description))
          
        }
        return(data)
      }
      else{
        return(df_deleted())
      }
    }
  } else{
    return(df_deleted())
  }
  
  
}


### Get comments from a specified user
### @parameters: user = string, sortby = string, limit = integer
### @returns: dataframe
# user.get_comments <- function(user, sortby="top", limit=100){
#   base_url = reddit_get_url("user")
#   url <- paste(c(base_url, user ,"/comments/.json?sort=", sortby,"&limit=", limit), collapse="")
#   print(url)
#   json <- jsonlite::fromJSON(url)
#   data <- data.frame(json$data$children$data)
# }


###################################### DATABASE OPERATIONS ######################################  


### Insert user into the database
### @parameters: user = string
### @returns: none
user.insert_in_db <- function(data){
  values <- ""
  query <- ""
  if(isFALSE(user.check_if_exists(data$id)) && !is.null(data)){ 
    if(data$name == "[deleted]" || data$name == "AutoModerator"){
      values <- paste0("('", data$id, "', '", data$name,"')", collapse = ", ") 
      query <- paste0("INSERT IGNORE INTO user (id, name) VALUES ", values, ";")
    }
    else{
      values <- paste0("('", 
                       data$id, "', '", 
                       data$name, "', '", 
                       data$created, "', '",  
                       data$date_added, "', ",
                       data$total_karma, ", ",
                       data$comment_karma, ", '",
                       data$public_description, "', ",
                       data$is_employee, ", ",
                       data$is_gold, ", ",
                       data$is_mod,")", collapse = ", ")
      query <- paste0("INSERT IGNORE INTO user VALUES ", values, ";")
    }
    
    # print(values)
    # print(query)
    
    #database$insert(query)
    db.insert(query)
  }
  
}


### Check if a user exists in the database
### @parameters: id = string
### @returns: TRUE or FALSE
user.check_if_exists <- function(id){
  #database <- Database$new("reddit")
  query <- paste("SELECT id FROM user WHERE id='", tolower(id),"';", sep="")
  # return(database$exists(query))
  return(db.exists(query))
}

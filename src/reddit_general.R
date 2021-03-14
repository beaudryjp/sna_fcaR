### package_load()
### Install and load R packages
package_load <- function(){
  list.of.packages <- c("tm", "wordcloud", "wordcloud2", "DT", "intergraph", "network", "sna", "scales", 
                        "GGally", "RMariaDB", "curl", "stringr", "textclean", "anytime", "stringi",
                        "jsonlite", "twitteR", "RedditExtractoR","tidyverse", "RColorBrewer","sna")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages) 
  invisible(lapply(list.of.packages, require, character.only = T))
}

### reddit_get_url()
### Returns predefined base url for requesting to the reddit api
reddit_get_url <- function(type){
  case_when(
    type == "comments" ~ "https://www.reddit.com",
    type == "user" ~ "https://www.reddit.com/user/",
    TRUE ~ "https://www.reddit.com/r/"
  )
}

### mariadb_get_settings()
### Returns mariadb configuration
mariadb_get_settings <- function(database){
  if(database == "reddit")(
    list(file=paste(c(getwd(), "/conf/", "sna_reddit.cnf"), collapse = ""), 
         db="sna_reddit")
  )
  else{
    list(file=paste(c(getwd(), "/conf/", "sna_reddit.cnf"), collapse = ""), 
         db="sna_twitter")
  }
}

### escape_strings()
### Removes certain characters from a string
escape_strings <- function(string){
  string <- tolower(string)
  string <- replace_non_ascii(string)
  string <- replace_hash(string)
  string <- replace_html(string)
  string <- replace_internet_slang(string)
  string <- str_replace(string, "@\\w+"," ")
  string <- str_replace(string, "#\\S+"," ")
  string <- str_replace(string, "http\\S+\\s*"," ")
  # string <- str_replace(string, "http[[:alnum:]]*"," ")
  # string <- str_replace(string, "http[[\\b+RT]]"," ")
  string <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", string)
  string <- str_replace(string, "[[:cntrl:]]"," ")
  string <- str_replace(string, "[^[:alnum:]]", " ")
  #string <- removeWords(string, stopwords("spanish"))
  string <- str_replace(string, "[\U00010000-\U0010ffff]", "")
  #string <- chartr("áéóúí", "aeoui", string)
  string <- stri_trans_general(string,id = "Latin-ASCII")
  string <- gsub('[[:punct:] ]+',' ',string)
  string <- gsub('[[:digit:] ]+',' ',string)
  #string <- paste(Filter(function(x) nchar(x) > 1, unlist(strsplit(string, "\\s+"))), collapse=" ") # remove single letters
  string <- str_trim(string)
  return(string)
}

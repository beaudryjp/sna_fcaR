###################################### DATA MANIPULATIONS ######################################  

### escape_strings()
### Removes certain characters from a string
escape_strings <- function(string, to_lower = TRUE){
	if(isTRUE(to_lower)) string <- tolower(string)
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

round_to_1st_digit <- function(number){
	result <- 0
	### Check if the number has decimals, if TRUE the number is an Integer
	#print(paste0(number, " ", round(number)))
	if(number == round(number)){
		num_digits <- floor(log10(abs(number))) 
		result <- round(number, -num_digits)
	}
	else
		result <- round(number/0.1) * 0.1
	return(result)
}

###################################### GENERAL DATABASE OPERATIONS ######################################  

db.insert = function(query){
	db <- Database$new("reddit")
	db$insert(query)
	rm(db)
}

db.check_if_exists = function(query){
	db <- Database$new("reddit")
	result <- db$select(query)
	rm(db)
	return(nrow(result) > 0)
}

db.fetch_all = function(query){
	db <- Database$new("reddit")
	result <- db$select(query)
	rm(db)
	return(result)
}
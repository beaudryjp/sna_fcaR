
### Removes certain characters from a string
### @parameters: string = string, to_lower = boolean
### @returns: string
escape_strings <- function(string, to_lower = TRUE){
    string <- textclean::replace_contraction(string)
    string <- textclean::replace_kern(string)
    string <- textclean::replace_number(string)
    string <- textclean::replace_non_ascii(string)
    string <- textclean::replace_hash(string)
    string <- textclean::replace_html(string)
    string <- textclean::replace_internet_slang(string)
    string <- stringr::str_replace(string, "@\\w+"," ")
    string <- stringr::str_replace(string, "#\\S+"," ")
    string <- stringr::str_replace(string, "http\\S+\\s*"," ")
    string <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", string)
    string <- stringr::str_replace(string, "[[:cntrl:]]"," ")
    string <- stringr::str_replace(string, "[^[:alnum:]]", " ")
    string <- stringr::str_replace(string, "[\U00010000-\U0010ffff]", "")
    string <- stringi::stri_trans_general(string,id = "Latin-ASCII")
    string <- gsub("'", '', string)
    string <- gsub("\"", "", string)
    string <- gsub('[[:punct:] ]+',' ',string)
    string <- gsub('[[:digit:] ]+',' ',string)
    string <- gsub("[^[:alnum:] ]", "", string)
    if(isTRUE(to_lower)) string <- tolower(string)
    return(stringr::str_trim(string))
}

### Obtains a JSON file from an URI
### @parameters: url = string
### @returns: dataframe
get_json <- function(url){
    result = tryCatch({
        jsonlite::fromJSON(url)
    }, error = function(error_condition) {
        return(list())
    })
}

### Creates a matrix for each transaction establishing 1 if an attribute is present
### @parameters: cols = vector, data = dataframe
### @returns: dataframe
create_binary_matrix <- function(cols, data){
    length_cols <- length(colnames(data))
    length_rows <- length(rownames(data))
    output <- data.frame(matrix(ncol = length(cols), nrow = 0))
    colnames(output) <- cols
    
    for(i in 1:length_rows){
        for(k in 1:length_cols){
            colname <- paste0(colnames(data)[k], "-", data[i, k])
            other <- paste0(colnames(data)[k], "-other")
            if(colname %in% cols){
                output[i, colname] <- 1
            } else{
                output[i, other] <- 1
            }
        }
    }
    output[is.na(output)] <- 0
    output[sapply(output, is.numeric)] <- lapply(output[sapply(output, is.numeric)], as.factor)
    return(output)
}

### Get frequent terms from a column in a dataset
### @parameters: column = string
### @returns: DocumentTermMatrix, dataframe
freq_terms_from_data = function(column){
    stopwords <- readLines(con = "data/stopwords-en.txt")
    column <- tm::removeWords(column, stopwords)
    column <- tm::stripWhitespace(column)
    
    corpus <- tm::Corpus(tm::VectorSource(column))
    ### if we want to use stemming
    #post_cp_stemmed <-  tm_map(post_corpus, stemDocument)
    #dtm <- DocumentTermMatrix(post_cp_stemmed); dtm
    dtm <- tm::DocumentTermMatrix(corpus)
    
    ### we use a sparse matrix since its faster
    m <-  Matrix::sparseMatrix(i=dtm$i, 
                               j=dtm$j, 
                               x=dtm$v, 
                               dims=c(dtm$nrow, dtm$ncol),
                               dimnames = dtm$dimnames)
    freq <- colSums(m)
    
    freq.dt <- as.data.frame(freq)
    freq.dt$term <- row.names(freq.dt)
    row.names(freq.dt) <- 1:NROW(freq.dt)
    
    freq.dt$term = tm::removeWords(freq.dt$term, stopwords)
    
    ### we remove text in which we have continous letters
    ### example: aaaaaaaaaa
    p <- " *\\b(?:[[:alnum:]]*([[:alnum:]])\\1{3}[[:alnum:]]*|[0-9]+)\\b"
    freq.dt$term <- gsub(p, "", freq.dt$term)
    freq.dt <- freq.dt %>% filter(term != "")
    
    return(list(dtm, freq.dt))
}

### Find correlated terms
### @parameters: dtm = DocumentTermMatrix, column = string, cor = double
### @returns: ggplot graph
find_correlated_terms = function(dtm, column, cor){
    assocs <- tm::findAssocs(dtm, column, cor)
    assocs.list <- lapply(assocs, function(x) data.frame(rhs = names(x), cor = x, stringsAsFactors = F))
    assocs.df <- dplyr::bind_rows(assocs.list, .id = "lhs")
}

### Returns string for a filename
### @parameters: none
### @returns: string
get_filename <- function(){
	return(paste0("sna_fcaR_", as.numeric(Sys.time())))
}

### Activates the debug for the shiny app if the constant is set
### @parameters: none
### @returns: none
shiny_debug <- function(debug){
    if(isTRUE(debug)){
        options(shiny.trace = TRUE)
        options(shiny.fullstacktrace = TRUE)
        options(shiny.reactlog = TRUE)
    }
}

###################################### GENERAL DATABASE OPERATIONS WHEN NOT USING POOL ######################################  

### Insert query
### @parameters: query = string
### @returns: none
db.insert <- function(query){
    database <- Database$new("reddit")
    database$insert(query)
}

### Check if something exists
### @parameters: query = string
### @returns: TRUE or FALSE
db.exists <- function(query){
    database <- Database$new("reddit")
    return(database$exists(query))
}

### Get all the data
### @parameters: query = string
### @returns: dataframe
db.fetch_all <- function(query){
    database <- Database$new("reddit")
    return(database$select(query))
}


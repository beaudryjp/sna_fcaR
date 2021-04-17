###################################### IMPORTS ######################################  

package_load = function(){
  # Package names
  packages <- c("R6", "tm", "jsonlite","wordcloud", "wordcloud2", "tictoc", "dtplyr", "intergraph", "network", "sna", "data.table", "scales", 
                "arules", "arulesViz", "GGally", "RMariaDB", "curl", "stringr", "textclean", "anytime", "stringi", "fcaR",  "twitteR", 
                "RedditExtractoR","tidyverse", "RColorBrewer", "Matrix", "BiocManager", "visNetwork", "networkD3",
                "shiny", "shinythemes", "shinyjs", "spsComps", "SnowballC")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) install.packages(packages[!installed_packages])
  if(isFALSE("Rgraphviz" %in% rownames(installed.packages()))) BiocManager::install("Rgraphviz")
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}
package_load()
#options('max.print' = 100000)

### Compile source files
source(paste0(getwd(), "/src/", "database.R"))
source(paste0(getwd(), "/src/", "general.R"))
source(paste0(getwd(), "/src/reddit/", "post.R"))
source(paste0(getwd(), "/src/reddit/", "subreddit.R"))
source(paste0(getwd(), "/src/reddit/", "user.R"))


###################################### FUNCTION CONFLICTS ######################################  

# -- Conflicts ---------------------------------------- tidyverse_conflicts() --
#   x ggplot2::annotate() masks NLP::annotate()
# x purrr::as_vector()  masks fcaR::as_vector()
# x dplyr::between()    masks data.table::between()
# x readr::col_factor() masks scales::col_factor()
# x purrr::discard()    masks scales::discard()
# x tidyr::expand()     masks Matrix::expand()
# x dplyr::filter()     masks stats::filter()
# x dplyr::first()      masks data.table::first()
# x purrr::flatten()    masks jsonlite::flatten()
# x dplyr::id()         masks twitteR::id()
# x dplyr::lag()        masks stats::lag()
# x dplyr::last()       masks data.table::last()
# x dplyr::location()   masks twitteR::location()
# x tidyr::pack()       masks Matrix::pack()
# x readr::parse_date() masks curl::parse_date()
# x dplyr::recode()     masks arules::recode()
# x purrr::transpose()  masks data.table::transpose()
# x tidyr::unpack()     masks Matrix::unpack()
# 
# Attaching package: ‘shiny’
# 
# The following object is masked from ‘package:jsonlite’:
#   
#   validate
# 
# Don't forget that shinyjs can also be used in Rmd documents!
# 
# Attaching package: ‘shinyjs’
# 
# The following object is masked from ‘package:shiny’:
# 
#     runExample
# 
# The following object is masked from ‘package:twitteR’:
# 
#     show
# 
# The following object is masked from ‘package:RMariaDB’:
# 
#     show
# 
# The following objects are masked from ‘package:arules’:
# 
#     info, show
# 
# The following object is masked from ‘package:Matrix’:
# 
#     show
# 
# The following objects are masked from ‘package:methods’:
# 
#     removeClass, show
# 
# Loading required package: spsUtil



###################################### CODE ######################################  

subreddit.auto_populate(10, "top", "all", FALSE)

subreddit.auto_populate(10, "top")

a <- post.get_by_listing("entertainment", "top", 10, "month", TRUE)

c <- post.get_by_listing("popular", "top", 100, "day", TRUE)

b <- post.get_by_listing("popular", "hot", 100, "day", TRUE)

b <- b %>% select(id, upvote_ratio, ups, total_awards_received, score, domain, author, subreddit)

b[sapply(b, is.integer)] <- lapply(b[sapply(b, is.integer)], as.numeric)
b[sapply(b, is.character)] <- lapply(b[sapply(b, is.character)], as.factor)

b[is.na(b)] <- 0
b <- discretizeDF(b, default = list(method = "interval", breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High")))

b_df <- b %>% select(-id, -author) %>% correlationfunnel::binarize(thresh_infreq = 0.00001, one_hot = T, name_infreq = "other")

fc <- FormalContext$new(b_df)
fc$find_implications()
fc$implications$cardinality()

fc2 <- fc$reduce(TRUE)
fc2$find_implications()
fc2$implications$cardinality()
fc2$implications$apply_rules(rules = c("reduction", "composition", "generalization",  "simplification"))

# Define a set of attributes
S <- SparseSet$new(attributes = fc2$attributes)
S$assign(subreddit__worldnews = 1, upvote_ratio__Very_High = 1)

# Compute the closure of S
Sc <- fc2$closure(S)
# Is Sc a closed set?
fc2$is_closed(Sc)

fc2$implications$recommend(S, attribute_filter = fc2$attributes)

###################################### COMMENTS ######################################  

query2 <- paste0("SELECT
post.id, 
comment.score, comment.comment, 
user.name as user, user.total_karma as user_total_karma, user.is_employee as user_is_employee, user.is_mod as user_is_mod, user.is_gold as user_is_gold, 
subreddit.name as subreddit
FROM
sna_reddit.comment, sna_reddit.post, sna_reddit.subreddit, sna_reddit.user
WHERE
sna_reddit.comment.post_id = sna_reddit.post.id AND
sna_reddit.comment.user_id = sna_reddit.user.id AND
sna_reddit.post.subreddit_id = sna_reddit.subreddit.id AND
post.id IN ", paste0("('", paste0(b$id, collapse="', '"), "')") , ";")

#print(str_replace_all(query2, "[\r\n]" , " "))

comments <- db.fetch_all(query2)

comments[is.na(comments)] <- 0

comments_df <- comments

comments <- comments %>% select(-id, -user, -comment)

comments$user_is_employee <- as.logical(comments$user_is_employee)
comments$user_is_mod <- as.logical(comments$user_is_mod)
comments$user_is_gold <- as.logical(comments$user_is_gold)

cols <- colnames(comments)[-c(1:2)]
comments[cols] <- lapply(comments[cols], factor)
comments <- discretizeDF(comments, default = list(method = "interval", breaks = 5, 
                                                  labels = c("Very Low", "Low", "Medium", "High", "Very High")))

comments_binary <- comments %>% correlationfunnel::binarize(thresh_infreq = 0.00001, one_hot = T, name_infreq = "other")

fc <- FormalContext$new(comments_binary)
fc$find_implications()
fc$implications$cardinality()
fc$reduce()
fc$implications$apply_rules()
fc$find_implications()
fc$implications$cardinality()

###################################### AWARDS THE POSTS RETREIVED ###################################### 


query1 <- paste0("
SELECT
subreddit.name AS subreddit, coin.name AS coin, awards_post.quantity, coin.coin_price, coin.coin_price * awards_post.quantity as cost
FROM
sna_reddit.post, sna_reddit.awards_post, sna_reddit.coin, sna_reddit.subreddit
WHERE
post.id = awards_post.post_id AND coin.id = awards_post.coin_id AND post.subreddit_id = subreddit.id AND
post.id IN ", paste0("('", paste0(b$id, collapse="', '"), "')") , "
ORDER BY subreddit;")
awards <- db.fetch_all(query1)
awards$coin <- sapply(awards$coin, function(x) escape_strings(x, FALSE))

awards[sapply(awards, is.integer)] <- lapply(awards[sapply(awards, is.integer)], as.numeric)
awards[sapply(awards, is.character)] <- lapply(awards[sapply(awards, is.character)], as.factor)

awards[is.na(awards)] <- 0
awards <- discretizeDF(awards, default = list(method = "interval", breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High")))

aw_df <- awards %>% correlationfunnel::binarize(thresh_infreq = 0.00001, one_hot = T, name_infreq = "other")

fc_a <- FormalContext$new(aw_df)
fc_a$find_implications()
fc_a$implications$cardinality()
fc_a$implications$apply_rules(rules = c("reduction", "composition", "generalization",  "simplification"))

fc2_a <- fc_a$reduce(TRUE)
fc2_a$find_implications()
fc2_a$implications$cardinality()
fc2_a$implications$apply_rules(rules = c("reduction",  "simplification"))

# Define a set of attributes
S <- SparseSet$new(attributes = fc_a$attributes)

###################################### AWARDS FROM ALL POSTS ###################################### 


query4 <- paste("
SELECT
       subreddit.name AS subreddit, coin.name AS coin, awards_post.quantity, coin.coin_price * awards_post.quantity as cost
FROM
     sna_reddit.post, sna_reddit.awards_post, sna_reddit.coin, sna_reddit.subreddit
WHERE
      post.id = awards_post.post_id AND coin.id = awards_post.coin_id AND post.subreddit_id = subreddit.id
ORDER BY subreddit;")
awards <- db.fetch_all(query4)

awards <- discretizeDF(awards, default = list(method = "interval", breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High")))

# awards$quantity <- ordered(cut(awards[["quantity"]], 
#                                seq(from = 0, 
#                                    to = max(awards$quantity), 
#                                    by = max(awards$quantity) / length(labls)), labels=labls))

awards[sapply(awards, is.character)] <- lapply(awards[sapply(awards, is.character)], as.factor)

# awards[sapply(awards, is.numeric)] <- lapply(awards[sapply(awards, is.numeric)], as.numeric)
# awards$quantity <- sapply(awards$quantity, function(x) round_to_1st_digit(x))
# awards$cost <- sapply(awards$cost, function(x) round_to_1st_digit(x))
# awards[sapply(awards, is.numeric)] <- lapply(awards[sapply(awards, is.numeric)], as.factor)

transactions3 <- as(awards, "transactions")
rules3 <- apriori(awards, parameter = list(supp = 0.05, conf = 0.4))
rules3 <- rules3[!is.redundant(rules3)]
inspect(rules3)

###################################### POSTS FROM DB ######################################  

query3 <- paste("SELECT
user.name AS user, user.total_karma, 
post.title, post.selftext, post.upvote_ratio, post.total_awards_received, post.score, post.created, post.domain,
subreddit.name AS subreddit
FROM post, user, subreddit
WHERE post.user_id = user.id AND post.subreddit_id = subreddit.id;", sep="")
posts <- db.fetch_all(query3)
posts_df <- posts

#posts <- posts %>% select(-title, -created)
posts[sapply(posts, is.integer)] <- lapply(posts[sapply(posts, is.integer)], as.numeric)
posts[sapply(posts, is.character)] <- lapply(posts[sapply(posts, is.character)], as.factor)

posts[is.na(posts)] <- 0

posts <- discretizeDF(posts, default = list(method = "interval", breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High")))
#posts <- posts[!is.na(posts)]

tic("sleeping")
### 22 - 25 sec
a <- posts %>%
  select(-user, -title, -selftext, -created) %>% 
  correlationfunnel::binarize(thresh_infreq = 0.00001, one_hot = T, name_infreq = "other")
toc()
a <- a %>% select(-contains("domain__self"))

### <262.14 sec
a <- as.data.frame(a)
tic("sleeping"); a[sapply(a, is.numeric)] <- lapply(a[sapply(a, is.numeric)], as.factor); toc()

#tic("sleeping"); trans <- as(a, "transactions"); toc()

tic("sleeping"); rules <- apriori(a,  parameter = list(conf = 1, minlen = 2)); toc()

rules <- rules[!is.redundant(rules)]
rules_conf <- sort(rules, by = "confidence")
rules_sig <- rules[is.significant(rules_conf)]
rules_lift <- sort(rules, by = "lift")
inspect(head(rules_conf))

is <- ImplicationSet$new(rules)
is$apply_rules(rules = c("composition", "simplification"))

#is$apply_rules(rules = c("reduction", "composition", "generalization",  "simplification"))


###################################### TEXT MINING TITLES FROM ALL POSTS ###################################### 

# Number of rows
nrow(posts_df)

# Number of unique users
length(unique(posts_df$user))

# Top users with posts
posts_df %>%
  filter(user != c('AutoModerator')) %>%
  group_by(user) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)

data <- freq_terms_from_data(posts_df$title); dtm <- data[[1]]; freq.dt <- data[[2]]

freq.mean <- mean(freq.dt$freq); freq.mean

over.mean <- freq.dt[freq.dt$freq > freq.mean,] 
top_overmean <- over.mean %>% arrange(desc(freq)) %>% head(400)
colnames(top_overmean) <- c("freq","word")
top_overmean <- top_overmean %>% select(word, everything())

over.mean %>%
  arrange(desc(freq)) %>%
  head(30) %>%
  mutate(term = reorder(term, freq)) %>%
  ggplot(aes(term, freq)) + 
  geom_col(show.legend = TRUE) +
  labs(x=NULL, y="Frequency") +
  coord_flip()+
  ggtitle("Most frequent terms")


wordcloud(freq.dt$term, freq.dt$freq, scale=c(3,0.5), 
          max.words=150, random.order=FALSE, 
          rot.per=0.010, use.r.layout=TRUE, 
          colors=brewer.pal(6, "Dark2")) 

wordcloud2(top_overmean, size=1.2)
#wordcloud2(top100, size = 1.2, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
#wordcloud2(data=df, size=1.6, color='random-dark')

###################################### TEXT MINING SUBTEXT FROM ALL POSTS ###################################### 

freq_terms_from_data = function(column){
  stopwords <- readLines(con = paste0(getwd(), "/data/", "stopwords-en.txt"))
  column <- removeWords(column, stopwords)
  column <- stripWhitespace(column)
  
  corpus <- Corpus(VectorSource(column))
  ### if we want to use stemming
  #post_cp_stemmed <-  tm_map(post_corpus, stemDocument)
  #dtm <- DocumentTermMatrix(post_cp_stemmed); dtm
  dtm <- DocumentTermMatrix(corpus)
  
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
  
  freq.dt$term = removeWords(freq.dt$term, stopwords)

  ### we remove text in which we have continous letters
  ### example: aaaaaaaaaa
  p <- " *\\b(?:[[:alnum:]]*([[:alnum:]])\\1{3}[[:alnum:]]*|[0-9]+)\\b"
  freq.dt$term <- gsub(p, "", freq.dt$term)
  freq.dt <- freq.dt %>% filter(term != "")
  
  return(list(dtm, freq.dt))
}

find_correlated_terms = function(dtm, column, cor){
  assocs <- findAssocs(dtm, column, cor)
  assocs.list <- lapply(assocs, function(x) data.frame(rhs = names(x), cor = x, stringsAsFactors = F))
  assocs.df <- dplyr::bind_rows(assocs.list, .id = "lhs")
  assocs.df %>% 
    group_by(lhs) %>% 
    top_n(5, cor) %>%
    ungroup() %>%
    mutate(rhs = reorder(rhs, cor)) %>%
    ggplot(aes(rhs, cor, fill=lhs)) +
    geom_col(show.legend = F) +
    coord_flip() +
    facet_wrap(~lhs,ncol=2, scales="free") +
    ggtitle("Top words & their correlation") +
    labs(x=NULL, y="Correlation")
}

posts_with_st <- posts_df %>% filter(selftext != "")

data <- freq_terms_from_data(posts_with_st$selftext); dtm <- data[[1]]; freq.dt <- data[[2]]

top.words <- freq.dt %>% arrange(desc(freq)) %>% head(17); top.words
find_correlated_terms(dtm, top.words$term, 0.5)

top.words <- freq.dt %>% arrange(desc(freq)) %>% head(8); top.words
find_correlated_terms(dtm, top.words$term, 0.35)


data(planets)
str(planets)
fc_planets <- FormalContext$new(planets)
fc_planets$implications
fc_planets$implications$get_LHS_matrix()
fc_planets$implications$get_RHS_matrix()
S <- SparseSet$new(attributes = fc_planets$attributes)
S$assign(large = 1, far = 1)
S
fc_planets$implications$closure(S)$closure


x <- matrix(c(0, 1, 1, 0, 1,
              1, 0, 1, 1, 0,
              1, 1, 1, 1, 0,
              1, 0, 0, 1, 0,
              1, 1, 1, 1, 0,
              1, 0, 1, 1, 0), 
            nrow = 6, 
            ncol = 5, 
            dimnames = list(c("o1","o2","o3","o4","o5","o6"), c("a1","a2","a3","a4","a5")))
x[1, ] <- c(0, 1, 1, 0, 1)
x[2, ] <- c(1, 0, 1, 1, 0)
x[3, ] <- c(1, 1, 1, 1, 0)
x[4, ] <- c(1, 0, 0, 1, 0)
x[5, ] <- c(1, 1, 1, 1, 0)
x[6, ] <- c(1, 0, 1, 1, 0)
rownames(x) <- c("o1","o2","o3","o4","o5","o6")
colnames(x) <- c("a1","a2","a3","a4","a5")
x
fc_animals <- FormalContext$new(x)
fc_animals$find_concepts()
fc_animals$concepts
fc_animals$concepts$plot()
fc_animals$find_implications()
fc_animals$implications
fc_animals$implications$print()

A = matrix(c(1,1,0,0,0,1,
             1,1,0,1,0,1,
             1,1,0,1,1,1,
             1,0,1,0,1,0,
             1,0,0,0,1,0,
             1,0,0,0,1,1,
             1,0,1,0,1,1), nrow=7, ncol = 6, byrow = TRUE)

rownames(A) <- c("avestruz","canario","pato","tiburón","salmón","rana", "cocodrilo")
colnames(A) <- c("huevos","pluma","dientes","vuela","nada", "respira")
A

fc_animals <- FormalContext$new(A)
fc_animals$find_concepts()
fc_animals$concepts
fc_animals$concepts$plot()

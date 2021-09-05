# SHINY
SHINY_MAX_PRINT_CONSOLE <- 10000
SHINY_SIDEBAR_WIDTH <- 3
SHINY_PLOT_HEIGHT <- 550
SHINY_SUBREDDIT_MAX_OPT <- 2500
SHINY_FREQ_PERCENTAGE <- .95
SHINY_DT_DEF_PAGELENGTH <- 5
SHINY_DT_DEF_LENGTHMENU <- c(5, 10, 25, 50, 100)
SHINY_DT_DEF_WIDTH <- "150px"
SHINY_DT_BUTTONS_POS <- "Bfrtip"
SHINY_ETL_ID <- "etl"
SHINY_POST_ID <- "post"
SHINY_AWARD_ID <- "award"
SHINY_COMMENT_ID <- "comment"
SHINY_USERS <- data.frame(
	user = c("shiny"),
	password = c("tomorrow north final social"),
	admin = c(TRUE),
	stringsAsFactors = FALSE
)

# LIMIT THE NUMBER OF POSTS TO OBTAIN
## PER EACH SUBREDDIT
POPULATE_LIMIT_MIN <- 1
POPULATE_LIMIT_MAX <- 50
POPULATE_LIMIT_DEF <- 5

## PER SEARCH ON THE API
API_LIMIT_MIN <- 5
API_LIMIT_MAX <- 100
API_LIMIT_DEF <- 25

## PER SEACH ON THE DB
DB_LIMIT_MIN <- 100
DB_LIMIT_MAX <- 10000
DB_LIMIT_DEF <- 2000


# DEFAULT SEARCH PARAMETERS
LISTINGS <- c("hot", "new", "rising", "top")
TIMEFRAMES <- c("hour", "day", "week", "month" , "year", "all")
SELECT_ORDER_AWARD <- list(label = "Select Order", choices = c("Quantity", "Cost"))
SELECT_ORDER_COMMENT <- list(label = "Select Order", choices = c("Comment score", "User karma", "Date created"))
SELECT_SUBREDDIT <- list(label = "Select Subreddit", choices = NULL)

# DATEPICKER OPTIONS
DPICK_1ST_DATE <- Sys.Date()-60
DPICK_2ND_DATE <- Sys.Date()
DPICK_FORMAT <- "dd/mm/yyyy"
DPICK_LANG <- "es"
DPICK_WEEKSTART <- 1

# BASE QUERIES
DB_BASE_POSTS <- "SELECT post.*, subreddit.name as subreddit, user.name as username FROM post JOIN subreddit ON post.subreddit_id = subreddit.id JOIN user ON post.user_id = user.id "
DB_BASE_COMMENTS <- "SELECT post.id, comment.comment as title, comment.score, user.name as username, user.total_karma as user_total_karma, user.is_employee as user_is_employee, user.is_mod as user_is_mod, user.is_gold as user_is_gold, subreddit.name as subreddit FROM comment JOIN post ON comment.post_id = post.id JOIN user ON comment.user_id = user.id JOIN subreddit ON post.subreddit_id = subreddit.id "
DB_BASE_AWARDS <- "SELECT subreddit.name AS subreddit, coin.name AS coin, sum(awards_post.quantity) as quantity, sum(coin.coin_price * awards_post.quantity) as cost FROM post JOIN awards_post ON post.id = awards_post.post_id JOIN coin ON coin.id = awards_post.coin_id JOIN subreddit ON post.subreddit_id = subreddit.id "

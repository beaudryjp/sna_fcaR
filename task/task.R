path <- "C:/Users/jeanp/shiny/app/"

source(paste0(path, "src/load.R"))
source(paste0(path, "src/database/standard.R"))
source(paste0(path, "src/functions.R"))
source(paste0(path, "src/reddit/post.R"))
source(paste0(path, "src/reddit/subreddit.R"))
source(paste0(path, "src/reddit/user.R"))
source(paste0(path, "src/reddit/award.R"))
source(paste0(path, "src/reddit/comment.R"))

subreddit.get_id("news")

#database <- Database$new("reddit")
post.get_by_listing("popular", "rising", 2, "day", TRUE)
#database$close()

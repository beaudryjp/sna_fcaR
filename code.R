source(paste0(getwd(), "/src/", "reddit_general.R"))
source(paste0(getwd(), "/src/", "reddit_posts.R"))
source(paste0(getwd(), "/src/", "reddit_subreddits.R"))
source(paste0(getwd(), "/src/", "reddit_user.R"))


posts <- post_get_listing("worldnews", "top", 5, "month")

c <- post_get_comments("https://www.reddit.com/r/worldnews/comments/m14ngw/china_breaching_every_act_in_genocide_convention/")



u <- user_get_info("_beaudry"); u

user_insert_db(u)

s <- subreddit_get_id("WorldNews"); s
# 
# comm <- post_get_comments("https://www.reddit.com/r/worldnews/comments/m14ngw/china_breaching_every_act_in_genocide_convention/")


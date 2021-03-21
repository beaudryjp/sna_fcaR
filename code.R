### Compile source files
source(paste0(getwd(), "/src/", "reddit_general.R"))
source(paste0(getwd(), "/src/", "reddit_posts.R"))
source(paste0(getwd(), "/src/", "reddit_subreddits.R"))
source(paste0(getwd(), "/src/", "reddit_user.R"))
### Load the necessary packages
package_load()

# posts <- post_get_listing("apple", "top", 6, "month")
# u <- user_get_info("crystals148"); u

subreddit_auto_populate(5, "top", "month")
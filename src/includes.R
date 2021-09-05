# GENERIC FILES
#source(paste0(SHINY_SRC_FOLDER, "database/pool.R"), local = TRUE)
source(paste0(SHINY_SRC_FOLDER, "database/standard.R"), local = TRUE)
source(paste0(SHINY_SRC_FOLDER, "functions.R"), local = TRUE)
source(paste0(SHINY_SRC_FOLDER, "constants.R"), local = TRUE)


# SHINY FILES
source(paste0(SHINY_UI_FOLDER, "sidebar.R"), local = TRUE)
source(paste0(SHINY_UI_FOLDER, "fca.R"), local = TRUE)
source(paste0(SHINY_UI_FOLDER, "data.R"), local = TRUE)
source(paste0(SHINY_UI_FOLDER, "etl.R"), local = TRUE)
source(paste0(SHINY_UI_FOLDER, "main.R"), local = TRUE)
source(paste0(SHINY_SRV_FOLDER, "data.R"), local = TRUE)
source(paste0(SHINY_SRV_FOLDER, "tm.R"), local = TRUE)
source(paste0(SHINY_SRV_FOLDER, "fca.R"), local = TRUE)
source(paste0(SHINY_SRV_FOLDER, "etl.R"), local = TRUE)


# ETL FILES
source(paste0(SHINY_ETL_FOLDER, "post.R"), local = TRUE)
source(paste0(SHINY_ETL_FOLDER, "subreddit.R"), local = TRUE)
source(paste0(SHINY_ETL_FOLDER, "user.R"), local = TRUE)
source(paste0(SHINY_ETL_FOLDER, "award.R"), local = TRUE)
source(paste0(SHINY_ETL_FOLDER, "comment.R"), local = TRUE)

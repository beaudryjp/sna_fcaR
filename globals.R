SHINY_BASE_FOLDER <- getwd()
SHINY_SRC_FOLDER <- paste0(SHINY_BASE_FOLDER, "/src/")
SHINY_MODULE_FOLDER <- paste0(SHINY_SRC_FOLDER, "/modules/")
SHINY_UI_FOLDER <- paste0(SHINY_MODULE_FOLDER, "/ui/")
SHINY_SRV_FOLDER <- paste0(SHINY_MODULE_FOLDER, "/server/")
SHINY_ETL_FOLDER <- paste0(SHINY_SRC_FOLDER, "/reddit/")

source(paste0(SHINY_SRC_FOLDER, "load.R"))
source(paste0(SHINY_SRC_FOLDER, "includes.R"))

shiny_debug(FALSE)

# SHINY AUTH - PRE-LOAD
# create_db(
# 	credentials_data = SHINY_USERS,
# 	sqlite_path = "data/credentials.sqlite",
# 	passphrase = "passphrase_wihtout_keyring"
# )

#database <- Database$new("reddit")
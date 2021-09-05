MainPage <- function(){
	fluidPage(
		theme = shinytheme("flatly"),
		useShinyjs(), 
		tags$head(tags$style("
		  th, td { white-space: nowrap; }
		  body {padding-top: 70px;}
		")),

		navbarPage(
			
			"Social Network Analysis",
			
			id = "navigation_menu",
			
			selected = "Reddit",
			position = c("fixed-top"),
			
			navbarMenu(
				"Data Extraction",
				tabPanel(
					"Reddit",
					SidebarAPI(), 
					ETLTab(SHINY_ETL_ID)
				)
			), #closure navBarMenu Data extraction
			
			navbarMenu(
				"Reddit",
				tabPanel(
					"Posts",
					SidebarSearch(SHINY_POST_ID, SELECT_SUBREDDIT), 
					mainPanel(
						id ="main_posts",
						tabsetPanel(
							id = "posts_tabs",
							tabPanel(
								"Summary",
								DataTab(SHINY_POST_ID)
							), # closure data tabPanel
							tabPanel(
								"Formal Concept Analysis",
								#ui.posts_fca
								FCATab(SHINY_POST_ID)
								
							) #closure fca tabPanel
							
						) #closure posts tabSetPanel
						
					) #closure posts mainPanel
					
				), #closure reddit tabPanel
				
				tabPanel(
					"Awards",
					SidebarSearch(SHINY_AWARD_ID, SELECT_ORDER_AWARD),
					mainPanel(
						id = "main_awards",
						tabsetPanel(
							id = "awards_tabs",
							tabPanel(
								"Summary",
								DataTab(SHINY_AWARD_ID)
							),
							
							tabPanel(
								"Formal Concept Analysis",
								FCATab(SHINY_AWARD_ID)
							)
							
						) # closure awards tabsetPanel
						
					) # closure awards mainPanel
					
				), # closure awards tabPanel
				
				tabPanel(
					"Comments",
					SidebarSearch(SHINY_COMMENT_ID, SELECT_ORDER_COMMENT),
					mainPanel(
						id = "main_comments",
						tabsetPanel(
							id = "comment_tabs",
							tabPanel(
								"Summary",
								DataTab(SHINY_COMMENT_ID)
							), # closure data tabPanel
							
							tabPanel(
								"Formal Concept Analysis",
								#ui.comments_fca
								FCATab(SHINY_COMMENT_ID)
								
							) #closure fca tabPanel
							
						) #closure comments tabsetPanel
						
					) # closure comments mainPanel
					
				) #closure comments tabPanel
				
			), #closure navBarMenu reddit
			
			
			tabPanel(tags$ul(id="time", class='nav navbar-nav disabled',style = "position:fixed; top:15px; right: 40px; ", htmlOutput("currentTime"))),
			
			tags$footer( HTML("<footer><small><b>&copy; Jean-Paul Beaudry</b><br></small></footer>"), align="left", style="position:absolute; bottom:0; width:95%; height:50px; color: #808080; padding: 0px; background-color: transparent; z-index: 1000;")
			
			
		), #closure navBarPage
		
		use_gotop()
		
	) #closure fluidPage,
	
	
}
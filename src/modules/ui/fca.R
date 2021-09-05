FCATab <- function(id){
	ns <- NS(id)
	field = paste0(id, "_")
	tabsetPanel(
		id = paste0(field, "tabs"),
		tabPanel(
			id = paste0(field, "tab_summary"), 
			
			"Summary",
			
			br(), br(),
			
			downloadButton(ns("download_fca_latex"),"Download Formal Context as latex"),
			
			h3("Information"),
			tabPanel("Information fca", verbatimTextOutput(ns("information"))),
			
			h3("Obtaining non redundant rules"),
			tabPanel("Information non redundant", verbatimTextOutput(ns("rules_non_redundant"))),
			
			h3("Plot"),
			tabPanel("Plot", plotOutput(ns("plot_initial"), height = SHINY_PLOT_HEIGHT)),
			
			tabPanel("Empty",div(style = "height:150px") )
			,
		),
		
		tabPanel(
			id = paste0(field, "tab_concepts"),
			
			"Concepts",
			
			h3("Find concepts"),
			tabPanel("Find Concepts", verbatimTextOutput(ns("concept_find"))),

			h3("One concept from the dataset"),
			tabPanel("One Concept", verbatimTextOutput(ns("concept_one"))),

			h3("The support for the concepts"),
			tabPanel("Concept support", verbatimTextOutput(ns("concept_summary"))),

			h3("Plot of the concepts"),
			plotOutput(ns("plot_concepts"), height = SHINY_PLOT_HEIGHT),

			h3("Sublattice of the concepts which have a support greater than 0.3"),
			tabPanel("Concept sublattice", verbatimTextOutput(ns("concept_sublattice"))),

			h3("Plot of the sublattice"),
			plotOutput(ns("plot_sublattice"), height = SHINY_PLOT_HEIGHT),

		),

		tabPanel(
			id = paste0(field, "tab_implications"),
			
			"Implications",
			
			h3("Find implications"),
			tabPanel("Find implications", verbatimTextOutput(ns("implication_find"))),

			h3("Remove redundancies in the implications"),
			tabPanel("Reduce implications", verbatimTextOutput(ns("implication_remove"))),

			h3("Cardinality of the implications"),
			tabPanel("Implications cardinality", verbatimTextOutput(ns("implication_cardinality"))),


			h3("Recommender"),
			h4("Define set of attributes"),
			tabPanel("Set attributes", verbatimTextOutput(ns("implication_set_attributes"))),

			h4("Assign a field"),
			tabPanel("Assign field", verbatimTextOutput(ns("implication_assign_field"))),

			h4("Compute intent"),
			tabPanel("Set intent", verbatimTextOutput(ns("implication_compute_intent"))),

			h4("Compute the closure of the intent"),
			tabPanel("Compute closure", verbatimTextOutput(ns("implication_compute_closure"))),

			h4("Is the closure calculated closed?"),
			tabPanel("Is the closure closed", verbatimTextOutput(ns("implication_closure_closed"))),

			h4("Recommend implications"),
			tabPanel("Recommend implications", verbatimTextOutput(ns("implication_recommend"))),

		),

		tabPanel(
			id = paste0(field, "tab_arules"),
			
			"Arules",
			
			h3("Non redundant rules"),
			h4("Plot"),
			plotOutput(ns("nonredundant_plot"), height = SHINY_PLOT_HEIGHT),
			h4("Table"),
			tabPanel("Non redundant Output", DT::dataTableOutput(ns("nonredundant_table"))),
			
			h3("Significant rules"),
			h4("Plot"),
			plotOutput(ns("significant_plot"), height = SHINY_PLOT_HEIGHT),
			h4("Table"),
			tabPanel("Significant Output", DT::dataTableOutput(ns("significant_table"))),
			
			h3("Export to arules"),
			tabPanel("Export rules", verbatimTextOutput(ns("export_rules"))),

			h3("Top 30 rules sorted by lift"),
			tabPanel("Non redundant Output", DT::dataTableOutput(ns("lift_table"))),

			h4("Scatter Plot"),
			plotOutput(ns("lift_scatter_plot"), height = SHINY_PLOT_HEIGHT),

			h4("Graph Plot"),
			plotOutput(ns("lift_graph_plot"), height = SHINY_PLOT_HEIGHT),

			h3("Top 30 rules sorted by support"),
			tabPanel("Non redundant Output", DT::dataTableOutput(ns("support_table"))),

			h4("Scatter Plot"),
			plotOutput(ns("support_scatter_plot"), height = SHINY_PLOT_HEIGHT),

			h4("Graph Plot"),
			plotOutput(ns("support_graph_plot"), height = SHINY_PLOT_HEIGHT),

		)
	)
}

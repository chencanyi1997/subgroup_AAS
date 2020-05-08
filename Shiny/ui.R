
library(shiny)

fluidPage(
	headerPanel("Two Stage Subgroup analysis for the Lung dataset"),
	# Sidebar with controls to provide a caption, select a dataset, and 
	# specify the number of observations to view. Note that changes made
	# to the caption in the textInput control are updated in the output
	# area immediately as you type
	sidebarPanel(
		numericInput("min", "Lower bound for k:", 1),
		numericInput("max", "Upper bound for k:", 20)
	),
	
	# Show the caption, a summary of the dataset and an HTML table with
	# the requested number of observations
	mainPanel(
		plotOutput(outputId = "myPlot")
	)
)

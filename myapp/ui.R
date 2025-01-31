#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Add CSS to hide the file input progress bar
  tags$head(
    tags$style("
      .shiny-file-input-progress {display: none}
      /* Style for subtitle */
      .subtitle {
        font-size: 1em;
        color: #666;
        margin-top: -10px;
        margin-bottom: 20px;
      }
    ")
  ),

  # Title and subtitle
  titlePanel(
    div(
      h1("PhyloDiSH: Phylogenetic Downsampling in Shiny"),
      tags$div(style = "height: 4px;"),  # Add a div with height of 4px for empty space
    #   p(class = "subtitle", "Phylogenetic Downsampling in Shiny")
    ),
    windowTitle = "PhyloDiSH"  # This is what shows in the browser tab
  ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Example data button first
      actionButton("loadExample", "Load Example Data",
                  style = "width: 100%;"),
      
      hr(),
      
      # File input buttons using uiOutput
      uiOutput("treeFile_ui"),
      uiOutput("metaFile_ui"),
      
      hr(),
      
      # Clear data button
      actionButton("clearData", "Clear Data",
                  class = "btn-warning",  # Makes the button yellow
                  style = "margin-top: 10px; width: 100%;"),  # Adds spacing and makes button full width
      
      hr(),
      
      # Status message
      verbatimTextOutput("statusMsg", placeholder = TRUE)
    ),

        mainPanel(
            width = 9,
            tabsetPanel(
                id = "mainTabs",
                tabPanel("Tree Info", 
                         fluidRow(
                             column(12,
                                    wellPanel(
                                        # Tree information below the plot
                                        verbatimTextOutput("treeInfo"),
                                        # Buttons container
                                        uiOutput("treeInfoButtons"),
                                        # Add some space
                                        br(),
                                        # Conditional plot output
                                        uiOutput("treeInfoPlotContainer")
                                    )
                             )
                         )),
                tabPanel("Metadata", 
                         # Add controls for column splitting
                         fluidRow(
                             column(12,
                                    wellPanel(
                                        h4("Add New Column"),
                                        # First row
                                        fluidRow(
                                            column(6,
                                                   textInput("newColName", "New Column Name:",
                                                             value = "")),
                                            column(6,
                                                   selectInput("splitColumn", "Column to split:",
                                                             choices = NULL))
                                        ),
                                        # Second row
                                        fluidRow(
                                            column(2,
                                                   textInput("delimiter", "Delimiter:",
                                                             value = "_", width = "80px")),
                                            column(3,
                                                   numericInput("position", "Position:",
                                                              value = 1, min = 1)),
                                            column(3,
                                                   selectInput("reverse", "Reverse:",
                                                             choices = c("FALSE" = FALSE, 
                                                                       "TRUE" = TRUE),
                                                             selected = FALSE)),
                                            column(4,
                                                   br(),
                                                   actionButton("addColumn", "Add Column",
                                                              class = "btn-primary"))
                                        )
                                    )
                             )
                         ),
                         DT::dataTableOutput("metadataTable")),
                tabPanel("Subsampling",
                         wellPanel(
                             h4(""),
                             fluidRow(
                                 column(6,
                                        selectInput("subsamplingMethod", 
                                                  "Subsampling Method:",
                                                  choices = c("monophyletic cluster" = "mono"))
                                 )
                             ),
                             conditionalPanel(
                                 condition = "input.subsamplingMethod == 'mono'",
                                 fluidRow(
                                     column(6,
                                            selectInput("groupingColumn", 
                                                      "Group by:",
                                                      choices = NULL)
                                     ),
                                     column(4,
                                            numericInput("keepNumber",
                                                       "Keep:",
                                                       value = 1,
                                                       min = 1)
                                     )
                                 )
                             ),
                             fluidRow(
                                 column(12,
                                        actionButton("runSubsampling", "Run Subsampling",
                                                   class = "btn-primary",
                                                   style = "margin-top: 15px; width: 100%")
                                 )
                             )
                         ),
                         wellPanel(
                             h4("Results"),
                             verbatimTextOutput("subsamplingResults")
                         )),
                # Conditional tab for subsampling results
                tabPanelBody("Subsampling Table",
                             value = "subsamplingTable",
                             DT::dataTableOutput("subsampledTable"))
            )
        )
    )
)
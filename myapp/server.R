#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(ape)
source("R/utils/string_utils.R")
source("R/utils/tree_utils.R")
source("R/utils/reactive_utils.R")
source("R/handlers/file_handlers.R")
source("R/handlers/subsampling_handlers.R")
source("R/handlers/download_handlers.R")
source("R/handlers/ui_handlers.R")
source("R/handlers/tree_plot_handlers.R")
source("R/subsampling/mono_cluster.R")
#source("subsampling.R")

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Example data
example_tree_text <- "((((A1_A,A2_A),(A3_A,A4_A)),((B1_B,B2_B),((B3_B,B4_B),(B5_B,B6_B)))),((C1_C,C2_C),C3_C));"
example_metadata <- data.frame(
  name = c("A1_A","A2_A","A3_A","A4_A","B1_B","B2_B","B3_B","B4_B","B5_B","B6_B","C1_C","C2_C","C3_C"),
  loc = c("A","A","A","A","B","B","B","B","B","B","C","C","C")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Initialize reactive values
    values <- initialize_reactive_values()

    # Create UI outputs for file inputs
    output$treeFile_ui <- renderUI({
        fileInput("treeFile", "Load Tree (Newick or Nexus format)",
                 accept = c(".tree", ".newick", ".nwk", ".nex", ".nexus"))
    })
    
    output$metaFile_ui <- renderUI({
        fileInput("metaFile", "Load Metadata (CSV format)",
                 accept = c(".csv"))
    })

    # Create a function to append messages to the log
    appendToLog <- function(message) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      values$statusLog <- c(values$statusLog, paste0("[", timestamp, "] ", message))
    }

    # File handling observers
    observeEvent(input$treeFile, {
        handle_tree_file(input, values, session, appendToLog)
    })
    
    observeEvent(input$metaFile, {
        handle_metadata_file(input, values, appendToLog)
    })
    
    observeEvent(input$loadExample, {
        handle_example_data(values, example_tree_text, example_metadata, appendToLog)
    })

    # Display basic tree information and plot
    observe({
        update_tree_visualizations(output, values)
    })

    # Handle Plot Tree button
    observeEvent(input$plotTreeInfo, {
        req(values$tree)
        values$show_tree_info_plot <- TRUE
    })
    
    # Handle Clear Plot button
    observeEvent(input$clearTreeInfoPlot, {
        values$show_tree_info_plot <- NULL
    })

    # Display metadata table
    output$metadataTable <- DT::renderDataTable({
        req(values$metadata)
        DT::datatable(values$metadata,
                     options = list(pageLength = 10,
                                  scrollX = TRUE),
                     rownames = FALSE)
    })

    # Update column choices when metadata is loaded or created
    observe({
        req(values$metadata)
        updateSelectInput(session, "splitColumn",
                         choices = names(values$metadata))
        updateSelectInput(session, "groupingColumn",
                         choices = names(values$metadata))
    })
    
    # Add new column based on splitting using get_loc function
    observeEvent(input$addColumn, {
        req(values$metadata, input$splitColumn)
        
        # Get the column to split
        col_data <- values$metadata[[input$splitColumn]]
        
        # Use get_loc function to extract tokens
        new_col <- get_loc(col_data, 
                           delimiter = input$delimiter, 
                           index = input$position, 
                           reverse = input$reverse)
        
        # Create new column name
        new_col_name <- input$newColName
        
        # Add the new column to the metadata
        values$metadata[[new_col_name]] <- new_col
        
        # Update status message
        appendToLog(paste("Added new column:", new_col_name))
    })

    # Subsampling observer
    observeEvent(input$runSubsampling, {
        handle_subsampling(input, values, output, appendToLog)
    })

    # Set up download handlers
    create_download_handlers(output, values)

    # Show/hide Subsampling Output tab
    observe({
        if (!is.null(values$subsampled_df)) {
            showTab(inputId = "mainTabs", target = "Subsampling Output")
        } else {
            hideTab(inputId = "mainTabs", target = "Subsampling Output")
        }
    })

    # Render the status log
    output$statusMsg <- renderPrint({
      cat(paste(values$statusLog, collapse = "\n"))
    })

    # Clear data handler
    observeEvent(input$clearData, {
        handle_clear_data(input, output, values, session, appendToLog)
    })

    # Handle Plot Downsampled Tree button
    observeEvent(input$plotDownsampledTree, {
        handle_plot_downsampled_tree(input, output, session, values)
    })

}

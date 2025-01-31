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
source("subsampling.R")
source("utils.R")

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

    # Reactive values to store the tree and metadata
    values <- reactiveValues(
        tree = NULL,
        metadata = NULL,
        subsampled_tree = NULL,
        subsampled_metadata = NULL,
        subsampled_df = NULL,
        show_tree_info_plot = NULL,
        statusLog = character(0)
    )

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

    # Handle tree file upload
    observeEvent(input$treeFile, {
        req(input$treeFile)
        
        # Clear all existing data first
        values$tree <- NULL
        values$metadata <- NULL
        values$subsampled_tree <- NULL
        values$subsampled_metadata <- NULL
        values$subsampled_df <- NULL
        values$show_tree_info_plot <- NULL
        
        # Remove the Subsampling Output tab if it exists
        removeTab(inputId = "mainTabs", target = "Subsampling Output")
        
        # Reset other inputs to default values
        updateSelectInput(session, "splitColumn", choices = NULL)
        updateTextInput(session, "newColName", value = "")
        updateTextInput(session, "delimiter", value = "_")
        updateNumericInput(session, "position", value = 1)
        updateSelectInput(session, "reverse", selected = FALSE)
        
        # Reset subsampling tab inputs
        updateSelectInput(session, "subsamplingMethod", selected = "mono")
        updateSelectInput(session, "groupingColumn", choices = NULL)
        updateNumericInput(session, "keepNumber", value = 1)
        
        # Now proceed with loading the new tree
        tryCatch({
            # Read the first few lines of the file to check format
            first_lines <- readLines(input$treeFile$datapath, n = 5)
            
            # Check if it's a NEXUS file
            if (any(grepl("#NEXUS", first_lines, ignore.case = TRUE))) {
                values$tree <- ape::read.nexus(input$treeFile$datapath)
            } else {
                # Assume Newick format
                tree_text <- readLines(input$treeFile$datapath)
                values$tree <- read.tree(text = tree_text)
            }
            
            values$metadata <- data.frame(
                name = values$tree$tip.label,
                stringsAsFactors = FALSE
            )

            appendToLog(paste("Tree file loaded:", input$treeFile$name))
        }, error = function(e) {
            appendToLog(paste("Error loading tree file:", input$treeFile$name, "-", e$message))
        })
    })
    
    # Handle metadata file upload
    observeEvent(input$metaFile, {
        req(input$metaFile)
        tryCatch({
            values$metadata <- read.csv(input$metaFile$datapath, stringsAsFactors = FALSE)
            appendToLog(paste("Metadata file loaded:", input$metaFile$name))
        }, error = function(e) {
            appendToLog(paste("Error loading metadata file:", input$metaFile$name))
        })
    })

    # Handle example data loading
    observeEvent(input$loadExample, {
        values$tree <- read.tree(text = example_tree_text)
        values$metadata <- example_metadata
        appendToLog("Example data loaded successfully!")
    })

    # Display basic tree information
    output$treeInfo <- renderPrint({
        if (!is.null(values$tree)) {
            cat("Tree summary:\n")
            cat("Number of tips:", length(values$tree$tip.label), "\n")
        } else {
            cat("No tree loaded")
        }
    })

    # Display metadata table
    output$metadataTable <- DT::renderDataTable({
        req(values$metadata)
        DT::datatable(values$metadata,
                     options = list(pageLength = 10,
                                  scrollX = TRUE),
                     rownames = FALSE)
    })

    # Plot the phylogenetic tree
    output$treePlot <- renderPlot({
        req(values$tree)
        par(mar = c(1, 1, 1, 1))  # Adjust margins
        plot(values$tree, 
             cex = 0.8,           # Adjust tip label size
             no.margin = TRUE,    # Remove margins around the plot
             direction = "right") # Plot tree from left to right
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

    # Placeholder for subsampling results
    output$subsamplingResults <- renderPrint({
        cat("Subsampling configuration:\n")
        cat("Method:", input$subsamplingMethod, "\n")
        if (input$subsamplingMethod == "mono_cluster") {
            cat("Grouping by:", input$groupingColumn, "\n")
            cat("Keep:", input$keepNumber, "\n")
        }
    })

    # Handle subsampling
    observeEvent(input$runSubsampling, {
        req(values$tree, values$metadata)
        
        tryCatch({
            if (input$subsamplingMethod == "mono") {
                # Run monophyletic cluster subsampling
                result <- downsample_tree_n_meta(
                    tree = values$tree,
                    meta = values$metadata,
                    n = input$keepNumber,
                    loc_column = input$groupingColumn
                )
                
                # Store results in reactive values
                values$node_annots <- result[[2]]
                values$subsampled_df <- result[[1]]

                toremove <- values$subsampled_df$taxa[values$subsampled_df$remove == 1]
                values$subsampled_tree <- drop.tip(values$tree, toremove)
                
                # Display results
                num_taxa <- length(values$tree$tip.label)
                num_removed <- length(values$subsampled_df$taxa[values$subsampled_df$remove == 1])
                output$subsamplingResults <- renderPrint({
                    cat("Subsampling completed successfully!\n\n")
                    cat("Original tree tips:", num_taxa, "\n")
                    cat("Subsampled tree tips:", num_taxa - num_removed, "\n")
                    cat("Number of clusters collapsed:", length(unique(values$subsampled_df$cluster[values$subsampled_df$remove == 1])), "\n")
                })
                
                # Render the subsampled table
                output$subsampledTable <- DT::renderDataTable({
                    req(values$subsampled_df)
                    DT::datatable(values$subsampled_df,
                                 options = list(pageLength = 10,
                                              scrollX = TRUE,
                                              title = "Subsampled Data Table"),
                                 rownames = FALSE)
                })
                
                # Create and switch to new tab with download buttons
                insertTab(inputId = "mainTabs",
                         tabPanel("Subsampling Output",
                                  wellPanel(
                                    h4("Downsampling Output"),
                                    br(),
                                    fluidRow(
                                      column(6,
                                            downloadButton("downloadDownsamplingTable", 
                                                         "Write Downsampling Table",
                                                         class = "btn-primary",
                                                         style = "width: 100%")),
                                      column(6,
                                            downloadButton("downloadTree", 
                                                         "Write Downsampled Tree",
                                                         class = "btn-primary",
                                                         style = "width: 100%"))
                                    )
                                  ),
                                  br(),
                                  DT::dataTableOutput("subsampledTable")),
                         target = "Subsampling",
                         position = "after",
                         select = TRUE)
                
                # Update status message
                appendToLog("Subsampling completed successfully!")
                
            } else {
                output$subsamplingResults <- renderPrint({
                    cat("Selected subsampling method not implemented yet.")
                })
            }
        }, error = function(e) {
            output$subsamplingResults <- renderPrint({
                cat("Error during subsampling:\n")
                cat(as.character(e))
            })
            appendToLog("Error during subsampling!")
        })
    })

    # Download handler for cluster table
    output$downloadDownsamplingTable <- downloadHandler(
        filename = function() {
            paste0("cluster_table_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            write.csv(values$subsampled_df, file, row.names = FALSE)
        }
    )
    
    # Download handler for tree
    output$downloadTree <- downloadHandler(
        filename = function() {
            paste0("downsampled_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".nwk")
        },
        content = function(file) {
            ape::write.tree(values$subsampled_tree, file)
        }
    )

    # Clear data should also clear the subsampled results and remove the tab
    observeEvent(input$clearData, {
        values$tree <- NULL
        values$metadata <- NULL
        values$subsampled_tree <- NULL
        values$subsampled_metadata <- NULL
        values$subsampled_df <- NULL
        values$show_tree_info_plot <- NULL
        
        # Remove the Subsampling Output tab if it exists
        removeTab(inputId = "mainTabs", target = "Subsampling Output")
        
        # Reset file inputs by recreating them
        output$treeFile_ui <- renderUI({
            fileInput("treeFile", "Load Tree (Newick or Nexus format)",
                     accept = c(".tree", ".newick", ".nwk", ".nex", ".nexus"))
        })
        output$metaFile_ui <- renderUI({
            fileInput("metaFile", "Load Metadata (CSV format)",
                     accept = c(".csv"))
        })
        
        # Reset other inputs to default values
        updateSelectInput(session, "splitColumn", choices = NULL)
        updateTextInput(session, "newColName", value = "")
        updateTextInput(session, "delimiter", value = "_")
        updateNumericInput(session, "position", value = 1)
        updateSelectInput(session, "reverse", selected = FALSE)
        
        # Reset subsampling tab inputs
        updateSelectInput(session, "subsamplingMethod", selected = "mono")
        updateSelectInput(session, "groupingColumn", choices = NULL)
        updateNumericInput(session, "keepNumber", value = 1)
        
        # Clear the subsampling results output
        output$subsamplingResults <- renderPrint({
            cat("")  # Empty output
        })
        
        # Update status message
        appendToLog("All data cleared!")
        
        # Remove the download handlers
        output$downloadDownsamplingTable <- NULL
        output$downloadTree <- NULL

        # Clear the log
        values$statusLog <- character(0)

        # run garbage collection
        gc()
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
    
    # Plot in Tree Info tab
    output$treeInfoPlot <- renderPlot({
        req(values$tree, values$show_tree_info_plot)
        par(mar = c(1, 1, 1, 1))  # Adjust margins
        plot(values$tree, 
            show.tip.label = FALSE,
            edge.width = 0.4,
            cex = 0.8,           # Adjust tip label size
            no.margin = TRUE,    # Remove margins around the plot
            direction = "right") # Plot tree from left to right
    })

    # Render tree info buttons
    output$treeInfoButtons <- renderUI({
        fluidRow(
            column(6,
                   if (!is.null(values$tree)) {
                       actionButton("plotTreeInfo", "Plot Tree",
                                  class = "btn-primary",
                                  style = "width: 100%")
                   }),
            column(6,
                   if (!is.null(values$show_tree_info_plot)) {
                       actionButton("clearTreeInfoPlot", "Clear Plot",
                                  class = "btn-warning",
                                  style = "width: 100%")
                   })
        )
    })
    
    # Render tree info plot container
    output$treeInfoPlotContainer <- renderUI({
        if (!is.null(values$show_tree_info_plot)) {
            plotOutput("treeInfoPlot", height = "calc(100vh - 300px)")
        }
    })
    
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

}

# Handlers for subsampling operations

handle_subsampling <- function(input, values, output, appendToLog) {
    req(values$tree, values$metadata)
    
    tryCatch({
        if (input$subsamplingMethod == "mono") {
            process_mono_subsampling(input, values, output)
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
}

process_mono_subsampling <- function(input, values, output) {
    result <- downsample_tree_n_meta(
        tree = values$tree,
        meta = values$metadata,
        n = input$keepNumber,
        loc_column = input$groupingColumn
    )
    
    # Store results
    values$node_annots <- result[[2]]
    values$subsampled_df <- result[[1]]
    
    toremove <- values$subsampled_df$taxa[values$subsampled_df$remove == 1]
    values$subsampled_tree <- drop.tip(values$tree, toremove)
    
    update_subsampling_outputs(values, output)
}

update_subsampling_outputs <- function(values, output) {
    # Display results
    num_taxa <- length(values$tree$tip.label)
    num_removed <- length(values$subsampled_df$taxa[values$subsampled_df$remove == 1])
    
    output$subsamplingResults <- renderPrint({
        cat("Subsampling completed successfully!\n\n")
        cat("Original tree tips:", num_taxa, "\n")
        cat("Subsampled tree tips:", num_taxa - num_removed, "\n")
        cat("Number of clusters collapsed:", 
            length(unique(values$subsampled_df$cluster[values$subsampled_df$remove == 1])), "\n")
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
    
    # Remove existing tab if it exists
    tryCatch({
        removeTab(inputId = "mainTabs", target = "Subsampling Output")
        removeTab(inputId = "mainTabs", target = "Downsampled Tree")
    }, error = function(e) {
        # Tab doesn't exist yet, that's okay
    })
    
    # Create new tab with download buttons
    insertTab(inputId = "mainTabs",
             tabPanel("Subsampling Output",
                      wellPanel(
                        h4("Downsampling Output"),
                        br(),
                        fluidRow(
                          column(4,
                                downloadButton("downloadDownsamplingTable", 
                                             "Write Downsampling Table",
                                             class = "btn-primary",
                                             style = "width: 100%")),
                          column(4,
                                downloadButton("downloadTree", 
                                             "Write Downsampled Tree",
                                             class = "btn-primary",
                                             style = "width: 100%")),
                          column(4,
                                actionButton("plotDownsampledTree",
                                           "Plot Downsampled Tree",
                                           class = "btn-primary",
                                           style = "width: 100%"))
                        )
                      ),
                      br(),
                      DT::dataTableOutput("subsampledTable")),
             target = "Subsampling",
             position = "after",
             select = TRUE)
}

handle_plot_downsampled_tree <- function(input, output, session, values) {
    req(values$subsampled_tree)
    
    # Remove existing tab if it exists
    tryCatch({
        removeTab(inputId = "mainTabs", target = "Downsampled Tree")
    }, error = function(e) {
        # Tab doesn't exist yet, that's okay
    })
    
    # Create new tab with tree plot and info
    insertTab(inputId = "mainTabs",
             tabPanel("Downsampled Tree",
                      wellPanel(
                        h4("Downsampled Tree Information"),
                        # Add tree information output
                        verbatimTextOutput("downsampledTreeInfo"),
                        br(),
                        h4("Downsampled Tree Visualization"),
                        plotOutput("downsampledTreePlot", height = "calc(100vh - 400px)")
                      )),
             target = "Subsampling Output",
             position = "after",
             select = TRUE)
    
    # Render the tree info
    output$downsampledTreeInfo <- renderPrint({
        req(values$subsampled_tree)
        cat("Tree Information:\n")
        cat("Number of tips:", length(values$subsampled_tree$tip.label), "\n")
        cat("Number of internal nodes:", values$subsampled_tree$Nnode, "\n")
        cat("Total number of nodes:", length(values$subsampled_tree$tip.label) + values$subsampled_tree$Nnode, "\n")
    })
    
    # Render the tree plot using the function from tree_plot_handlers
    render_downsampled_tree(output, values)
} 
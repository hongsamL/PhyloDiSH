# Utility functions for reactive elements

initialize_reactive_values <- function() {
    reactiveValues(
        tree = NULL,
        metadata = NULL,
        subsampled_tree = NULL,
        subsampled_metadata = NULL,
        subsampled_df = NULL,
        show_tree_info_plot = NULL,
        statusLog = character(0)
    )
}

reset_all_values <- function(values, session) {
    # Reset reactive values
    values$tree <- NULL
    values$metadata <- NULL
    values$subsampled_tree <- NULL
    values$subsampled_metadata <- NULL
    values$subsampled_df <- NULL
    values$show_tree_info_plot <- NULL
    
    # Remove the Subsampling Output tab if it exists
    removeTab(inputId = "mainTabs", target = "Subsampling Output")
    
    # Reset input values
    updateSelectInput(session, "splitColumn", choices = NULL)
    updateTextInput(session, "newColName", value = "")
    updateTextInput(session, "delimiter", value = "_")
    updateNumericInput(session, "position", value = 1)
    updateSelectInput(session, "reverse", selected = FALSE)
    updateSelectInput(session, "subsamplingMethod", selected = "mono")
    updateSelectInput(session, "groupingColumn", choices = NULL)
    updateNumericInput(session, "keepNumber", value = 1)
}

#' Reset a file input control to its initial state
#' @param inputId The ID of the file input control
#' @param session The current session object
reset_file_inputs <- function(inputId, session) {
    # Create a new file input with the same properties
    if(inputId == "treeFile") {
        session$output[[paste0(inputId, "_ui")]] <- renderUI({
            fileInput("treeFile", "Load Tree (Newick format)",
                     accept = c(".tree", ".newick", ".nwk"))
        })
    } else if(inputId == "metaFile") {
        session$output[[paste0(inputId, "_ui")]] <- renderUI({
            fileInput("metaFile", "Load Metadata (CSV format)",
                     accept = c(".csv"))
        })
    }
} 

reset_outputs <- function(output) {
    # Clear the subsampling results output
    output$subsamplingResults <- renderPrint({
        cat("")  # Empty output
    })
    
    # Remove the download handlers
    output$downloadDownsamplingTable <- NULL
    output$downloadTree <- NULL
} 
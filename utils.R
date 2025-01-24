#' Reset a file input control to its initial state
#' @param inputId The ID of the file input control
#' @param session The current session object
reset_file_input <- function(inputId, session) {
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
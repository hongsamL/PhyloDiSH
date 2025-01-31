# UI event handlers

handle_clear_data <- function(input, output, values, session, appendToLog) {
    # Reset all reactive values and inputs
    reset_all_values(values, session)
    
    # Reset file inputs
    reset_file_inputs("treeFile", session)
    reset_file_inputs("metaFile", session)
    
    # Reset outputs
    reset_outputs(output)
    
    # Remove tabs
    tryCatch({
        removeTab(inputId = "mainTabs", target = "Subsampling Output")
        removeTab(inputId = "mainTabs", target = "Downsampled Tree")
    }, error = function(e) {
        # Tabs might not exist, that's okay
    })
    
    # Clear the log
    values$statusLog <- character(0)
    
    # Update status message
    appendToLog("All data cleared!")
    
    # run garbage collection
    gc()
} 
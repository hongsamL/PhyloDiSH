# File handling functions for tree and metadata files

handle_tree_file <- function(input, values, session, appendToLog) {
    req(input$treeFile)
    
    # Clear all existing data first
    reset_all_values(values, session)
    
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
}

handle_metadata_file <- function(input, values, appendToLog) {
    req(input$metaFile)
    tryCatch({
        values$metadata <- read.csv(input$metaFile$datapath, stringsAsFactors = FALSE)
        appendToLog(paste("Metadata file loaded:", input$metaFile$name))
    }, error = function(e) {
        appendToLog(paste("Error loading metadata file:", input$metaFile$name))
    })
}

handle_example_data <- function(values, example_tree_text, example_metadata, appendToLog) {
    values$tree <- read.tree(text = example_tree_text)
    values$metadata <- example_metadata
    appendToLog("Example data loaded successfully!")
} 
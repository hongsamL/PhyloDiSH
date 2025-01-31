# Download handlers for various file outputs

create_download_handlers <- function(output, values) {
    output$downloadDownsamplingTable <- downloadHandler(
        filename = function() {
            paste0("cluster_table_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            write.csv(values$subsampled_df, file, row.names = FALSE)
        }
    )
    
    output$downloadTree <- downloadHandler(
        filename = function() {
            paste0("downsampled_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".nwk")
        },
        content = function(file) {
            ape::write.tree(values$subsampled_tree, file)
        }
    )
} 
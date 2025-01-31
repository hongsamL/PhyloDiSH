# Handlers for tree visualization and plotting

plot_tree_with_settings <- function(tree, show_labels = FALSE) {
    par(mar = c(1, 1, 1, 1))  # Adjust margins
    plot(tree,
         show.tip.label = show_labels,
         edge.width = 0.4,
         cex = 0.8,           
         no.margin = TRUE,    
         direction = "right")
}

render_tree_info <- function(output, values) {
    output$treeInfo <- renderPrint({
        req(values$tree)
        cat("Tree Information:\n")
        cat("Number of tips:", length(values$tree$tip.label), "\n")
        cat("Number of internal nodes:", values$tree$Nnode, "\n")
        cat("Total number of nodes:", length(values$tree$tip.label) + values$tree$Nnode, "\n")
    })
}

render_tree_info_plot <- function(output, values) {
    output$treeInfoPlot <- renderPlot({
        req(values$tree, values$show_tree_info_plot)
        plot_tree_with_settings(values$tree, show_labels = FALSE)
    })
}

render_tree_info_buttons <- function(output, values) {
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
}

render_tree_info_plot_container <- function(output, values) {
    output$treeInfoPlotContainer <- renderUI({
        if (!is.null(values$show_tree_info_plot)) {
            plotOutput("treeInfoPlot", height = "calc(100vh - 300px)")
        }
    })
}

render_downsampled_tree <- function(output, values) {
    output$downsampledTreePlot <- renderPlot({
        req(values$subsampled_tree)
        plot_tree_with_settings(values$subsampled_tree, show_labels = FALSE)
    })
}

update_tree_visualizations <- function(output, values) {
    render_tree_info(output, values)
   #render_phylo_tree(output, values)
    render_tree_info_plot(output, values)
    render_tree_info_buttons(output, values)
    render_tree_info_plot_container(output, values)
    render_downsampled_tree(output, values)
} 
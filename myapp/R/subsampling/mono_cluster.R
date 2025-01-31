# required packages
# library(ape)
# R/utils/tree_utils.R
# R/utils/string_utils.R

# Downsample N from monophyletic clusters
downsample_tree_n_meta <- function(tree, meta, n, loc_column) {
  num_taxa <- length(tree$tip.label)
  tip_annot <- get_loc_array_meta(tree$tip.label, meta, loc_column)
  node_annots <- get_node_annots(tree, tip_annot)
  clusters <- get_clusters(tree, node_annots)
  toremove_df <- prune_clusters_n(tree, clusters, n)
  num_removed <- length(toremove_df$taxa[toremove_df$remove == 1])
  print(sprintf("Tree pruned from %d taxa to %d after collapsing %d clusters",
                num_taxa, num_taxa - num_removed, length(clusters)))
  return(list(toremove_df, node_annots))
}

# Get the node annotations given a tip annotation.
# If all descendant tips are the same, the node has the same annotation,
# otherwise the annotation is MIXED
get_node_annots <- function(tree, tip_annot) {
  tree <- reorder(tree, "pruningwise")
  num_nodes <- (length(tree$tip.label) + tree$Nnode)
  node_annot_list <- rep(0, length = num_nodes)
  rootnode <- length(tree$tip.label) + 1
  for (node in c(as.array(tree$edge[, 2]), rootnode)) {
    if (node <= length(tree$tip.label)) {
      node_annot_list[node] <- tip_annot[node]
    } else {
      children_nodes <- get_descendants(tree, node)
      locs_in_children <- c()
      for (c in children_nodes) {
        locs_in_children <- c(locs_in_children, node_annot_list[c])
      }
      locs_in_children <- unique(unlist(locs_in_children))
      if (("MIXED" %in% locs_in_children) || (length(locs_in_children) > 1)) {
        node_annot_list[node] <- "MIXED"
      } else {
        node_annot_list[node] <- node_annot_list[c]
      }
    }
  }
  return(node_annot_list)
}

# Get clusters of monophyletic nodes 
get_clusters <- function(tree, node_annot) {
  non_mixed <- which(node_annot != "MIXED")
  children_nodes <- c()
  for (internal_node in non_mixed[non_mixed > length(tree$tip.label)]) {
    children_nodes <- c(children_nodes, get_descendants(tree, internal_node))
  }
  return(setdiff(non_mixed[non_mixed > length(tree$tip.label)], children_nodes))
}

# Prune clusters of monophyletic nodes keeping N taxa
prune_clusters_n <- function(tree, cluster_nodes, n) {
  toremove_df <- data.frame(taxa = tree$tip.label,
                           cluster = 0,
                           remove = 0)
  for (c in cluster_nodes) {
    subtree_tips <- tree$tip.label[get_tips_from_node(tree, c)]
    toremove_df$cluster[toremove_df$taxa %in% subtree_tips] <- c
    subtree_size <- length(subtree_tips)
    if (subtree_size > n) {
      filtered <- sample(subtree_tips, subtree_size - n)
      toremove_df$remove[toremove_df$taxa %in% filtered] <- 1
    } else {
      filtered <- NA
    }
  }
  return(toremove_df)
}
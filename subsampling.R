library(ape)

# Helper functions
# Get location from string using delimiter and index
get_loc <- function(string, delimiter, index, reverse = FALSE) {
  if (reverse == TRUE) {
    splitted <- strsplit(string, split = delimiter, fixed = TRUE)
    reved <- lapply(splitted, rev)
    return(lapply(reved, "[[", index))
  } else {
    return(unlist(lapply(strsplit(string, split = delimiter, fixed = TRUE),
                  "[[", index)))
  }
}

# Get location from metadata file dataframe and taxon name
get_loc_from_meta <- function(n, metadf, loc_column) {
  loc <- metadf[metadf$name == n, loc_column]
  return(as.character(loc))
}

# Get an array of locations given an array of names and a metadata file
get_loc_array_meta <- function(name_array, meta, loc_column) {
  locs <- sapply(name_array, get_loc_from_meta, metadf = meta, loc_column = loc_column)
  return(as.character(locs))
}

# Get the descendant nodes given an ape tree and a node number
get_descendants <- function(tree, node) {
  if (node <= length(tree$tip.label)) {
    return(-1)
  } else {
    return(tree$edge[which(tree$edge[, 1] == node), 2])
  }
}

# Get all descendants of a node given an ape tree
get_all_descendants <- function(tree, node, curr = NULL) {
  if (!inherits(tree, "phylo")) {
    stop("tree should be an object of class \'phylo\'.")
  }

  if (is.null(curr)) {
    curr <- vector()
  }

  daughters <- tree$edge[tree$edge[, 1] == node, 2]
  curr <- c(curr, daughters)

  if (length(curr) == 0 && node <= Ntip(tree)) {
    curr <- node
  }

  w <- which(daughters > Ntip(tree))

  if (length(w) > 0) {
    for (i in seq_along(w)) {
      curr <- get_all_descendants(tree, daughters[w[i]], curr)
    }
  }

  return(curr)
}

# Get the descendant tips from a given node
get_tips_from_node <- function(tree, node) {
  desc <- get_all_descendants(tree, node, curr = NULL)
  return(desc[desc <= length(tree$tip.label)])
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

get_clusters <- function(tree, node_annot) {
  non_mixed <- which(node_annot != "MIXED")
  children_nodes <- c()
  for (internal_node in non_mixed[non_mixed > length(tree$tip.label)]) {
    children_nodes <- c(children_nodes, get_descendants(tree, internal_node))
  }
  return(setdiff(non_mixed[non_mixed > length(tree$tip.label)], children_nodes))
}

prune_clusters <- function(tree, cluster_nodes) {
  toremove_df <- data.frame(taxa = tree$tip.label, 
                           cluster = 0,
                           remove = 0)
  for (c in cluster_nodes) {
    subtree_tips <- tree$tip.label[get_tips_from_node(tree, c)]
    filtered <- sample(subtree_tips, length(subtree_tips) - 1)
    toremove_df$cluster[toremove_df$taxa %in% subtree_tips] <- c
    toremove_df$remove[toremove_df$taxa %in% filtered] <- 1
  }
  return(toremove_df)
}

downsample_tree <- function(tree, split_char, split_index, reverse) {
  num_taxa <- length(tree$tip.label)
  # Get tip annotations by delimiter and index, 
  # use reverse to count from the back
  tip_annot <- get_loc(tree$tip.label, split_char, split_index, reverse)
  node_annots <- get_node_annots(tree, tip_annot)
  clusters <- get_clusters(tree, node_annots)
  toremove_df <- prune_clusters(tree, clusters)
  num_removed <- length(toremove_df$taxa[toremove_df$remove == 1])
  print(sprintf("Tree pruned from %d taxa to %d after collapsing %d clusters",
                num_taxa, num_taxa - num_removed, length(clusters)))
  return(list(toremove_df, node_annots))
}

# From metadata file with location annotated as loc
downsample_tree_meta <- function(tree, meta, loc_column) {
  num_taxa <- length(tree$tip.label)
  # Get tip annotations by delimiter and index, use reverse to count from back
  tip_annot <- get_loc_array_meta(tree$tip.label, meta, loc_column)
  node_annots <- get_node_annots(tree, tip_annot)
  clusters <- get_clusters(tree, node_annots)
  toremove_df <- prune_clusters(tree, clusters)
  num_removed <- length(toremove_df$taxa[toremove_df$remove == 1])
  print(sprintf("Tree pruned from %d taxa to %d after collapsing %d clusters",
                num_taxa, num_taxa - num_removed, length(clusters)))
  return(list(toremove_df, node_annots))
}

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

# Keep only N per cluster
downsample_tree_n <- function(tree, n, split_char, split_index, reverse) {
  num_taxa <- length(tree$tip.label)
  # Get tip annotations by delimiter and index, use reverse to count from back
  tip_annot <- get_loc(tree$tip.label, split_char, split_index, reverse)
  node_annots <- get_node_annots(tree, tip_annot)
  clusters <- get_clusters(tree, node_annots)
  toremove_df <- prune_clusters_n(tree, clusters, n)
  num_removed <- length(toremove_df$taxa[toremove_df$remove == 1])
  print(sprintf("Tree pruned from %d taxa to %d after collapsing %d clusters. %d taxa removed.",
                num_taxa, num_taxa - num_removed, length(clusters), num_removed))
  return(list(toremove_df, node_annots))
}

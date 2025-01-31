# Tree manipulation and subsampling utility functions

downsample_tree_n_meta <- function(tree, meta, n = 1, loc_column = "loc") {
    # Get unique locations
    locs <- unique(meta[[loc_column]])
    
    # Initialize results dataframe
    results <- data.frame(
        taxa = tree$tip.label,
        cluster = NA,
        remove = 0,
        stringsAsFactors = FALSE
    )
    
    # Initialize node annotations
    node_annots <- list()
    
    # Process each location
    cluster_id <- 1
    for (loc in locs) {
        # Get taxa for this location
        loc_taxa <- meta$name[meta[[loc_column]] == loc]
        
        if (length(loc_taxa) <= n) {
            next  # Skip if already at or below target number
        }
        
        # Find monophyletic groups
        mono_groups <- find_monophyletic_groups(tree, loc_taxa)
        
        # Process each monophyletic group
        for (group in mono_groups) {
            if (length(group) <= n) {
                next  # Skip if already at or below target number
            }
            
            # Mark all but n taxa for removal
            to_remove <- sample(group, size = length(group) - n)
            results$cluster[results$taxa %in% group] <- cluster_id
            results$remove[results$taxa %in% to_remove] <- 1
            
            # Store node annotation
            mrca <- find_mrca(tree, group)
            node_annots[[as.character(mrca)]] <- list(
                cluster = cluster_id,
                location = loc,
                size = length(group)
            )
            
            cluster_id <- cluster_id + 1
        }
    }
    
    return(list(results, node_annots))
}

find_monophyletic_groups <- function(tree, taxa) {
    # Find all monophyletic groups within a set of taxa
    
    # Get all possible combinations of taxa
    n <- length(taxa)
    groups <- list()
    
    # Check each possible group size
    for (size in 2:n) {
        # Get all combinations of this size
        combs <- combn(taxa, size, simplify = FALSE)
        
        # Check each combination for monophyly
        for (group in combs) {
            if (is.monophyletic(tree, group)) {
                groups[[length(groups) + 1]] <- group
            }
        }
    }
    
    # Add single taxa as their own groups
    for (taxon in taxa) {
        groups[[length(groups) + 1]] <- taxon
    }
    
    return(groups)
}

find_mrca <- function(tree, taxa) {
    # Find Most Recent Common Ancestor node number for a group of taxa
    if (length(taxa) == 1) {
        return(which(tree$tip.label == taxa))
    }
    
    # Get node numbers for all taxa
    tip_nums <- match(taxa, tree$tip.label)
    
    # Find MRCA using ape function
    mrca <- getMRCA(tree, tip_nums)
    
    return(mrca)
}

is.monophyletic <- function(tree, taxa) {
    # Check if a group of taxa form a monophyletic group
    if (length(taxa) == 1) {
        return(TRUE)
    }
    
    # Get node numbers for all taxa
    tip_nums <- match(taxa, tree$tip.label)
    
    # Find MRCA
    mrca <- getMRCA(tree, tip_nums)
    
    # Get all descendants of MRCA
    desc <- extract.clade(tree, mrca)$tip.label
    
    # Check if all descendants are in our taxa group
    return(all(desc %in% taxa) && all(taxa %in% desc))
} 


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

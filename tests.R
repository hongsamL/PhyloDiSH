source("subsampling.R")

#Test the functions with a test newick tree and metadata
# load test_newick
tree <- ape::read.tree("example_files/example_tree.tre")

# Create a test dataframe with name and location columns
meta <- read.csv("example_files/example_metadata.csv")


# Test get_loc function
print("Testing get_loc")
test_locs <- get_loc(tree$tip.label,'_',2)
expected_locs <- meta$loc
stopifnot(all(test_locs == expected_locs))

# Test get_loc_from_meta function
print("Testing get_loc_from_meta")
test_name <- "A1_A" 
test_loc <- get_loc_from_meta(test_name, meta)
stopifnot(test_loc == "A")

# Test get_loc_array_meta function
print("Testing get_loc_array_meta")
test_names <- c("A1_A", "B2_B", "C3_C")
test_locs <- get_loc_array_meta(test_names, meta)
expected_locs <- c("A", "B", "C")
stopifnot(all(test_locs == expected_locs))

# Test get_descendants function
print("Testing get_descendants")
test_node <- 16 # Internal node
descendants <- get_descendants(tree, test_node)
stopifnot(length(descendants) > 0)

test_tip <- 1 # Tip node
tip_descendants <- get_descendants(tree, test_tip)
stopifnot(tip_descendants == -1)

# Test get_all_descendants function
print("Testing get_all_descendants")
test_node <- 14 #root 
all_descendants <- get_all_descendants(tree, test_node)
stopifnot(length(all_descendants) == 24)

#test get_tips_from_node
print("Testing get_tips_from_node")
test_node <- length(tree$tip.label)+1 
all_tips <- get_tips_from_node(tree,test_node)
stopifnot(length(all_tips) == test_node-1)

# Test downsample_tree_meta function
print("Testing downsample_tree_meta")
results <- downsample_tree_meta(tree, meta)
toremove_df <- results[[1]]
num_taxa <- length(tree$tip.label)
num_removed <- length(toremove_df$taxa[toremove_df$remove == 1])
node_annots <- results[[2]]
stopifnot(is.data.frame(toremove_df))
stopifnot(num_taxa - num_removed == 3)

# Test downsample_tree_n function
print("Testing downsample_tree_n")
n <- 2
results_n <- downsample_tree_n(tree, n, "_", 2, FALSE)
toremove_df_n <- results_n[[1]]
num_removed_n <- length(toremove_df_n$taxa[toremove_df_n$remove == 1])
node_annots_n <- results_n[[2]]
stopifnot(is.data.frame(toremove_df_n))
stopifnot(num_taxa - num_removed_n == 6)

print("All tests passed!")

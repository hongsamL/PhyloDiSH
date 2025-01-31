# String manipulation utility functions

get_loc <- function(x, delimiter = "_", index = 1, reverse = FALSE) {
    # Split string by delimiter and extract token at specified position
    # If reverse=TRUE, count position from end
    
    # Split each string by delimiter
    tokens <- strsplit(x, delimiter,fixed=TRUE)
    
    # Extract token at specified position
    result <- sapply(tokens, function(t) {
        if (reverse) {
            # Count from end if reverse=TRUE
            pos <- length(t) - index + 1
        } else {
            pos <- index
        }
        
        # Return NA if position is invalid
        if (pos < 1 || pos > length(t)) {
            return(NA)
        }
        
        return(t[pos])
    })
    
    return(result)
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



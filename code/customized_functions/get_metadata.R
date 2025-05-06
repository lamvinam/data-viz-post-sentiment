# --- R Notebook Chunk: Function to Get List Metadata with Missing & Duplicates ---

# Ensure you have dplyr and purrr installed and loaded
# install.packages("dplyr") # dplyr is part of the tidyverse
# install.packages("purrr") # purrr is part of the tidyverse
# library(dplyr)
# library(purrr)

#' Generates a combined metadata table for a list of data frames/tibbles.
#'
#' Includes source table name, dimensions, total missing cells,
#' proportion of missing cells, number of duplicate rows,
#' and a list of variables with their data types.
#'
#' @param list_of_data A named list where each element is a data frame or tibble.
#'   The names of the list elements will be used for the 'source_table' column.
#' @return A tibble containing combined metadata for all input tables.
#'         Returns an empty tibble with expected columns if the input list is empty or contains no data frames.
get_metadata <- function(list_of_data) { # Renamed function
  
  # Input Validation
  if (!is.list(list_of_data)) {
    stop("Input 'list_of_data' must be a list.")
  }
  
  if (length(list_of_data) == 0) {
    message("Input list is empty. Returning empty metadata table.")
    # Return an empty tibble with the expected columns
    return(dplyr::tibble(source_table = character(),
                         num_rows = integer(),
                         num_cols = integer(),
                         total_missing_cells = integer(), # Added
                         proportion_missing_cells = double(), # Added
                         num_duplicate_rows = integer(), # Added
                         variables_and_types = character()))
  }
  
  # Ensure the list is named for the 'source_table' column
  if (is.null(names(list_of_data))) {
    message("Input list is not named. Using indices as source_table names.")
    names(list_of_data) <- seq_along(list_of_data)
  }
  
  message("Generating metadata for each table in the list...")
  
  # Use purrr::map_dfr to iterate, apply function, and combine results row-wise
  # .id = "source_table" adds a column with the list item's name
  combined_metadata_table <- purrr::map_dfr(list_of_data, function(df) { # .y implicitly available if .id is used
    # Check if the current element is a data frame; skip if not
    if (!is.data.frame(df)) {
      # Accessing list name inside map_dfr when not passed as .y argument in function(df, .y) is tricky
      # We'll rely on .id adding the source_table column later, and this row will have NAs
      warning("List element is not a data frame. Skipping metadata extraction for this item.")
      return(NULL) # purrr::map_dfr drops NULL results. A more robust version would return an error row here.
    }
    
    # --- Calculate the 3 new metrics ---
    num_rows <- nrow(df)
    num_cols <- ncol(df)
    
    # Calculate total cells (handle zero dimensions)
    total_cells <- if (num_rows > 0 && num_cols > 0) num_rows * num_cols else 0
    
    # Calculate total missing cells
    total_missing_cells <- sum(is.na(df))
    
    # Calculate proportion of missing cells (handle division by zero)
    proportion_missing_cells <- if (total_cells > 0) total_missing_cells / total_cells else 0
    
    # Calculate duplicate rows
    num_duplicate_rows <- sum(duplicated(df))
    # --- End new metrics calculation ---
    
    
    # Get column names
    col_names <- names(df)
    
    # Get primary data types for each column
    # Use purrr::map_chr for a tidy approach to get the first class string
    col_types <- purrr::map_chr(df, ~class(.x)[1])
    
    # Combine names and types into the desired string format
    vars_and_types_vector <- paste0(col_names, " (", col_types, ")")
    
    # Collapse the vector into a single string separated by ", "
    vars_and_types_string <- paste(vars_and_types_vector, collapse = ", ")
    
    # Return a single-row tibble with metadata for this data frame
    dplyr::tibble(
      num_rows = num_rows,
      num_cols = num_cols,
      total_missing_cells = total_missing_cells, # Added
      proportion_missing_cells = proportion_missing_cells, # Added
      num_duplicate_rows = num_duplicate_rows, # Added
      variables_and_types = vars_and_types_string
    )
    
  }, .id = "source_table") # .id adds the column based on list names
  
  message("Finished generating combined metadata table.")
  
  # Return the combined metadata table
  return(combined_metadata_table)
}

# --- Example Usage ---
# Create sample tibbles in a named list
# df1 <- dplyr::tibble(ID = 1:10, Value = rnorm(10), Category = factor(sample(letters[1:2], 10, replace = TRUE)))
# df2 <- dplyr::tibble(Date = Sys.Date() + 0:5, Log = c(TRUE, FALSE, TRUE, TRUE, FALSE, NA), Name = c("A", "B", "C", "D", "E", "F"))
# df3 <- df1[c(1, 2, 2, 3), ] # df with duplicates
# df_list_example_enhanced <- list(DatasetA = df1, AnotherData = df2, DuplicatesTest = df3)


# Load libraries first if not already loaded
# library(dplyr)
# library(purrr)

# Get the combined metadata table with new columns
# combined_metadata_enhanced <- get_metadata(df_list_example_enhanced)

# Print the result
# print(combined_metadata_enhanced)

# Example with the list including a non-data frame (will be skipped with warning)
# df_list_example_with_error_simple <- list(DatasetA = df1, NotData = "hello", AnotherData = df2)
# combined_metadata_with_error_simple <- get_metadata(df_list_example_with_error_simple)
# print(combined_metadata_with_error_simple)
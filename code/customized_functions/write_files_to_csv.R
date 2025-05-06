# Load necessary library (for tibble and sample data)
library(dplyr)

# --- Function to write a list of data frames/tibbles to CSV files ---
#' Writes each data frame or tibble in a list to a separate CSV file.
#'
#' The filenames are generated from the names of the list elements.
#' If a list element has no name, a default name like "table_1" is used.
#'
#' @param list_of_tables A list where each element is a data frame or tibble.
#' @param path A character string specifying the directory where the CSV files will be saved.
#'                   The directory will be created if it does not exist.
#' @param row_names A logical value indicating whether to write row names. Defaults to FALSE.
#' @return Invisibly returns the list of tables (useful for piping).
#' @examples
#' # Create a sample list
#' sample_list <- list(
#'   dataset_A = data.frame(col1 = 1:3, col2 = c("A", "B", "C")),
#'   dataset_B = tibble(x = letters[1:2], y = 10:11)
#' )
#' # Define an output directory
#' output_folder <- "my_output_csvs"
#' # Write the tables to CSV
#' # write_files_to_csv(sample_list, output_folder)
#' # Check the 'my_output_csvs' folder for 'dataset_A.csv' and 'dataset_B.csv'
write_files_to_csv <- function(list_of_tables, path, row_names = FALSE) {
  # --- Input Validation ---
  if (!is.list(list_of_tables)) {
    stop("Input 'list_of_tables' must be a list.")
  }
  if (missing(path) || !is.character(path) || length(path) != 1) {
    stop("An output directory path (character string) must be provided.")
  }
  
  # --- Create Output Directory ---
  if (!dir.exists(path)) {
    cat(paste("Creating output directory:", path, "\n"))
    dir.create(path, recursive = TRUE) # recursive = TRUE allows creating parent directories
  } else {
    cat(paste("Output directory already exists:", path, "\n"))
  }
  
  # --- Iterate and Write Files ---
  cat("Starting to write tables to CSV...\n")
  for (i in seq_along(list_of_tables)) {
    current_table <- list_of_tables[[i]]
    table_name <- names(list_of_tables)[i] # Get the name of the list element
    
    # Generate a default name if the list element has no name
    if (is.null(table_name) || table_name == "") {
      table_name <- paste0("table_", i)
      cat(paste("Warning: List element at index", i, "has no name. Using default name:", table_name, "\n"))
    }
    
    # Check if the current item is a data frame or tibble before writing
    if (!is.data.frame(current_table)) {
      warning(paste("Skipping list element", table_name, "as it is not a data frame or tibble."))
      next # Skip to the next item in the loop
    }
    
    # Construct the full file path
    file_name <- paste0(table_name, ".csv")
    full_file_path <- file.path(path, file_name) # Use file.path for cross-platform compatibility
    
    # Write the data frame to CSV
    tryCatch({
      write.csv(current_table, file = full_file_path, row.names = row_names)
      cat(paste("Successfully wrote", table_name, "to:", full_file_path, "\n"))
    }, error = function(e) {
      warning(paste("Error writing table", table_name, "to CSV:", e$message))
    })
  }
  cat("Finished writing tables to CSV.\n")
  
  # --- Return Value ---
  # Invisibly return the original list (useful if you want to pipe this function)
  invisible(list_of_tables)
}

# --- Sample Usage ---

# Create a sample list of data frames/tibbles (some named, one unnamed)
sample_list_to_save <- list(
  customers = data.frame(
    CustomerID = 1:3,
    Name = c("Alice", "Bob", "Charlie")
  ),
  orders = tibble(
    OrderID = 101:104,
    CustomerID = c(1, 2, 1, 3),
    Amount = c(50.5, 75.0, 120.0, 30.0)
  ),
  # Unnamed list element
  data.frame(
    colA = c(TRUE, FALSE),
    colB = c("X", "Y")
  )
)

# Define the output directory path
my_csv_output_folder <- "my_cleaned_tables" # Change this path as needed

# --- Call the function to write the list of tables to CSV ---
# The function will print messages as it runs and save files
# The function also invisibly returns the list, which you could assign
# processed_list <- write_files_to_csv(sample_list_to_save, my_csv_output_folder)

# Example call:
# write_files_to_csv(sample_list_to_save, my_csv_output_folder)

# After running, check the 'my_cleaned_tables' folder in your working directory.
# You should find 'customers.csv', 'orders.csv', and 'table_3.csv'.

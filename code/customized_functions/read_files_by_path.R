# Ensure you have readr and purrr installed and loaded before using this function
# install.packages("readr")
# install.packages("purrr")
library(readr)
library(purrr) # purrr is part of the tidyverse

#' Reads all files matching a pattern in a directory into a list of data frames.
#'
#' @param directory_path The path to the directory containing the files. Defaults to the current working directory (".").
#' @param file_pattern A regular expression pattern to filter files. Defaults to ".\\.csv$" for CSV files.
#' @param name_list Logical. If TRUE, names the list elements using the filenames (without extension). Defaults to TRUE.
#' @param ... Additional arguments passed to readr::read_csv.
#' @return A list of data frames, or an empty list if no files are found.
read_files_by_path <- function(directory_path = ".", file_pattern = "\\.csv$", name_list = TRUE, ...) {
  
  # 1. List files matching the pattern in the specified directory
  file_paths <- list.files(path = directory_path, pattern = file_pattern, full.names = TRUE)
  
  # 2. Check if any files were found
  if (length(file_paths) == 0) {
    message("No files matching pattern '", file_pattern, "' found in directory: ", directory_path)
    return(list()) # Return an empty list
  }
  
  message("Found ", length(file_paths), " files matching pattern '", file_pattern, "'. Reading...")
  
  # 3. Read each file into a data frame using purrr::map
  #    Errors during reading will cause the function to stop unless handled inside map
  list_of_dataframes <- purrr::map(file_paths, readr::read_csv, show_col_types = FALSE, ...)
  
  # 4. Optionally name the list elements
  if (name_list) {
    names(list_of_dataframes) <- tools::file_path_sans_ext(basename(file_paths))
  }
  
  message("Finished reading files. Returning list of data frames.")
  
  # 5. Return the list of data frames
  return(list_of_dataframes)
}

# --- Example Usage ---
# Assuming you have some CSV files in your working directory
# Load libraries first if not already loaded in another chunk
# library(readr)
# library(purrr)

# Read all CSVs in the current working directory
# my_csv_data <- read_files_by_path()

# Read all .txt files from a specific folder
# my_text_data <- read_files_by_path(directory_path = "my_data_folder", file_pattern = "\\.txt$")

# Read CSVs and pass extra arguments to read_csv (e.g., different delimiter)
# my_semicolon_csvs <- read_files_by_path(file_pattern = "\\.csv$", delim = ";")
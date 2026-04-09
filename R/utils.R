#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL



#' Load data from designated format and folder
#'
#' @param data_folder the path to the data
#' @param data_name the name of data to be loaded
#' @param format_colnames whether to format column names so that they follow R
#'   naming conventions
#' \itemize{
#'  \item{`sas7bdat`} {SAS format with `haven::read_sas`}
#'  \item{`csv`} {CSV file supported by `readr::read_csv`}
#'  \item{`xlsx` or `xls`} {Excel sheet supported by `readxl::read_excel`}
#'  \item{`rds`} {rds file by `readr::read_rds`} 
#' }
#' @param ... other parameters 
#' @importFrom readr read_rds
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom haven read_sas
#'
#' @return the data set with column names supported in R
#' @noRd
# #' @export
#'
# #' @examples
load_data <- function(
    data_folder, 
    data_name, 
    format_colnames = TRUE,
    ...){
  
  # to remove redudant "/" if provided 
  n_string <- nchar(data_folder)
  last_string <- stringr::str_sub(data_folder, start = n_string)
  if(last_string == "/"){
    data_folder <- stringr::str_sub(data_folder, end = n_string - 1)
  }
  
  data_format <- stringr::str_split(data_name, "[.]")[[1]][2]
  data_to_read <- paste0(data_folder, "/", data_name)
  
  if (data_format == "sas7bdat"){
    dat <- haven::read_sas(data_to_read, ...)
  } else if (data_format == "csv"){
    dat <- readr::read_csv(data_to_read, ...)
  } else if (data_format %in% c("xlsx", "xls")){
    dat <- readxl::read_excel(data_to_read, ...)
  } else if (data_format %in% c("rds")){
    dat <- readr::read_rds(data_to_read, ...)
  }
  
  if(format_colnames){
    colnames(dat) <- tolower(colnames(dat))
    # replace special characters with underscore
    if (any(stringr::str_detect(names(dat), "[ .]"))){
      names(dat) <- stringr::str_replace_all(names(dat), "[ .]", "_")
    }
  }
  
  return(dat)
}


#' get the labels of a SAS data set
#'
#' @param sas_data 
#'
#' @return a tibble showing the column names, their associated labels, as well 
#'    as the location (column numbers)
#' @noRd
#'
get_sas7bdat_labels <- function(sas_data, df_view = TRUE, row_filter = NULL){
  
  var_labels <- sapply(sas_data, function(x) attr(x, "label"))
  var_format <- sapply(sas_data, class)
  
  result <- tibble::tibble(
    col_number = 1:length(var_labels),
    col_name = names(sas_data),
    col_label = as.list(var_labels), 
    col_format = as.list(var_format)
  )
  
  if(!is.null(row_filter)){
    result <- result %>% dplyr::filter(
      stringr::str_detect(
        as.character(col_label), stringr::regex(row_filter, ignore_case = TRUE)
        )
    )
  }
  
  if(df_view){
    View(result)
  } else{
    return(result)
  }
}



#' Use the labels as the names of the output if the data features labels (e.g., SAS)
#'
#' @param df a data frame
#'
#' @return a tibble with updated column names by the corresponding labels
#' @noRd
#'
use_label_as_name <- function(df){
  
  var_labels <- lapply(df, function(x) attr(x, "label"))
  var_names <- names(df)
  
  new_name <- rep(NA, length(var_labels))
  
  for(i in seq_along(var_labels)) {
    
    tmp <- var_labels[[i]]
    if (is.null(tmp)){
      new_name[i] <- var_names[i]
    } else if (tmp %in% new_name){
      new_name[i] <- paste0(var_names[i], "-", tmp)
    } else {
      new_name[i] <- tmp
    }
  }
  
  names(df) <- new_name
  
  return(df)
}



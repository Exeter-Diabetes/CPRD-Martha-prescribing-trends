# -----------------------------------------------
# functions/00_utils.R
# General helper utilities used across the analysis
# -----------------------------------------------

#' Check whether a vector is of type integer64
#'
#' @param x A vector.
#' @return Logical indicating whether x is integer64.
is_integer64 <- function(x) {
  inherits(x, "integer64")
}


#' Convert all integer64 columns in a data frame to integer
#'
#' @param df A data frame.
#' @return A data frame with integer64 columns converted to integer.
convert_integer64_to_integer <- function(df) {
  df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = where(is_integer64),
        .fns  = as.integer
      )
    )
}


#' Row-wise minimum date across multiple columns
#'
#' @param df A data frame.
#' @return A Date vector containing the minimum date for each row.
row_min_date <- function(df) {
  
  mat <- as.matrix(df)
  
  # Convert each column to days 
  num <- apply(mat, 2, function(col) as.numeric(as.Date(col)))
  
  # Apply row-wise minimum, but return NA if all entries are NA
  mins <- apply(num, 1, function(r) {
    if (all(is.na(r))) return(NA_real_)
    min(r, na.rm = TRUE)
  })
  
  # Convert back to Date 
  as.Date(mins, origin = "1970-01-01")
}


#' Safe pmin for Date vectors
#'
#' @param ... Date vectors.
#' @return A Date vector giving the minimum across inputs for each row.
safe_pmin_date <- function(...) {
  dots  <- list(...)
  
  # Make sure all inputs are Dates
  dates <- lapply(dots, as.Date)
  
  # Compute pmin across all input vectors
  res <- do.call(pmin, c(dates, list(na.rm = TRUE)))
  
  # pmin returns Inf when all inputs are NA; replace with NA
  res[is.infinite(as.numeric(res))] <- NA
  
  # Make output class Date
  as.Date(res, origin = "1970-01-01")
}

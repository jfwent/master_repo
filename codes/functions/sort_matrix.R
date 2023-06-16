# Define the sorting function
sort_matrix <- function(matrix) {
  sort_cols <- sort(colnames(matrix))
  matrix_sorted <- matrix[, sort_cols]
  
  sort_rows <- sort(rownames(matrix_sorted))
  matrix_sorted <- matrix_sorted[sort_rows,]
  
  return(matrix_sorted)
}

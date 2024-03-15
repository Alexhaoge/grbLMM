# Require complete observation for each subject
#' Split Data into k Folds by Proportion
#'
#' This function splits the input data into k folds for cross-validation, ensuring each subject has complete observations in each fold.
#' 
#' @param ds A data frame containing the dataset to be split. It should have a column 'id' indicating the subject identifiers.
#' @param prop Proportion of data to be used for each fold (0 < prop < 1).
#' 
#' @return A list of length k, where each element is a data frame representing a fold.
#' 
#' @examples
#' # Split data with each fold containing 20% of the data
#' folds <- k_fold_by_prop(dataset, prop = 0.2)
#'
#' @keywords data manipulation
#' @export
#'
k_fold_by_prop <- function(ds, prop) {
  if (prop <= 0 || prop >= 1) {
    stop("Invalid proportion")
  }
  gene_groups <- split(ds, ds$id)
  m <- nrow(gene_groups[[1]])
  fold_size <- max(1, round(m * prop))
  k <- floor(m / fold_size)
  fold_samples <- vector("list", length = k)

  for (gene in gene_groups){
    sampled_indices <- sample.int(m, replace = FALSE)
    # Assign sampled points to the each fold
    for (i in 1:k) {
      end_ind <- ifelse(i == k, m, i * fold_size)
      fold_indices <- sampled_indices[((i - 1) * fold_size + 1):end_ind]
      fold_samples[[i]] <- rbind(fold_samples[[i]], gene[fold_indices, ])
    }
  }
  return(fold_samples)
}
library(dplyr)
library(tidyr)
library(reshape2)

#' Find highesy correlations in your data
#'
#' @param data Your data in the form of a matrix, data.frame, or data.table
#'
#' @returns A data.frame of all the pairwise correlations of your data, in order of decreasing magnitude
#' @export
#'
#' @examples top_cors(df1)
urp_top_cors <- function(data) {
  tryCatch({

    # see which variables cannot go into correlation calculations
    nonNumericNames <- c()
    for(i in 1:ncol(data)){
      if(class(data[,i]) %in% c("integer", "numeric")){
        nonNumericNames <- c(nonNumericNames,names(data)[i])
      }
    }

    data <- data[,!names(data) %in% nonNumericNames]
    # data <- as.matrix(data)
    # 1. Compute the correlation matrix
    cor_matrix <- cor(data, use = "pairwise.complete.obs")

    # 2. Melt the correlation matrix into a long format
    cor_long <- melt(cor_matrix)

    # 3. Remove duplicates and self-correlations
    # Remove self-correlations
    cor_long <- cor_long %>%
      filter(Var1 != Var2) # keep rows that satisfy this condition

    doops <-
      duplicated(t(apply(cor_long[, c("Var1", "Var2")], 1, sort))) # boolean for duplicate rows

    cor_long <- cor_long[doops,] # sort by duplicates

    # 4. Sort by absolute correlation
    cor_top <- cor_long %>%
      mutate(abs_value = abs(value)) %>%
      arrange(desc(abs_value))
    cor_top$value <- round(cor_top$value, 3)
    cor_top$abs_value <- round(cor_top$abs_value, 3)
    return(cor_top)
  },
  warning = function(w) {
  },
  error = function(e) {
    # custom error handling
    message("Error: You entered variable type ",class(data),". Instead, ", e$message)
  })
}

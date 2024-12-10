simulate_session <- function(base,alpha,beta1,beta2,pi,ndraws,nchoices,J,seed) {
  set.seed(seed)
  
  get_params <- function(alpha,beta1,beta2,J) {
    a <- rgamma(J, shape = alpha[1], scale = alpha[2])
    b1 <- runif(J, min = beta1[1], max = beta1[2])
    b2 <- runif(J, min = beta2[1], max = beta2[2])
    return(list(a,b1,b2))
  }
  params <- get_params(alpha,beta1,beta2,J)
  a <- params[[1]]
  b1 <- params[[2]]
  b2 <- params[[3]]
  
  # Calculate utilities
  for (j in 1:J) {
    if (j <= nchoices) {
      base[, paste0("J",j)] <- (pi * a[j] * base$party + b1[j] * base$age + 
                                  b2[j] * base$origin) + rnorm(1)/2}
    else {
      base[, paste0("J",j)] <- 0
        # leads to choice probability being zero for words outside the choice set
    }
  }
  
  # Calculate choice probabilities
  base[, paste0("J", 1:J)] <- exp(base[, paste0("J", 1:J)])
  base$total <- rowSums(base[, paste0("J", 1:J)]) + 1
  for (j in 1:J) {
    base[, paste0("J",j)] <- base[, paste0("J",j)] / base$total
  }
  
  J_columns <- base[, grepl("^J", colnames(base))]
  draws_matrix <- matrix(0, nrow = 100, ncol = J)
  
  # Perform 100 draws for each individual (each row)
  for (i in 1:100) {
    # For each individual, draw 100 choices based on the probabilities in J_columns_normalized
    draws <- sample(1:J, size = ndraws, replace = TRUE, prob = J_columns[i, ])
    
    # Count the number of times each choice (J*) is selected and store it in the draws_matrix
    draws_table <- table(draws)  # counts how many times each column is chosen
    draws_matrix[i, as.numeric(names(draws_table))] <- draws_table
  }
  
  # Convert draws_matrix to a data frame to display
  draws_matrix_df <- as.data.frame(draws_matrix)
  output <- as.data.frame(cbind(base[c("id","party","age","origin")],draws_matrix_df))
  for (j in 1:J) {
    if (j > nchoices) {
      output[, paste0("V",j)] <- NA
      # leads to choice probability being zero
    }
  }
    # set values to NA for not yet available choices
  
  return(output)
}

cv_test_adjusted <- function (x, y, seed, adjusted_n1, adjusted_n2) 
{
  if (!missing(seed)) 
    set.seed(seed)
  if (!is.numeric(x) && (!is.numeric(y) | !is.character(y) | 
                         !is.integer(y))) {
    warning("x is not numeric or y is not numeric, character or integer: returning NA")
    return(NA_real_)
  }
  if (anyNA(x)) {
    warning("x cannot contain any NA values: returning NA")
    return(NA_real_)
  }
  if (anyNA(y)) {
    warning("y cannot contain any NA values: returning NA")
    return(NA_real_)
  }
  
  k <- length(unique(y)) # Number of groups
  
  n_j <- data.frame(table(y))$Freq # Initial sample size

  s_j <- aggregate(x, by = list(y), FUN = sd)[2] # sd per group
  x_j <- aggregate(x, by = list(y), FUN = mean)[2] # mean per group
  m_j <- n_j - 1 # df per groups
  
  # Provide adjusted df
  if (!is.null(adjusted_n1))
  { 
    cat(paste("DF for Group 1 adjusted manually from ", m_j[1], "to ", adjusted_n1 - 1, "\n"))
    m_j[1] <- adjusted_n1 - 1
  }
  if (!is.null(adjusted_n2))
  { 
    cat(paste("DF for Group 2 adjusted manually from ", m_j[2], "to ", adjusted_n2 - 1, "\n"))
    m_j[2] <- adjusted_n2 - 1
  }
  
  # Compute index
  D <- (sum(m_j * (s_j/x_j)))/sum(m_j)
  D_AD <- (sum(m_j * (s_j/x_j - D)^2))/(D^2 * (0.5 + D^2))
  p_value <- pchisq(D_AD, k - 1, lower.tail = FALSE)
  return(list(D_AD = D_AD, p_value = p_value))
}

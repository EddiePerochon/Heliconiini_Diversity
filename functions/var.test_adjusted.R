
var.test_adjusted <- function (x, y, ratio = 1, 
                               df_x = NULL, df_y = NULL, # Provide adjusted df for the test
                               alternative = c("two.sided", "less", "greater"), conf.level = 0.95, ...) 
{
  
  # Test data
  if (!((length(ratio) == 1L) && is.finite(ratio) && (ratio > 
                                                      0))) 
    stop("'ratio' must be a single positive number")
  alternative <- match.arg(alternative)
  
  if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
        (conf.level > 0) && (conf.level < 1))) 
    stop("'conf.level' must be a single number between 0 and 1")
  
  DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))
  
  # Prepare data
  if (inherits(x, "lm") && inherits(y, "lm")) {
    DF.x <- x$df.residual
    DF.y <- y$df.residual
    V.x <- sum(x$residuals^2)/DF.x
    V.y <- sum(y$residuals^2)/DF.y
  } else {
    x <- x[is.finite(x)]
    DF.x <- length(x) - 1L # Uncorrected df for X as N - 1
    if (DF.x < 1L) 
      stop("not enough 'x' observations")
    y <- y[is.finite(y)]
    DF.y <- length(y) - 1L # Uncorrected df for Y as N - 1
    if (DF.y < 1L) 
      stop("not enough 'y' observations")
    V.x <- var(x) # Compute variance of X
    V.y <- var(y) # Compute variance of X
  }
  
  # Provide adjusted df
  if (!is.null(df_x))
  { 
    cat(paste("DF for X adjusted manually from ", DF.x, "to ", df_x, "\n"))
    DF.x <- df_x
  }
  if (!is.null(df_y))
  { 
    cat(paste("DF for Y adjusted manually from ", DF.y, "to ", df_y, "\n"))
    DF.y <- df_y
  }

  # Compute test
  ESTIMATE <- V.x/V.y # Compute F-stat as the ratio of variance
  STATISTIC <- ESTIMATE/ratio # Compare to expectation (here expectation = 1)
  PARAMETER <- c(`num df` = DF.x, `denom df` = DF.y) # Can adjust parameter of the F-test by providing adjusted df for each variable
  PVAL <- pf(STATISTIC, DF.x, DF.y)
  if (alternative == "two.sided") {
    PVAL <- 2 * min(PVAL, 1 - PVAL)
    BETA <- (1 - conf.level)/2
    CINT <- c(ESTIMATE/qf(1 - BETA, DF.x, DF.y), ESTIMATE/qf(BETA, 
                                                             DF.x, DF.y))
  }
  else if (alternative == "greater") {
    PVAL <- 1 - PVAL
    CINT <- c(ESTIMATE/qf(conf.level, DF.x, DF.y), Inf)
  }
  else CINT <- c(0, ESTIMATE/qf(1 - conf.level, DF.x, DF.y))
  names(STATISTIC) <- "F"
  names(ESTIMATE) <- names(ratio) <- "ratio of variances"
  attr(CINT, "conf.level") <- conf.level
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
               p.value = PVAL, conf.int = CINT, estimate = ESTIMATE, 
               null.value = ratio, alternative = alternative, method = "F test to compare two variances", 
               data.name = DNAME)
  attr(RVAL, "class") <- "htest"
  return(RVAL)
}


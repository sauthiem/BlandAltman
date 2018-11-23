#' Bland-Altman analysis
#'
#' This function returns the Bland-Altman elements with 95% confidence
#' interval on the bias and on the limits of agreements. It also 
#' calculates the percentage of error. Data can be converted to 
#' relative estimation.
#' 
#' References: 
#' 1) Bland, J.M. & Altman, D.G., 1986. Statistical methods for assessing agreement
#' between two methods of clinical measurement. Lancet, 1(8476), pp.307–310.
#' 2) Giavarina, D., 2015. Understanding Bland Altman analysis. Biochemia Medica, 25(2), pp.141–151
#'   
#' @param x A numerical vector
#' @param y A numerical vector
#' @param perrcent bool for relative estimation.
#' @return All the element required to a BA analysis
#' @examples
#' BA.analysis(c(1,2,3,4), c(2,3,4,5))
#' @export


BA.analysis <- function(x, y, ...){
  k <- list(...)
  if (any(names(k) == 'percent')){
    percent <- k$percent
  } else {
    percent <- F
  }
  
  if (any(names(k) == 'conf.int')){
    conf.int <- k$conf.int
  } else {
    conf.int <- 0.95
  }
  
  # Verify length
  if (length(x) != length(y)){
    stop('x and y length are different')
  }
  
  # Mean average and differences
  if (percent == T){
    dataY <- (x - y) / ((x + y) / 2)
    dataY[is.na(dataY)] <- 0
  } else {
    dataY <- (x - y)
  }
  
  dataX <- (x+y) / 2
  
  # Statistiques
  n <- length(dataY)
  bias <- mean(dataY)
  agrt <- qnorm(1-((1-conf.int)/2))*sd(dataY)
  limit.agrmt.upper <- bias+agrt
  limit.agrmt.lower <- bias-agrt
  
  
  #Calculate confidence interval
  se_mean <- sqrt((sd(dataY)^2)/n)
  se_agrt <- sqrt((3*sd(dataY)^2)/n)
  tval <- qt(1-(1-conf.int)/2,n-1)
  
  bias.ci <- se_mean*tval
  limit.agrmt.ci <- se_agrt*tval
  
  limit.agrmt.upper.ci.upper <- limit.agrmt.upper + limit.agrmt.ci
  limit.agrmt.upper.ci.lower <- limit.agrmt.upper - limit.agrmt.ci
  
  limit.agrmt.lower.ci.upper <- limit.agrmt.lower + limit.agrmt.ci
  limit.agrmt.lower.ci.lower <- limit.agrmt.lower - limit.agrmt.ci
  
  
  # coefficient de variation: CV = sd/mean
  CVa = sd(x) / mean(x)
  CVm = sd(y) / mean(y)
  percentage.error <- sqrt(CVa^2+CVm^2)
  
  return(list('bias'=bias,
              'bias.ci'=bias.ci,
              'bias.ci.lower'=bias-bias.ci,
              'bias.ci.upper'=bias+bias.ci,
              'limit.agrmt.ci'=limit.agrmt.ci,
              'limit.agrmt.upper'=limit.agrmt.upper,
              'limit.agrmt.upper.ci.upper'=limit.agrmt.upper.ci.upper,
              'limit.agrmt.upper.ci.lower'=limit.agrmt.upper.ci.lower,
              
              'limit.agrmt.lower'=limit.agrmt.lower,
              'limit.agrmt.lower.ci.upper'=limit.agrmt.lower.ci.upper,
              'limit.agrmt.lower.ci.lower'=limit.agrmt.lower.ci.lower,
              'percentage.error'=percentage.error,
              'n'=n,
              'conf.int'=conf.int))
  
}


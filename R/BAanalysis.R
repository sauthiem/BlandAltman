#' Bland-Altman analysis
#'
#' This function returns the Bland-Altman (BA) elements with 95% confidence
#' interval (by default) on the bias and on the limits of agreements. It also
#' calculates the percentage of error. Data can be converted to
#' relative estimation. Validation of differences normality with
#' Shapiro-Wilk test.
#'
#' References:
#'
#' 1) Bland, J.M. & Altman, D.G., 1986. Statistical methods for assessing agreement
#' between two methods of clinical measurement. Lancet, 1(8476), pp.307–310.
#'
#' 2) Giavarina, D., 2015. Understanding Bland Altman analysis. Biochemia Medica, 25(2), pp.141–151
#'
#' @param a A numerical vector method A
#' @param b A numerical vector method B
#' @param percent bool for relative estimation (default F)
#' @param conf.int Confidence interval (default 0.95)
#' @return List if the element required to a BA analysis
#' @examples
#' ba <- BA.analysis(c(1,2,3,4), c(2,3,4,5))
#' @export


BA.analysis <- function(a, b, ...){
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
  if (length(a) != length(b)){
    stop('a and b length are different')
  }

  # Mean average and differences
  if (percent == T) {
    y <- (a - b) / ((a + b) / 2)
    y[is.na(y)] <- 0
  } else {
    y <- (a - b)
  }

  x <- (a + b)/2

  # Normality validation
  if (sd(y) == 0) {
    warning("Normality of the differences cannot be evaluated, beacuse all differences are the same")
  } else if (shapiro.test(y)$p.value < 0.05) {
    warning(paste("The differences are not normally distributed (Shapiro-Wilk test p-value = ",signif(shapiro.test(y)$p.value, 3),"), therefore the Bland-Altman analysis is not recommended.\n",
                  "Please see hist(BA.analysis(a,b)$y) or qqnorm(BA.analysis(a,b)$y, plot.it = T)\n",
                  "A log transformation can be tried.",
                  sep=""))
  }

  # Statistiques
  n <- length(x)

  bias <- mean(y)

  agrt <- qnorm(1-((1-conf.int)/2))*sd(y)
  limit.agrmt.upper <- bias+agrt
  limit.agrmt.lower <- bias-agrt


  #Calculate confidence interval
  se_mean <- sqrt((sd(y)^2)/n)
  se_agrt <- sqrt((3*sd(y)^2)/n)
  tval <- qt(1-(1-conf.int)/2,n-1)

  bias.ci <- se_mean*tval
  limit.agrmt.ci <- se_agrt*tval

  limit.agrmt.upper.ci.upper <- limit.agrmt.upper + limit.agrmt.ci
  limit.agrmt.upper.ci.lower <- limit.agrmt.upper - limit.agrmt.ci

  limit.agrmt.lower.ci.upper <- limit.agrmt.lower + limit.agrmt.ci
  limit.agrmt.lower.ci.lower <- limit.agrmt.lower - limit.agrmt.ci


  # coefficient de variation: CV = sd/mean
  # McLean (1997) Anaesthesia and Intensive Care, 25(3), pp.250–254.

  CVa = sd(a) / mean(a)
  CVb = sd(b) / mean(b)

  percentage.error <- sqrt((2*CVa)^2 + (2*CVb)^2)


  if (percent == T){
    fact <- 100
  } else {
    fact <- 1
  }

  return(list('x'= x,
              'y'= y*fact,
              'bias'=bias*fact,
              'bias.ci'=bias.ci*fact,
              'bias.ci.lower'=(bias-bias.ci)*fact,
              'bias.ci.upper'=(bias+bias.ci)*fact,
              'limit.agrmt.ci'=limit.agrmt.ci*fact,
              'limit.agrmt.upper'=limit.agrmt.upper*fact,
              'limit.agrmt.upper.ci.upper'=limit.agrmt.upper.ci.upper*fact,
              'limit.agrmt.upper.ci.lower'=limit.agrmt.upper.ci.lower*fact,

              'limit.agrmt.lower'=limit.agrmt.lower*fact,
              'limit.agrmt.lower.ci.upper'=limit.agrmt.lower.ci.upper*fact,
              'limit.agrmt.lower.ci.lower'=limit.agrmt.lower.ci.lower*fact,
              'percentage.error'=percentage.error,
              'n'=n,
              'conf.int'=conf.int))

}



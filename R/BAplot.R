#' Bland-Altman plot
#'
#' This function creates a Bland-Altman plot with 95% confidence
#' interval on the bias and on the limits of agreements. It also 
#' calculates the percentage of error. Data can be converted to 
#' relative estimation.
#' 
#' References: 
#' 1) Bland, J.M. & Altman, D.G., 1986. Statistical methods for assessing agreement
#' between two methods of clinical measurement. Lancet, 1(8476), pp.307–310.
#' 2) Giavarina, D., 2015. Understanding Bland Altman analysis. Biochemia Medica, 25(2), pp.141–151
#'   
#' @param x and y: numerical vector, title of the plot and percent (bool) for relative
#' estimation.
#' @return standard R plot 
#' @export


BA.plot <- function(x, y, ...){
  k <- list(...)
  if (any(names(k) == 'percent')){
    percent <- k$percent
  } else {
    percent <- F
  }
  
  if (any(names(k) == 'title')){
    title <- k$title
  } else {
    title <- ' '
  }
  
  
  # Verify length
  if (length(x) != length(y)){
    stop('x and y length are different')
  }
  
  # Clear plots
  if(!is.null(dev.list())) dev.off()
  
  # Mean average and differences
  if (percent == T){
    dataY <- (x - y) / ((x + y) / 2)
    dataY[is.na(dataY)] <- 0
  } else {
    dataY <- (x - y)
  }
  
  dataX <- (x+y) / 2
  
  # Statistiques
  meanDiff <- mean(dataY)
  agrt <- qnorm(1-((1-0.95)/2))*sd(dataY)
  
  #Calculate confidence interval
  se_mean <- sqrt((sd(dataY)^2)/length(dataY))
  se_agrt <- sqrt((3*sd(dataY)^2)/length(dataY))
  tval <- qt(1-(1-0.95)/2,length(dataY)-1)
  meanDiff.conf <- se_mean*tval
  agrt.conf <- se_agrt*tval
  
  # coefficient de variation: CV = sd/mean
  CVa = sd(x) / mean(x)
  CVm = sd(y) / mean(y)
  ErrorPercentage<-sqrt(CVa^2+CVm^2)*100
  
  # Graphique
  jitterLevel <- 0
  
  if(percent == T){
    plot(jitter(dataY, jitterLevel) ~ jitter(dataX, jitterLevel), 
         pch=19,  cex=0.6,
         col=rgb(red = 0.275, green = 0.51, blue = 0.706, alpha = 0.6),# 'steelblue',
         #  xlim = c(0,30),
         #   ylim = c(-3,3),
         yaxt = 'n',
         ylab = '% differences ± IC95%',
         xlab ='Mean'
    )
    
    # xlimits <- c(0,30)
    
    axis(2, at=pretty(-3:3), lab=paste(pretty(-3:3) * 100, '%'), las=TRUE, cex.axis=0.7)
    
  } else {
    plot(jitter(dataY, jitterLevel) ~ jitter(dataX, jitterLevel), 
         pch=19,  cex=0.6,
         col=rgb(red = 0.275, green = 0.51, blue = 0.706, alpha = 0.8),# 'steelblue',
         #  xlim = c(0,30),
         #  ylim = c(-40,40),
         ylab = 'Differences ± 95%IC',
         xlab = 'Mean'
    )
  }
  
  grid(NULL, NULL, lwd = 0.5, lty=1)
  
  abline(h=meanDiff, col='red', lty=1)
  abline(h=meanDiff+meanDiff.conf, col='darkgrey', lty=3)
  abline(h=meanDiff-meanDiff.conf, col='darkgrey', lty=3)
  abline(h=meanDiff+agrt, col='blue', lty=1)
  abline(h=meanDiff+agrt+agrt.conf, col='darkgrey', lty=3)
  abline(h=meanDiff+agrt-agrt.conf, col='darkgrey', lty=3)
  abline(h=meanDiff-agrt, col='blue', lty=1)
  abline(h=meanDiff-agrt+agrt.conf, col='darkgrey', lty=3)
  abline(h=meanDiff-agrt-agrt.conf, col='darkgrey', lty=3)
  
  title(title, line=2.5)
  mtext(paste('N=', length(dataX), 
              ', Bias ', round(meanDiff, 2), ' ±95CI ', round(meanDiff.conf, 2), 
              ' (Lim. agrmt: ', round(meanDiff-agrt, 2), ';', round(meanDiff+agrt, 2), ', ±95CI ', round(agrt.conf,2),')',
              ', Percentage of error: ', round(ErrorPercentage, 1), '%', 
              sep=''),
        side=3, line=1, cex = 0.7, outer=F)
}


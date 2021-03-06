#' Bland-Altman plot
#'
#' This function returns a Bland-Altman (BA) plot with 95% confidence
#' interval. It also calculates the percentage of error. Data can be converted to
#' relative estimation. Validation of differences normality with
#' Shapiro-Wilk test.
#'
#' @param x A numerical vector
#' @param y A numerical vector
#' @param title The title of the plot
#' @param percent bool for relative estimation (default F)
#' @param xlim c(x,y) limits on x axis
#' @param ylim c(x,y) limits on y axis
#' @param xlab x-axis title
#' @param ylab y-axis title
#' @param alpha point alpha (delfault 0.6)
#' @param pch point type (default 19)
#' @param conf.int Confidence interval (default 0.95)
#' @param hide.conf.int Hide confidence intervals (default FALSE)
#' @param hide.ci.text.plot Hide just the confidence interval values on the plot
#' @param reference c("A", "B", "mean") method to be used on X axis (default "mean")
#' @param save path to save plot (PDF). If only set to TRUE, title is used.
#' @param size c(width,height) in inch, default c(11,8.5)
#' @return Base R plot
#' @examples
#' BA.plot(c(1,2,3,4), c(2,3,4,5), title="My great title")
#' @export


BA.plot <- function(a, b, ...){
  ba <- BlandAltman::BA.analysis(a, b, ...)

  # Retrieve parameters
  k <- list(...)

  if (any(names(k) == 'title')){
    title <- k$title
  } else {
    title <-' '
  }

  if (any(names(k) == 'save')){
    save <- k$save
    if (save == T){
      save <- paste(title, '.pdf', sep="")
    }
  } else {
    save <- FALSE
  }

  if (any(names(k) == 'size')){
    size <- k$size
  } else {
    size <- c(11,8.5)
  }


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

  if (any(names(k) == 'hide.conf.int')){
    hide.conf.int <- k$hide.conf.int
  } else {
    hide.conf.int <- FALSE
  }

  if (any(names(k) == 'hide.ci.text.plot')){
    hide.ci.text.plot <- k$hide.ci.text.plot
  } else {
    hide.ci.text.plot <- F
  }

  if (any(names(k) == 'reference')){
    reference <- k$reference
  } else {
    reference <- "mean"
  }

  #Take into account NAs filtration
  a = ba$a
  b = ba$b

  # X axis: default mean(A,B)
  if (tolower(reference) == 'mean') {
    x <- (a + b)/2
    xlab <- 'Mean of method A and B'
  } else if (toupper(reference) == 'A') {
    x <- a
    xlab <- 'Method A'
  } else if (toupper(reference) == 'B') {
    x <- b
    xlab <- 'Method B'
  }

  # Y axis
  if (percent == T) {
    ylab <- paste('Relative differences (A - B) [%] (± ',ba$conf.int*100,'% CI)', sep="")

  } else {
    ylab <- paste('Absolute differences (A - B) (± ',ba$conf.int*100,'% CI)', sep="")

  }

  # Labs
  if (any(names(k) == 'xlab')){
    xlab <- k$xlab
  }

  if (any(names(k) == 'ylab')){
    ylab <- k$ylab
  }

  # Points
  if (any(names(k) == 'alpha')){
    alpha <- k$alpha
  } else {
    alpha <- 0.6
  }


  # Points
  if (any(names(k) == 'pch')){
    pch <- k$pch
  } else {
    pch <- 19
  }


  # Limits calculations
  if (any(names(k) == 'xlim')){
    xlim <- k$xlim
  } else {
    xlim <- c(min(x), max(x))
    #xlim <- c(xlim[1] - abs(xlim[1]*0.2) , xlim[2] + abs(xlim[2]*0.2))
  }

  if (any(names(k) == 'ylim')){
    ylim <- k$ylim
  } else {
    ylim <- c(min(c(ba$y, ba$limit.agrmt.lower.ci.lower)), max(c(ba$y, ba$limit.agrmt.upper.ci.upper)))
    #ylim <- c(ylim[1] - abs(ylim[1]*0.2) , ylim[2] + abs(ylim[2]*0.2))
  }


  if (save != F){
    cairo_pdf(filename=save, width = size[1], height = size[2])
  }

  par(mar = c(5,5,5,2.5))
  par(xpd=FALSE) # Draw inside the plot (grid)


  plot(x, ba$y,
       pch=pch,  cex=0.6,
       #   col=rgb(red = 0.275, green = 0.51, blue = 0.706, alpha = 0.7),# 'steelblue',
       col=rgb(red = 0.25, green = 0.25, blue = 0.25, alpha = alpha),# 'gray',
       bg=rgb(red = 0.25, green = 0.25, blue = 0.25, alpha = alpha),# 'gray',

       xlim = xlim,
       ylim = ylim,
       # yaxt = 'n', # Suppress y axes
       xaxs = "i", # Remove internal margins
       ylab = ylab,
       xlab = xlab,
  )

  grid(NULL, NULL, lwd = 0.5, lty=1)

  par(xpd=TRUE) # Draw outside the plot (legend)

  # Legends
  decalage <- ba$limit.agrmt.ci * 0.1

  # Upper CI limits of agreement
  if (hide.conf.int != T){
    polygon(c(xlim[1], xlim[1], xlim[2], xlim[2]),
            c(ba$limit.agrmt.upper.ci.upper, ba$limit.agrmt.upper.ci.lower, ba$limit.agrmt.upper.ci.lower , ba$limit.agrmt.upper.ci.upper),
            col = rgb(red = 0.1, green = 0.1, blue = 0.1, alpha = 0.15),
            border = NA)

    # Bias
    polygon(c(xlim[1], xlim[1], xlim[2], xlim[2]),
            c(ba$bias.ci.upper, ba$bias.ci.lower, ba$bias.ci.lower , ba$bias.ci.upper),
            col = rgb(red = 0.1, green = 0.1, blue = 0.1, alpha = 0.25),
            border = NA)

    # Lower CI limits of agreement
    polygon(c(xlim[1], xlim[1], xlim[2], xlim[2]),
            c(ba$limit.agrmt.lower.ci.upper, ba$limit.agrmt.lower.ci.lower, ba$limit.agrmt.lower.ci.lower , ba$limit.agrmt.lower.ci.upper),
            col = rgb(red = 0.1, green = 0.1, blue = 0.1, alpha = 0.15),
            border = NA)

    if(hide.ci.text.plot==F){
      text(xlim[2],ba$limit.agrmt.upper.ci.upper+decalage, round(ba$limit.agrmt.upper.ci.upper, 2), pos=4, cex=0.5)
      text(xlim[2],ba$limit.agrmt.upper.ci.lower-decalage, round(ba$limit.agrmt.upper.ci.lower, 2), pos=4, cex=0.5)

      text(xlim[2],ba$bias.ci.upper+decalage, round(ba$bias.ci.upper, 2), pos=4, cex=0.5)
      text(xlim[2],ba$bias.ci.lower-decalage, round(ba$bias.ci.lower, 2), pos=4, cex=0.5)

      text(xlim[2],ba$limit.agrmt.lower.ci.upper+decalage, round(ba$limit.agrmt.lower.ci.upper, 2), pos=4, cex=0.5)
      text(xlim[2],ba$limit.agrmt.lower.ci.lower-decalage, round(ba$limit.agrmt.lower.ci.lower, 2), pos=4, cex=0.5)
    }
  }

  # Zero, bias and limits of agreements
  segments(xlim[1], 0, xlim[2], 0, col='gray10', lty=5, lwd=1)

  segments(xlim[1], ba$limit.agrmt.lower, xlim[2], ba$limit.agrmt.lower, col='gray30', lty=3, lwd=1)
  segments(xlim[1], ba$bias, xlim[2], ba$bias, col='gray30', lty=1, lwd=1.5)
  segments(xlim[1], ba$limit.agrmt.upper, xlim[2], ba$limit.agrmt.upper, col='gray30', lty=3, lwd=1)


  text(xlim[2],ba$limit.agrmt.upper, round(ba$limit.agrmt.upper, 2), pos=4, cex=0.5)
  text(xlim[2],ba$bias, round(ba$bias, 2), pos=4, cex=0.5)
  text(xlim[2],ba$limit.agrmt.lower, round(ba$limit.agrmt.lower, 2), pos=4, cex=0.5)


  title(title, line=3)
  mtext(paste('N = ', length(x),
              ', Bias ', round(ba$bias, 2), ' (',ba$conf.int*100,'%CI ±', round(ba$bias.ci, 2),')\n',
              'Lim. agrmt (±',round(qnorm(1-((1-conf.int)/2)), 2),'SD): ', round(ba$limit.agrmt.lower, 2), ';', round(ba$limit.agrmt.upper, 2), ' (',ba$conf.int*100,'%CI ±', round(ba$limit.agrmt.ci,2),')\n',
              'Percentage of error: ', round(ba$percentage.error*100, 1), '%',
              sep=''),
        side=3, line=0.5, cex = 0.7, outer=F)

  if (save != F){
    dev.off()
  }

}

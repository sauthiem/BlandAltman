#' Agreement
#'
#' This function returns the overall and specific agreeement (Cicchetti 1990)
#'
#' @param x A numerical vector or a matrix
#' @param y A numerical vector
#' @return overall agreement, specific agreements per classes (+ or - or more) and the confusion matrix
#' @examples
#' agreement(c(1,2,2,3,2), c(1,2,4,3,2))
#' @export


agreement <- function(a, b = NULL){
  if (is.null(b) == F){
    if(length(a) == length(b)){
      mat <- as.matrix(cbind(a,b))
    }	else {
      stop("a and b length is different")
    }
  } else if (is.matrix(a) == T) {
    mat <- a
  } else {
    stop("Impossible")
  }

  liste_classes <- levels(factor(mat))
  cm <- table(factor(mat[,1], levels = liste_classes), factor(mat[,2], levels = liste_classes))
  overall.agree <- (cm[1,1] + cm[2,2])/sum(cm)

  specific.agree <- NULL
  for (ij in 1:length(liste_classes)) {
    a <- (2*cm[ij,ij])/(sum(cm[ij,])+sum(cm[,ij]))
    specific.agree <- c(specific.agree, a)
  }
  names(specific.agree) <- liste_classes

  return(list("conf.matrix"=cm,
              "overall"=overall.agree,
              "specific"=specific.agree
  ))
}



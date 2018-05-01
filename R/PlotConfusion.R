
#'Plot a confusion matrix
#'
#'@param Confusion_mat Confusion matrix
#'@return A pretty confusion matrix
#'@export

PlotConfusion <- function(Confusion_mat = NULL, is.percent = FALSE){

  if(is.percent==FALSE){
    scale_max <- ceiling(max(Confusion_mat))
  }
  if(is.percent==TRUE){
    scale_max <- 100
  }

  corrplot(Confusion_mat,
           method = "square", #circle, square, pie, shade, color
           col=viridis(40, direction=1, option="plasma",
                       begin = 0, end=1, alpha = 1),
           cl.lim = c(0, scale_max), cl.length=11,
           addCoef.col = "black", addCoefasPercent = F,
           tl.cex = 1.25, cl.cex = 1, number.cex = 1.25,
           is.corr = F)

}


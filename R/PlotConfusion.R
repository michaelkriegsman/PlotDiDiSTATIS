
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
                       begin = 0, end=.9, alpha = 1),
           cl.lim = c(0, scale_max), cl.length=11,
           addCoef.col = "black", addCoefasPercent = F,
           tl.cex = 1.25, cl.cex = 1, number.cex = 1.25,
           is.corr = F)


  ###Plot the Chi2 test, to see which cells are different from expected
  # Expect <- matrix(100/res_DiMDS$input$DESIGN_rows$B, res_DiMDS$input$DESIGN_rows$B, res_DiMDS$input$DESIGN_rows$B)
  # Chi2 <- (res_DiMDS$LOO_Rows$Confusion_rand_norm - Expect)^2 / Expect
  # corrplot(Chi2 * sign(res_DiMDS$LOO_Rows$Confusion_rand_norm - Expect), is.cor = FALSE, addCoef.col = "gray60")


}


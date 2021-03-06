
#'Plot a confusion matrix
#'
#'@param Confusion_mat Confusion matrix
#'@param scale_max_100 Boolean to set the max to 100
#'@param dev_new Flag to appease ReporteRs (set FALSE to print results to pptx)
#'@return A pretty confusion matrix
#'@export

PlotConfusion <- function(Confusion_mat = NULL, scale_max_100 = TRUE, dev_new = TRUE){

  if(scale_max_100==TRUE){
    scale_max <- 100
  }
  if(scale_max_100==FALSE){
    scale_max <- ceiling(max(Confusion_mat))
  }


  if(dev_new) dev.new()
  corrplot(Confusion_mat,
           method = "square", #circle, square, pie, shade, color
           col=viridis(40, direction=1, option="plasma", begin = 0, end=.9, alpha = 1),
           cl.lim = c(0, scale_max),
           cl.length=11,
           addCoef.col = "black",
           tl.cex = 1.25,
           cl.cex = 1,
           number.cex = 1.25,
           is.corr = F)


  ###Plot the Chi2 test, to see which cells are different from expected
  # Expect <- matrix(100/res_DiMDS$input$DESIGN_rows$B, res_DiMDS$input$DESIGN_rows$B, res_DiMDS$input$DESIGN_rows$B)
  # Chi2 <- (res_DiMDS$LOO_Rows$Confusion_rand_norm - Expect)^2 / Expect
  # corrplot(Chi2 * sign(res_DiMDS$LOO_Rows$Confusion_rand_norm - Expect), is.cor = FALSE, addCoef.col = "gray60")


}


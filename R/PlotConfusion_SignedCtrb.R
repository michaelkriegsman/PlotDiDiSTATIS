
#'Plot the signed contribution to Chi2
#'
#'@param Confusion_mat Confusion matrix
#'@return A pretty confusion matrix


PlotConfusion_SignedCtrb <- function(Confusion_mat = NULL, is.percent = FALSE){

  if(is.percent==FALSE){
    scale_max <- ceiling(max(Confusion_mat))
  }
  if(is.percent==TRUE){
    scale_max <- 100
  }

  Confusion_mat_norm <- (Confusion_mat / rowSums(Confusion_mat)) * 100
  Expect <- matrix(100/res_DiMDS$input$DESIGN_rows$B, res_DiMDS$input$DESIGN_rows$B, res_DiMDS$input$DESIGN_rows$B)
  Chi2 <- (Confusion_mat_norm - Expect)^2 / Expect

  corrplot(Chi2 * sign(Confusion_mat_norm - Expect),
           # method = "square",
           is.cor = FALSE, addCoef.col = "gray60",
           tl.cex = 1.25, cl.cex = 1, number.cex = 1.25)


}

# corrplot(Chi2 * sign(res_DiMDS$LOO_Rows$Confusion_rand_norm - Expect), is.cor = FALSE, addCoef.col = "gray60")











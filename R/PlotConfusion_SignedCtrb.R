
#'Plot the signed contribution to Chi2
#'
#'@param Confusion_mat_norm Confusion matrix, row-normed
#'@param scale_max_100 Boolean to set the max to 100%
#'@param dev_new Flag to appease ReporteRs (set FALSE to print results to pptx)
#'@return A pretty confusion matrix
#'@export


PlotConfusion_norm_SignedCtrb <- function(Confusion_mat_norm = NULL, DESIGN_rows = NULL, scale_max_100 = TRUE, dev_new = TRUE){

  if(scale_max_100==TRUE){
    Expected <- 100/DESIGN_rows$B
    #Largest possible observed value = 100
    scale_max <- ((100 - Expected)^2) / Expected
    #Smallest possible observed value = 0
    scale_min <- -1 * (( 0  - Expected)^2) / Expected
    TheScale <- c(scale_min, scale_max)
  }
  if(scale_max_100==FALSE){
    TheScale <- NULL
  }

  Expect <- matrix(100/DESIGN_rows$B, DESIGN_rows$B, DESIGN_rows$B)
  Chi2 <- (Confusion_mat_norm - Expect)^2 / Expect
  Signed_Ctrb <- Chi2 * sign(Confusion_mat_norm - Expect)

  if(dev_new) dev.new()
  corrplot(Signed_Ctrb,
           # method = "square",
           is.cor = FALSE,
           addCoef.col = "gray60",
           cl.lim = TheScale,
           tl.cex = 1.25,
           cl.cex = 1,
           number.cex = 1.25)


}











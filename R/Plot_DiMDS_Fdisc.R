#Plot_DiMDS_Fdisc.R
#
#' Plot Fdisc
#'
#' @param res_DiMDS The output of DiMDS
#' @param axes Axes to plot, default = c(1,2)
#' @param dev_new Flag to create a new device
#' @export

Plot_DiMDS_Fdisc <- function(res_DiMDS, axes = c(1,2), dev_new = T){

  alpha <- .8
  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
             col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_B,alpha),
             cex = 3, pch=15, dev.new=dev_new,
             constraints = minmaxHelper(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes]),
             main = "Discriminant Factor Scores, Fdisc")

  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes],
             col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_AB,alpha),
             cex = 1.5, pch=15,
             display_names = F,
             dev.new = F, new.plot = F)
}

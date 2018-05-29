#Plot_DiMDS_F.b.R
#
#' Plot F.b
#'
#' @param res_DiMDS The output of DiMDS
#' @param axes Axes to plot, default = c(1,2)
#' @param dev_new Flag to create a new device
#' @export

Plot_DiMDS_F.b <- function(res_DiMDS, axes = c(1,2), dev_new = T){

  alpha <- .8
  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
             col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_B, alpha),
             cex = 3, pch=15, dev.new = dev_new,
             xlab = paste0("Component ", axes[1]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][1],1), "% of Barycentric variance"),
             ylab = paste0("Component ", axes[2]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][2],1), "% of Barycentric variance"),
             constraints = minmaxHelper(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes]),
             main = "Barycentric Factor Scores, F.b")

}

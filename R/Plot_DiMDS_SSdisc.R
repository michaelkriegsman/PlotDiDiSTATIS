#Plot_DiMDS_SSdisc.R
#
#' Plot SSdisc
#'
#' @param res_DiMDS The output of DiMDS
#' @param axes Axes to plot, default = c(1,2)
#' @export

Plot_DiMDS_SSdisc <- function(res_DiMDS, axes = c(1,2)){

  alpha <- .8
  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes],
             col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_AB,alpha),
             cex = 1.5, pch=15, dev.new = F,
             display_names = F,
             main = paste0("SSdisc = ",round(res_DiMDS$res_Disc_Full$proj2Bary$SSdisc,3)))

  segments(x0 = 0,
           y0 = 0,
           x1 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[1]],
           y1 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[2]],
           col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_AB,alpha))

}

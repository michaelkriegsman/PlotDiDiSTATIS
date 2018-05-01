#Plot_DiMDS_SSab.R
#
#' Plot SSab
#'
#' @param res_DiMDS The output of DiMDS
#' @param axes Axes to plot, default = c(1,2)
#' @export

Plot_DiMDS_SSab <- function(res_DiMDS, axes = c(1,2)){

  alpha <- .8
  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes],
             col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_AB,alpha),
             cex = 1.5, pch=15, dev.new = F,
             display_names = F,
             main = paste0("SSab = ",round(res_DiMDS$res_Disc_Full$proj2Bary$SSab,3)))

  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
             dev.new = F, new.plot = F,
             cex = 3, pch=15,
             display_names = F,
             col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_B,alpha))

  segments(x0 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[1]],
           y0 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[2]],
           x1 = res_DiMDS$res_Disc_Full$eig$Fb_Full[,axes[1]],
           y1 = res_DiMDS$res_Disc_Full$eig$Fb_Full[,axes[2]],
           col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_AB, alpha))

}

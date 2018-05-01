#Plot_DiMDS_SS.b.R
#
#' Plot SS.b
#'
#' @param res_DiMDS The output of DiMDS
#' @param axes Axes to plot, default = c(1,2)
#' @export

Plot_DiMDS_SS.b <- function(res_DiMDS, axes = c(1,2)){

  alpha <- .8
  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes],
             col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_AB,alpha),
             cex = 1.5, pch=15, dev.new = F,
             display_names = F,
             main = paste0("SS.b = ",round(res_DiMDS$res_Disc_Full$proj2Bary$SSb,3)))

  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
             dev.new = F, new.plot = F,
             cex = 3, pch=15,
             display_names = F,
             col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_B,alpha))

  segments(x0 = 0,
           y0 = 0,
           x1 = res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes[1]],
           y1 = res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes[2]],
           col = add.alpha(res_DiMDS$input$DESIGN_rows$colors_B, alpha),
           lwd = 2)

}

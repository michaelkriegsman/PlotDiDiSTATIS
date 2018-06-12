#Plot_DiSTATIS_F_BD.R
#'
#' Plot DiSTATIS Compromise Factor Scores
#'
#' @param res_DiSTATIS The output of EigenDiSTATIS
#' @param axes Axes to plot, by default c(1,2)
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_DiSTATIS_F_BD <- function(res_DiSTATIS, axes = c(1,2), Flip_axis1 = FALSE, Flip_axis2=FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  prettyPlot(res_DiSTATIS$res_DiSTATIS$eig$F[,axes] %*% Flip_mat,
             dev.new = dev.new,
             display_names = FALSE,
             cex=1.5,
             xlab = paste0("Component ", axes[1]," variance = ", round(res_DiSTATIS$res_DiSTATIS$eig$t[axes][1],1), "%"),
             ylab = paste0("Component ", axes[2]," variance = ", round(res_DiSTATIS$res_DiSTATIS$eig$t[axes][2],1), "%"),
             col = add.alpha(res_DiSTATIS$input$DESIGN_rows$colors_AB, .5),
             # col = add.alpha("black", .7),
             pch=15,
             main = paste0('DiSTATIS F_BD'))

  prettyPlot(res_DiSTATIS$res_DiSTATIS$eig$F_Cond[,axes] %*% Flip_mat,
             display_names = F,
             dev.new = F, new.plot = F,
             # text.cex = 1.5,
             cex = 3.5, pch = 15,
             col = add.alpha(res_DiSTATIS$input$DESIGN_rows$colors_B, .8))

  for(d in 1:res_DiSTATIS$input$DESIGN_tables$D){
    #Compute the group partial factor scores
    Cd <- which(res_DiSTATIS$input$DESIGN_tables$mat[,d]==1)
    Group_PFS_BD <- res_DiSTATIS$input$DESIGN_rows$Pb_Cond %*% apply(res_DiSTATIS$res_DiSTATIS$ProjCP$F[,,Cd], c(1,2), mean)

    #Plot the group partial factor scores (segments, then factor scores)
    Segments_from_to(From = res_DiSTATIS$res_DiSTATIS$eig$F_Cond[,axes] %*% Flip_mat,
                     To = Group_PFS_BD[,axes] %*% Flip_mat,
                     XandY = c(1,2),
                     col = add.alpha(res_DiSTATIS$input$DESIGN_tables$colors_D[d], .7),
                     lwd = 2)

    prettyPlot(Group_PFS_BD[,axes] %*% Flip_mat,
               display_names = F,
               dev.new = F, new.plot = F,
               # text.cex = 1.5,
               cex = 2.5, pch = 19,
               col = add.alpha(res_DiSTATIS$input$DESIGN_tables$colors_D[d], .8))

  }

  legend(x = "right",
         legend = c(res_DiSTATIS$input$DESIGN_rows$labels, " ", res_DiSTATIS$input$DESIGN_tables$labels),
         col = add.alpha(c(res_DiSTATIS$input$DESIGN_rows$colors_B, 'white', res_DiSTATIS$input$DESIGN_tables$colors_D), .75),
         pch = c(rep(15, res_DiSTATIS$input$DESIGN_rows$B), 15, rep(19, res_DiSTATIS$input$DESIGN_tables$D)),
         pt.cex = 2)

}


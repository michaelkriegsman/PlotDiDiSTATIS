# Plot_DiDiSTATIS_FaB.d
#
#' Plot FaB.d (Discriminant Group Partial Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param priority #"ab", "d"
#' @param connect #0 = all, integer or vector connects those ab or d (depending on priority)
#' @param quiet_B.. Flag to suppress B..
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_DiDiSTATIS_FaB.d <- function(res_DiDiSTATIS, axes = c(1,2), priority = "ab", connect = 0, quiet_B.. = FALSE,
                                  Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  TheTitle <- paste0("DiDiSTATIS Disc Group Compromises, Fab.d \nMFA2 = ",as.character(res_DiDiSTATIS$input$MFA2_Flag)," \nPriority = ", priority, "\nConnect = ", deparse(connect))

  alpha_foreground <- .8
  alpha_background <- .4
  alpha_B.. <- alpha_background
  if(isTRUE(quiet_B..)){ alpha_B.. <- 0 }

  # Priority = "ab" -----
  if(priority=="ab"){
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha_B..),
               cex = 3, pch=15,
               dev.new = dev.new,
               xlab = paste0("Component ", axes[1]),
               ylab = paste0("Component ", axes[2]),
               constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes,])  %*% Flip_mat),
               main = TheTitle)

    Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$eig$Fb..[,axes] %*% Flip_mat,
                     To   = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes] %*% Flip_mat,
                     col  = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, alpha_B..),
                     lwd = 1)

    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes] %*% Flip_mat,
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, alpha_background),
               pch=15, cex = 1.75,
               dev.new = F, new.plot = F)


    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){

      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes,d] %*% Flip_mat,
                 # col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground),
                 cex=1.25, pch=16,
                 new.plot = F, dev.new = F)

      #if connect is 1 value, and that value equals 0...
      if(length(connect)==1){
        if(connect==0){
          Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes] %*% Flip_mat,
                           To   = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes,d] %*% Flip_mat,
                           col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D, alpha_foreground),
                           lwd = 1)
        }
      }
      #if connect is 1 value, and that value does not equal 0...
      if(length(connect)==1){
        if(connect!=0){
          Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[connect,axes] %*% Flip_mat,
                           To   = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[connect,axes,d] %*% Flip_mat,
                           col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground),
                           lwd = 3)
        }
      }
      #if connect is a vector
      if(length(connect)>1){
        Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[connect,axes] %*% Flip_mat,
                         To   = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[connect,axes,d] %*% Flip_mat,
                         col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground),
                         lwd = 3)
      }
    }

    legend(x = "right",
           legend = c(res_DiDiSTATIS$input$DESIGN_rows$labels, " ", res_DiDiSTATIS$input$DESIGN_tables$labels),
           col = c(add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha_background), 'white', add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D, alpha_foreground)),
           pch = c(rep(15, res_DiDiSTATIS$input$DESIGN_rows$B), 15, rep(19, res_DiDiSTATIS$input$DESIGN_tables$D)),
           pt.cex = 2)
  }
















  #### Priority = "d" ####
  if(priority=="d"){
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B,alpha_B..),
               cex = 3, pch=15,
               display_points = F,
               dev.new = dev.new,
               xlab = paste0("Component ", axes[1]),
               ylab = paste0("Component ", axes[2]),
               constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes,]) %*% Flip_mat),
               main = TheTitle)

    Group_cols <- res_DiDiSTATIS$input$DESIGN_tables$colors_D
    Group_cols[d] <- add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground)
    Group_cols[-d] <- add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[-d], alpha_background)

    legend(x = "right",
           legend = c(res_DiDiSTATIS$input$DESIGN_rows$labels, " ", res_DiDiSTATIS$input$DESIGN_tables$labels),
           col = c(add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha_foreground), 'white', Group_cols),
           pch = c(rep(15, res_DiDiSTATIS$input$DESIGN_rows$B), 15, rep(19, res_DiDiSTATIS$input$DESIGN_tables$D)),
           pt.cex = 2)

    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
      Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
                       To   = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes,d] %*% Flip_mat,
                       col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_background),
                       lwd = 1.5)

      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes,d] %*% Flip_mat,
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d],alpha_background),
                 cex=2, pch=19,
                 new.plot = F, dev.new = F)
    }

    #plot all Fab.d, and...
    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes,d] %*% Flip_mat,
                 # col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, alpha_foreground),
                 cex=1, pch=15,
                 new.plot = F, dev.new = F)


    }

    #if connect is 1 value...
    if(length(connect)==1){
      #... and that value equals 0, Then connect them to F.b.d
      if(connect==0){
        for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
          Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,axes,d] %*% Flip_mat,
                           To   = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes,d] %*% Flip_mat,
                           col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground),
                           lwd = 2)

        }
      }
      #... and that value does not equal 0... plot all Fab.d, and connect Fab.d[,,connect] to F.b.d[,,connect]
      if(connect!=0){
        for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
          Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,axes,connect] %*% Flip_mat,
                           To   = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes,connect] %*% Flip_mat,
                           col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[connect], alpha_foreground),
                           lwd = 2)
        }
      }
    }
    #if connect is a vector... plot all Fab.d, and connect Fab.d[,,connect] to F.b.d[,,connect]
    if(length(connect)>1){
      for(d in 1:length(connect)){
        Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,axes,connect[d]] %*% Flip_mat,
                         To   = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes,connect[d]] %*% Flip_mat,
                         col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground),
                         lwd = 2)
      }
    }
  }
}



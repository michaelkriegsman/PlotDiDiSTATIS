# Plot_DiDiSTATIS_Fab.d
#
#' Plot Fab.d (Discriminant Group Partial Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param priority #"ab", "d"
#' @param connect #0 = all, integer or vector connects those ab or d (depending on priority)
#' @export

Plot_DiDiSTATIS_Fab.d <- function(res_DiDiSTATIS, axes = c(1,2), priority = "ab", connect = 0){

  TheTitle <- paste0("Disc Group Compromises, Fab.d \nPriority = ", priority, "\nConnect = ", deparse(connect))
  alpha <- .8

  # Priority = "ab" -----
  if(priority=="ab"){
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
               cex = 3, pch=15,
               constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D)),
               main = TheTitle)

    segments(x0 = res_DiDiSTATIS$res_BaryGrand$eig$Fb..[,axes[1]],
             y0 = res_DiDiSTATIS$res_BaryGrand$eig$Fb..[,axes[2]],
             x1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes[1]],
             y1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes[2]],
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, alpha))

    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, alpha),
               pch=15, cex = 1.5,
               dev.new = F, new.plot = F)


    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){

      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
                 # col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
                 cex=1, pch=16,
                 new.plot = F, dev.new = F)

      #if connect is 1 value, and that value equals 0...
      if(length(connect)==1){
        if(connect==0){
          segments(x0 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes[1]],
                   y0 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes[2]],
                   x1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes[1],d],
                   y1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes[2],d],
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha))
        }
      }
      #if connect is 1 value, and that value does not equal 0...
      if(length(connect)==1){
        if(connect!=0){
          segments(x0 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[connect,axes[1]],
                   y0 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[connect,axes[2]],
                   x1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[connect,axes[1],d],
                   y1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[connect,axes[2],d],
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha),
                   lwd = 3)
        }
      }
      #if connect is a vector
      if(length(connect)>1){
        segments(x0 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[connect,axes[1]],
                 y0 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[connect,axes[2]],
                 x1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[connect,axes[1],d],
                 y1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[connect,axes[2],d],
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha),
                 lwd = 3)
      }

      ### For each D, put B hulls around their respective ab stimuli
      # for(b in 1:res_DiDiSTATIS$input$DESIGN_rows$B){
      #   these_stimuli <- which(res_DiDiSTATIS$input$DESIGN_rows$mat[,b]==1)
      #   hull.matrix <- res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[these_stimuli,,d]
      #   peeledHull(hull.matrix[,axes],percentage=.95,lwd=4)
      #   peeledHull(hull.matrix[,axes],percentage=.95,lwd=2,col=res_DiDiSTATIS$input$DESIGN_rows$colors_B[b])
      # }

    }
  }
















  #### Priority = "d" ####
  if(priority=="d"){
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
               cex = 3, pch=15,
               constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D)),
               main = TheTitle)

    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
      segments(x0 = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes[1]],
               y0 = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes[2]],
               x1 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes[1],d],
               y1 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes[2],d],
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha),
               lwd = 3)

      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d],
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d],alpha),
                 cex=2, pch=19,
                 new.plot = F, dev.new = F)
    }

    #plot all Fab.d, and...
    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
                 # col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
                 cex=1, pch=15,
                 new.plot = F, dev.new = F)
    }

    #if connect is 1 value...
    if(length(connect)==1){
      #... and that value equals 0, Then connect them to F.b.d
      if(connect==0){
        for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
          segments(x0 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,axes[1],d],
                   y0 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,axes[2],d],
                   x1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes[1],d],
                   y1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes[2],d],
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha),
                   lwd = 2)
        }
      }
      #... and that value does not equal 0... plot all Fab.d, and connect Fab.d[,,connect] to F.b.d[,,connect]
      if(connect!=0){
        for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
          segments(x0 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,axes[1],connect],
                   y0 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,axes[2],connect],
                   x1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes[1],connect],
                   y1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes[2],connect],
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[connect], alpha),
                   lwd = 2)
        }
      }
    }
    #if connect is a vector... plot all Fab.d, and connect Fab.d[,,connect] to F.b.d[,,connect]
    if(length(connect)>1){
      for(d in 1:length(connect)){
        segments(x0 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,axes[1],connect[d]],
                 y0 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,axes[2],connect[d]],
                 x1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes[1],connect[d]],
                 y1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,axes[2],connect[d]],
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha),
                 lwd = 2)
      }
    }
  }
}



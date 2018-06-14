#Print_res2ppt_DiDiSTATIS.R
#
#'Print standard DiDiSTATIS results
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param FabCd_ab For the FabCd plot, which stimulus to highlight
#' @param FabCd_d For the FabCd plot, which group to highlight
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param main Title
#' @return A Powerpoint of results
#' @export

Print_res2ppt_DiDiSTATIS <- function(res_DiDiSTATIS, axes = c(1,2), FabCd_ab = 1, FabCd_d = 1, Flip_axis1 = F, Flip_axis2 = F, main = NULL){

  # Create a PowerPoint document
  #for instructions, see: http://www.sthda.com/english/wiki/create-and-format-powerpoint-documents-from-r-software
  options("ReporteRs-default-font" = "Times New Roman")
  doc = pptx( )

  if(is.null(main)) main <- "Results"
  # Slide 0 : Title slide
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Title Slide")
  doc <- addTitle(doc, paste0(main,"\n MFA2 = ", res_DiDiSTATIS$input$MFA2_Flag))
  doc <- addDate(doc)

  # *** Add a slide to print the calls that were called... DATA, data_are, MFA1, RV1, MFA2, RV2 ####

  # Slide 1 : Add plot: F.B..
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "Barycentric Grand Factor Scores")
  plot_F.B.. <- function(){ Plot_DiDiSTATIS_F.B..(res_DiDiSTATIS, axes = axes, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, plot_F.B.., vector.graphic = TRUE)

  # Slide 2 : Add plot: FabCd
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Content with Caption")
  ab <- FabCd_ab
  d  <- FabCd_d
  doc <- addTitle(doc, paste0("FabCd, ab = ", ab, ", d = ", d))
  plot_FabCd <- function(){ Plot_DiDiSTATIS_FabCd(res_DiDiSTATIS, ab = ab, d = d, quiet_B.. = TRUE, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, plot_FabCd, vector.graphic = TRUE)

  # Slide Break : Grand Results ####
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Title Slide")
  doc <- addTitle(doc, paste0("Grand Results"))


  # Slide 1 : Add plot: F.B..
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Content with Caption")
  doc <- addTitle(doc, "Summary Stats")
  Flex_TheTable <- vanilla.table(round(res_DiDiSTATIS$TheTable, 2), add.rownames = T)
  doc <- addFlexTable(doc,  Flex_TheTable )


  # Slide 3 : Add plot: FAB.. & Confusion_Grand_Fixed
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "F_Disc^.. & Confusion_Grand_Fixed")
  plot_FAB.. <- function(){ Plot_DiDiSTATIS_FAB..(res_DiDiSTATIS, axes = axes, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, plot_FAB.., vector.graphic = TRUE)
  plot_Confusion_Fixed_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows, scale_max_100 = F, dev_new = F) }
  doc <- addPlot(doc, plot_Confusion_Fixed_Grand, vector.graphic = TRUE)
  doc <- addFooter(doc, paste0("r2_Categories^.. = ", round(res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Categories.., 2)))

  # Slide 3.1 : Add plot: Perfect Confusion Results would look like...
  Perfect_Conf_norm <- diag(rep(100, res_DiDiSTATIS$input$DESIGN_rows$B))
  dimnames(Perfect_Conf_norm) <- dimnames(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows)
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "Perfect Confusion Example")
  Perf_conf_norm <- function(){ PlotConfusion(Perfect_Conf_norm, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, Perf_conf_norm, vector.graphic = TRUE)
  Perf_signed_ctrb_norm <- function(){ PlotConfusion_norm_SignedCtrb(Perfect_Conf_norm, DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, Perf_signed_ctrb_norm, vector.graphic = TRUE)

  # Slide 3.5 :
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "Confusion_Grand_Fixed_norm & Signed_Ctrb")
  #
  plot_Confusion_norm_Fixed_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows_norm, scale_max_100 = F, dev_new = F) }
  doc <- addPlot(doc, plot_Confusion_norm_Fixed_Grand, vector.graphic = TRUE)
  plot_Confusion_SignedCtrb_norm_Fixed_Grand <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows_norm, DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = F, dev_new = F) }
  doc <- addPlot(doc, plot_Confusion_SignedCtrb_norm_Fixed_Grand, vector.graphic = TRUE)

  if("Perm_Omnibus" %in% names(res_DiDiSTATIS)){
    # Slide 4 : Add plot: Perm_r2_Categories
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Content with Caption")
    doc <- addTitle(doc, "Perm_r2_Categories^..")
    plot_Perm_r2_Categories.. <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories..(res_DiDiSTATIS, dev_new = F) }
    doc <- addPlot(doc, plot_Perm_r2_Categories.., vector.graphic = TRUE)
  }

  if("LOO_Rows" %in% names(res_DiDiSTATIS) & "SH_Rows" %in% names(res_DiDiSTATIS)){
    # Slide 5 : SH_Grand & LOO_Grand
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Two Content")
    doc <- addTitle(doc, "SH_Grand & LOO_Grand")
    plot_SH_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$SH_Rows$Grand$Confusion_rand_norm, scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_SH_Grand, vector.graphic = TRUE)
    plot_LOO_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$LOO_Rows$Grand$Confusion_rand_norm, scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_LOO_Grand, vector.graphic = TRUE)

    # Slide 5.5 : SH_Grand_Signed_Ctrb & LOO_Grand_Signed_Ctrb
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Two Content")
    doc <- addTitle(doc, "SH_Grand_Signed_Ctrb & LOO_Grand_Signed_Ctrb")
    plot_SH_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$SH_Rows$Grand$Confusion_rand_norm, DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_SH_Grand_Signed_Ctrb, vector.graphic = TRUE)
    plot_LOO_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$LOO_Rows$Grand$Confusion_rand_norm, DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_LOO_Grand_Signed_Ctrb, vector.graphic = TRUE)
  }
  # Slide Break : Group Results ####
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Title Slide")
  doc <- addTitle(doc, paste0("Group Results"))

  # Slide 6 : Add plot: F.B.D
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "F.B.D")
  plot_F.B.D <- function(){ Plot_DiDiSTATIS_F.B.D(res_DiDiSTATIS, axes = axes, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, plot_F.B.D, vector.graphic = TRUE)

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    # Slides 7.D : Add plot: FABd & Confusion_Fixed_Group_d
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Two Content")
    doc <- addTitle(doc, paste0("Group: ", res_DiDiSTATIS$input$DESIGN_tables$labels[d], "\n F_AB^.d & Group_Confusion_Fixed"))
    Plot_FAB.d <- function(){ Plot_DiDiSTATIS_FaB.d(res_DiDiSTATIS, priority = "d", connect = d, quiet_B.. = T, axes = axes, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
    doc <- addPlot(doc, Plot_FAB.d, vector.graphic = TRUE)
    plot_Confusion_Fixed_Group_d <- function(){ PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion.d[,,d], scale_max_100 = F, dev_new = F) }
    doc <- addPlot(doc, plot_Confusion_Fixed_Group_d, vector.graphic = TRUE)
  }

  if("Perm_Omnibus" %in% names(res_DiDiSTATIS)){
    # Slide 8 : Add plot: Perm_r2_Categories
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Content with Caption")
    doc <- addTitle(doc, "Perm_r2_Categories.D")
    plot_Perm_r2_Categories <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories.D(res_DiDiSTATIS, dev_new = F) }
    doc <- addPlot(doc, plot_Perm_r2_Categories, vector.graphic = TRUE)

    # Slide 9 : Add plot: Perm_r2_Groups
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Content with Caption")
    doc <- addTitle(doc, "Perm_r2_Groups_b")
    plot_Perm_r2_Groups <- function(){ Plot_DiDiSTATIS_Perm_r2_Groups_b(res_DiDiSTATIS, dev_new = F) }
    doc <- addPlot(doc, plot_Perm_r2_Groups, vector.graphic = TRUE)
  }

  if("Boot_Tables" %in% names(res_DiDiSTATIS)){
    # Slide 10 : Add plot: Boot_CIs
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Content with Caption")
    doc <- addTitle(doc, "Boot_CIs")
    plot_Boot_CIs <- function(){ Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_DiDiSTATIS, dev.new = F) }
    doc <- addPlot(doc, plot_Boot_CIs, vector.graphic = TRUE)

  }

  if("LOO_Rows" %in% names(res_DiDiSTATIS) & "SH_Rows" %in% names(res_DiDiSTATIS)){

    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
      # Slides 11.D :
      #+++++++++++++++++++++++
      doc <- addSlide(doc, "Two Content")
      doc <- addTitle(doc, paste0("SH & LOO Confusion: ", res_DiDiSTATIS$input$DESIGN_tables$labels[d]))
      plot_SH_Group_d <- function(){ PlotConfusion(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,d], scale_max_100 = T, dev_new = F) }
      doc <- addPlot(doc, plot_SH_Group_d, vector.graphic = TRUE)
      plot_LOO_Group_d <- function(){ PlotConfusion(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,d], scale_max_100 = T, dev_new = F) }
      doc <- addPlot(doc, plot_LOO_Group_d, vector.graphic = TRUE)

      # Slide 12.D :
      #+++++++++++++++++++++++
      doc <- addSlide(doc, "Two Content")
      doc <- addTitle(doc, paste0("SH & LOO Signed Ctrb: ", res_DiDiSTATIS$input$DESIGN_tables$labels[d]))
      plot_SH_Group_d_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,d], DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
      doc <- addPlot(doc, plot_SH_Group_d_Signed_Ctrb, vector.graphic = TRUE)
      plot_LOO_Group_d_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,d], DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
      doc <- addPlot(doc, plot_LOO_Group_d_Signed_Ctrb, vector.graphic = TRUE)
    }
  }

  pptFileName <- paste0("DiDiSTATIS ", main," ", Sys.Date(), "_",round(runif(1),3), ".pptx")
  writeDoc(doc, pptFileName)

}

#Print_res2ppt_DiSTATIS.R
#
#'Print standard DiSTATIS results
#'
#' @param res_DiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param Fik For Fik, which stimuli to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param main Title
#' @return A Powerpoint of results
#' @export

Print_res2ppt_DiSTATIS <- function(res_DiSTATIS, axes = c(1,2), Fik = 1, Flip_axis1 = F, Flip_axis2 = F, main = NULL){

  # Create a PowerPoint document
  #for instructions, see: http://www.sthda.com/english/wiki/create-and-format-powerpoint-documents-from-r-software
  options("ReporteRs-default-font" = "Times New Roman")
  doc = pptx( )

  if(is.null(main)) main <- "Results"
  # Slide 0 : Title slide
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Title Slide")
  doc <- addTitle(doc, paste0(main))
  doc <- addDate(doc)

  # Slide 2 : Add plot: F
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Content with Caption")
  doc <- addTitle(doc, "F")
  plot_F <- function(){ Plot_DiSTATIS_F(res_DiSTATIS, axes = axes, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = FALSE) }
  doc <- addPlot(doc, plot_F, vector.graphic = TRUE)


  # Slide 3 : Add plot: FiK; i = c(20,33)
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Content with Caption")
  doc <- addTitle(doc, "FiK")
  plot_FiK <- function(){ Plot_DiSTATIS_FiK(res_DiSTATIS, axes = axes, i = Fik, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = FALSE) }
  doc <- addPlot(doc, plot_FiK, vector.graphic = TRUE)


  # Slide 4 : Title slide
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Title Slide")
  doc <- addTitle(doc, "Explore Discriminant Effects")
  doc <- addDate(doc)


  # Slide 5 : Add plot: F_Plain
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "F_Plain")
  plot_F_Plain <- function(){ Plot_DiSTATIS_F_Plain(res_DiSTATIS, axes = axes, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, plot_F_Plain, vector.graphic = TRUE)


  # Slide 6 : Add plot: F_Colored
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "F_Colored")
  Plot_F_Colored <- function(){ Plot_DiSTATIS_F_Colored(res_DiSTATIS, axes = axes, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, Plot_F_Colored, vector.graphic = TRUE)


  # Slide 7 : Add plot: F_B
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "F_B")
  Plot_F_B <- function(){ Plot_DiSTATIS_F_B(res_DiSTATIS, axes = axes, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, Plot_F_B, vector.graphic = TRUE)


  # Slide 8 : Add plot: F_BD
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "F_B")
  Plot_F_BD <- function(){ Plot_DiSTATIS_F_BD(res_DiSTATIS, axes = axes, Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, Plot_F_BD, vector.graphic = TRUE)



  pptFileName <- paste0("DiSTATIS ", main," ", Sys.Date(), "_",round(runif(1),3), ".pptx")
  writeDoc(doc, pptFileName)

}

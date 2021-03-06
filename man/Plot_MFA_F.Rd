% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot_MFA_F.R
\name{Plot_MFA_F}
\alias{Plot_MFA_F}
\title{Plot MFA Consensus Factor Scores}
\usage{
Plot_MFA_F(res_MFA, axes = c(1, 2), Flip_axis1 = FALSE,
  Flip_axis2 = FALSE, dev.new = TRUE)
}
\arguments{
\item{res_MFA}{The output of EigenMFA}

\item{axes}{Axes to plot, by default c(1,2)}

\item{Flip_axis1}{Flag to flip Component 1}

\item{Flip_axis2}{Flag to flip Component 2}

\item{dev.new}{Flag to appease ReporteRs (set FALSE to print results to pptx)}
}
\description{
Plot MFA Consensus Factor Scores
}

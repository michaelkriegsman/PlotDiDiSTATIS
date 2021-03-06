% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot_MFA_FiK.R
\name{Plot_MFA_FiK}
\alias{Plot_MFA_FiK}
\title{Plot MFA Partial Factor Scores}
\usage{
Plot_MFA_FiK(res_MFA, axes = c(1, 2), i = 0, Flip_axis1 = FALSE,
  Flip_axis2 = FALSE, dev.new = TRUE)
}
\arguments{
\item{res_MFA}{The output of EigenMFA}

\item{axes}{Axes to plot, by default c(1,2)}

\item{i}{Which rows to plot (i = 0 gives all)}

\item{Flip_axis1}{Flag to flip Component 1}

\item{Flip_axis2}{Flag to flip Component 2}

\item{dev.new}{Flag to appease ReporteRs (set FALSE to print results to pptx)}
}
\description{
Plot MFA Partial Factor Scores
}

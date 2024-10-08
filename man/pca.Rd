% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.h.R
\name{pca}
\alias{pca}
\title{PCA & Group plot}
\usage{
pca(
  data,
  mode = "simple",
  labels,
  vars,
  eigen = FALSE,
  plot = FALSE,
  plot1 = FALSE,
  plot2 = FALSE,
  vars1,
  facs,
  plot3 = FALSE,
  plot4 = FALSE,
  width = 500,
  height = 500,
  width1 = 500,
  height1 = 500,
  width2 = 500,
  height2 = 500,
  width3 = 500,
  height3 = 500,
  width4 = 500,
  height4 = 500
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{mode}{.}

\item{labels}{.}

\item{vars}{.}

\item{eigen}{.}

\item{plot}{.}

\item{plot1}{.}

\item{plot2}{.}

\item{vars1}{.}

\item{facs}{.}

\item{plot3}{.}

\item{plot4}{.}

\item{width}{.}

\item{height}{.}

\item{width1}{.}

\item{height1}{.}

\item{width2}{.}

\item{height2}{.}

\item{width3}{.}

\item{height3}{.}

\item{width4}{.}

\item{height4}{.}
}
\value{
A results object containing:
\tabular{llllll}{
  \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
  \code{results$eigen} \tab \tab \tab \tab \tab a table \cr
  \code{results$plot} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot1} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot2} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot3} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot4} \tab \tab \tab \tab \tab an image \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$eigen$asDF}

\code{as.data.frame(results$eigen)}
}
\description{
PCA & Group plot
}

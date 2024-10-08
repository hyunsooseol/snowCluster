% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/famd.h.R
\name{famd}
\alias{famd}
\title{Factor analysis of mixed data}
\usage{
famd(
  data,
  vars,
  labels,
  eigen = TRUE,
  rowvar = "coordinates",
  colvar = "coordinates",
  quanvar = "coordinates",
  qualvar = "coordinates",
  ci = FALSE,
  cg = FALSE,
  quanti = FALSE,
  qual = FALSE,
  plot = TRUE,
  plot1 = FALSE,
  plot2 = FALSE,
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

\item{vars}{.}

\item{labels}{.}

\item{eigen}{.}

\item{rowvar}{.}

\item{colvar}{.}

\item{quanvar}{.}

\item{qualvar}{.}

\item{ci}{.}

\item{cg}{.}

\item{quanti}{.}

\item{qual}{.}

\item{plot}{.}

\item{plot1}{.}

\item{plot2}{.}

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
  \code{results$ci} \tab \tab \tab \tab \tab a table \cr
  \code{results$cg} \tab \tab \tab \tab \tab a table \cr
  \code{results$quanti} \tab \tab \tab \tab \tab a table \cr
  \code{results$qual} \tab \tab \tab \tab \tab a table \cr
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
Factor analysis of mixed data
}

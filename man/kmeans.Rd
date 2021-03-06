% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmeans.h.R
\name{kmeans}
\alias{kmeans}
\title{K-means clustering method}
\usage{
kmeans(
  data,
  vars,
  k = 2,
  algo = "Hartigan-Wong",
  nstart = 10,
  stand = FALSE,
  plot = TRUE,
  plot1 = FALSE,
  plot2 = FALSE,
  plot3 = FALSE
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{vars}{.}

\item{k}{.}

\item{algo}{.}

\item{nstart}{.}

\item{stand}{.}

\item{plot}{.}

\item{plot1}{.}

\item{plot2}{.}

\item{plot3}{.}
}
\value{
A results object containing:
\tabular{llllll}{
  \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
  \code{results$ss} \tab \tab \tab \tab \tab a table \cr
  \code{results$clustering} \tab \tab \tab \tab \tab a table \cr
  \code{results$centroids} \tab \tab \tab \tab \tab a table \cr
  \code{results$plot} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot1} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot2} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot3} \tab \tab \tab \tab \tab an image \cr
  \code{results$clust} \tab \tab \tab \tab \tab an output \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$ss$asDF}

\code{as.data.frame(results$ss)}
}
\description{
K-means clustering method
}

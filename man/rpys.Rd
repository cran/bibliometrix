% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rpys.R
\name{rpys}
\alias{rpys}
\title{Reference Publication Year Spectroscopy}
\usage{
rpys(M, sep = ";", timespan = NULL, median.window = "centered", graph = T)
}
\arguments{
\item{M}{is a data frame obtained by the converting function
\code{\link{convert2df}}. It is a data matrix with cases corresponding to
articles and variables to Field Tag in the original ISI or SCOPUS file.}

\item{sep}{is the cited-references separator character. This character separates cited-references in the CR
column of the data frame. The default is \code{sep = ";"}.}

\item{timespan}{is a numeric vector c(min year,max year). The default value is NULL (the entire timespan is considered).}

\item{median.window}{is a character string that can be "centered" or "backward". It indicates the type of median to be used. 
"centered" is the default value and it uses the centered 5-year median (t-2 to t+2) as proposed by Marx et al. (2014). "backward" uses the backward 5-year median (t-4 to t) as proposed by Aria and Cuccurullo (2017).}

\item{graph}{is a logical. If TRUE the function plot the spectroscopy otherwise the plot is created but not drawn down.}
}
\value{
a list containing the spectroscopy (class ggplot2) and three dataframes with the number of citations
per year, the list of the cited references for each year, and the reference list with citations recorded year by year, respectively.
}
\description{
\code{rpys} computes a Reference Publication Year Spectroscopy for detecting
the Historical Roots of Research Fields.
The method was introduced by Marx et al., 2014.\cr\cr
}
\details{
References:\cr\cr
Marx, W., Bornmann, L., Barth, A., & Leydesdorff, L. (2014).
Detecting the historical roots of research fields by reference publication
year spectroscopy (RPYS). Journal of the Association for Information Science and Technology,
65(4), 751-764.\cr\cr
Thor A., Bornmann L., Mark W. & Mutz R.(2018). 
Identifying single influential publications in a research field: new analysis opportunities of the CRExplorer. 
Scientometrics, 116:591–608 https://doi.org/10.1007/s11192-018-2733-7\cr\cr
}
\examples{

\dontrun{
data(management, package = "bibliometrixData")
res <- rpys(management, sep = ";", graph = TRUE)
}

}
\seealso{
\code{\link{convert2df}} to import and convert an ISI or SCOPUS
  Export file in a data frame.

\code{\link{biblioAnalysis}} to perform a bibliometric analysis.

\code{\link{biblioNetwork}} to compute a bibliographic network.
}

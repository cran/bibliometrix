#' Citation frequency distribution
#'
#' It calculates frequency distribution of citations.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Thomson Reuters' ISI Web of Knowledge file.
#' @param field is a character. It can be "article" or "author" to obtain frequency distribution of cited citations or cited first authors respectively. The default is \code{field = "article"}.
#' @param sep is the field separator character. This character separates citations in each string of CR column of the bibiographic data frame. The default is \code{sep = ";"}.
#' @return an object of \code{class} "table".
#'
#' 
#'
#' @examples
#' ## EXAMPLE 1: Cited articles
#' 
#' data(scientometrics)
#'
#' CR <- citations(scientometrics, field = "article", sep = ";")
#'
#' CR[1:10]
#'
#' ## EXAMPLE 2: Cited first authors
#' 
#' data(scientometrics)
#'
#' CR <- citations(scientometrics, field = "author", sep = ";")
#'
#' CR[1:10]
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#'

citations <- function(M, field = "article", sep = ";"){
  CR=NULL
  
  if (field=="article"){
    listCR=strsplit(M$CR,sep)
    listCR=lapply(listCR, function(l){
      ind=nchar(l)
      l=l[ind>2]
      listL=strsplit(unlist(l),",")
      l=lapply(listL,function(x){
        if(length(x)>3){x=paste(x[1:3],collapse=",")}
        else {return(x=paste(x,collapse=","))}
        })
      
      })
    
  }
  if (field=="author"){
    listCR=strsplit(M$CR,sep)
    listCR=lapply(listCR, function(l){
      ListL=lapply(strsplit(unlist(l),","),function(x) x[1])
      l=unlist(ListL)
      })
  }
  
  CR=trim.leading(unlist(listCR))
  CR=sort(table(CR=CR[CR!="NA"]),decreasing=TRUE)
  return(CR)
}
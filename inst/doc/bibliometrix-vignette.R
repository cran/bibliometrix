## ----Package citation----------------------------------------------------
citation("bibliometrix")

## ----Data loading--------------------------------------------------------
D <- readLines("http://www.bibliometrix.org/datasets/savedrecs.bib")

## ----Data converting-----------------------------------------------------
library(bibliometrix)

M <- convert2df(D, dbsource = "isi", format = "bibtex")

## ----biblioAnalysis------------------------------------------------------
results <- biblioAnalysis(M, sep = ";")

## ----summary generic function--------------------------------------------
S=summary(object = results, k = 10, pause = FALSE)

## ----plot generic function-----------------------------------------------
plot(x = results, k = 10, pause = FALSE)

## ------------------------------------------------------------------------
# M$CR[1]

## ----Article citation----------------------------------------------------
CR <- citations(M, field = "article", sep = ".  ")
CR[1:10]

## ----Author citation-----------------------------------------------------
CR <- citations(M, field = "author", sep = ".  ")
CR[1:10]

## ----Local Author citation-----------------------------------------------
CR <- localCitations(M, results, sep = ".  ")
CR[1:10]

## ----Dominance Ranking---------------------------------------------------
DF <- dominance(results, k = 10)
DF

## ----h-index-------------------------------------------------------------
indices <- Hindex(M, authors="BORNMANN L", sep = ";")

# Bornmann's impact indices:
indices$H

# Bornmann's citations
indices$CitationList


## ----h-index 10 authors--------------------------------------------------

authors=gsub(","," ",names(results$Authors)[1:10])

indices <- Hindex(M, authors, sep = ";")

indices$H

## ----Lotka law-----------------------------------------------------------
L <- lotka(results)

# Author Productivity. Empirical Distribution
L$AuthorProd

# Beta coefficient estimate
L$Beta

# Constant
L$C

# Goodness of fit
L$R2

# P-value of K-S two sample test
L$p.value


## ----Lotka law comparison, out.width='700px', dpi=200--------------------
# Observed distribution
Observed=L$AuthorProd[,3]

# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")

## ----Bipartite network---------------------------------------------------
A <- cocMatrix(M, Field = "SO", sep = ";")

## ----Most relevant sources-----------------------------------------------
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

## ------------------------------------------------------------------------
# A <- cocMatrix(M, Field = "CR", sep = ".  ")

## ------------------------------------------------------------------------
# A <- cocMatrix(M, Field = "AU", sep = ";")

## ------------------------------------------------------------------------
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
# A <- cocMatrix(M, Field = "AU_CO", sep = ";")

## ------------------------------------------------------------------------
# A <- cocMatrix(M, Field = "DE", sep = ";")

## ------------------------------------------------------------------------
# A <- cocMatrix(M, Field = "ID", sep = ";")

## ------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")

## ----similarity, out.width='700px', dpi=200------------------------------
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "sources", sep = ";")

# calculate jaccard similarity coefficient
S <- couplingSimilarity(NetMatrix, type="jaccard")

# plot journals' similarity (with min 3 manuscripts)
diag <- Matrix::diag
MapDegree <- 3
NETMAP <- S[diag(NetMatrix)>=MapDegree,diag(NetMatrix)>=MapDegree]
diag(NETMAP) <- 0

H <- heatmap(max(NETMAP)-as.matrix(NETMAP),symm=T, cexRow=0.3,cexCol=0.3)

## ------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")

## ------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")

## ------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

## ----igraph, out.width='700px', dpi=200----------------------------------

# Load package igraph (install if needed)

require(igraph)


## ----Country collaboration, out.width='700px', dpi=200-------------------
# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# define functions from package Matrix
diag <- Matrix::diag 
colSums <-Matrix::colSums

# delete not linked vertices
ind <- which(Matrix::colSums(NetMatrix)-Matrix::diag(NetMatrix)>0)
NET <- NetMatrix[ind,ind]

# Select number of vertices to plot
n <- 20    # n. of vertices
NetDegree <- sort(diag(NET),decreasing=TRUE)[n]
NET <- NET[diag(NET)>=NetDegree,diag(NET)>=NetDegree]

# delete diagonal elements (self-loops)
diag(NET) <- 0

# Create igraph object
bsk.network <- graph.adjacency(NET,mode="undirected")

# Compute node degrees (#links) and use that to set node size:
deg <- degree(bsk.network, mode="all")
V(bsk.network)$size <- deg*1.1

# Remove loops
bsk.network <- simplify(bsk.network, remove.multiple = F, remove.loops = T) 

# Choose Network layout
#l <- layout.fruchterman.reingold(bsk.network)
l <- layout.circle(bsk.network)
#l <- layout.sphere(bsk.network)
#l <- layout.mds(bsk.network)
#l <- layout.kamada.kawai(bsk.network)


## Plot the network
plot(bsk.network,layout = l, vertex.label.dist = 0.5, vertex.frame.color = 'blue', vertex.label.color = 'black', vertex.label.font = 1, vertex.label = V(bsk.network)$name, vertex.label.cex = 0.5, main="Country collaboration")


## ----Co-citation network, out.width='700px', dpi=200---------------------
# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")

# define functions from package Matrix
diag <- Matrix::diag 
colSums <-Matrix::colSums

# delete not linked vertices
ind=which(Matrix::colSums(NetMatrix)-Matrix::diag(NetMatrix)>0)
NET=NetMatrix[ind,ind]

# Select number of vertices to plot
n <- 10    # n. of vertices
NetDegree <- sort(diag(NET),decreasing=TRUE)[n]
NET <- NET[diag(NET)>=NetDegree,diag(NET)>=NetDegree]

# delete diagonal elements (self-loops)
diag(NET) <- 0

# Create igraph object
bsk.network <- graph.adjacency(NET,mode="undirected")

# Remove loops
bsk.network <- simplify(bsk.network, remove.multiple = F, remove.loops = T) 

# Choose Network layout
l = layout.fruchterman.reingold(bsk.network)

## Plot
plot(bsk.network,layout = l, vertex.label.dist = 0.5, vertex.frame.color = 'blue', vertex.label.color = 'black', vertex.label.font = 1, vertex.label = V(bsk.network)$name, vertex.label.cex = 0.5, main="Co-citation network")


## ----Keyword coupling, out.width='700px', dpi=200------------------------
# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "keywords", sep = ";")

# define functions from package Matrix
diag <- Matrix::diag 
colSums <-Matrix::colSums

# delete not linked vertices
ind=which(Matrix::colSums(NetMatrix)-Matrix::diag(NetMatrix)>0)
NET=NetMatrix[ind,ind]

# Select number of vertices to plot
n <- 10    # n. of vertices
NetDegree <- sort(diag(NET),decreasing=TRUE)[n]
NET <- NET[diag(NET)>=NetDegree,diag(NET)>=NetDegree]

# delete diagonal elements (self-loops)
diag(NET) <- 0

# Plot Keywords' Heatmap (most frequent 30 words)
n=30
NETMAP=NetMatrix[ind,ind]
MapDegree <- sort(diag(NETMAP),decreasing=TRUE)[n]
NETMAP <- NETMAP[diag(NETMAP)>=MapDegree,diag(NETMAP)>=MapDegree]
diag(NETMAP) <- 0

H <- heatmap(max(NETMAP)-as.matrix(NETMAP),symm=T, cexRow=0.3,cexCol=0.3)

# Create igraph object
bsk.network <- graph.adjacency(NET,mode="undirected")

# Remove loops
bsk.network <- simplify(bsk.network, remove.multiple = T, remove.loops = T) 

# Choose Network layout
l = layout.fruchterman.reingold(bsk.network)


## Plot
plot(bsk.network,layout = l, vertex.label.dist = 0.5, vertex.frame.color = 'black', vertex.label.color = 'black', vertex.label.font = 1, vertex.label = V(bsk.network)$name, vertex.label.cex = 0.5, main="Keyword coupling")

## ----Historical Co-citation network, out.width='700px', dpi=200----------
# Create a historical co-citation network

histResults <- histNetwork(M, n = 15, sep = ".  ")

# Create igraph object
bsk.network <- graph.adjacency(histResults[[1]],mode="directed")

# Remove loops
bsk.network <- simplify(bsk.network, remove.multiple = T, remove.loops = T) 

# Create the network layout (fixing vertical vertex coordinates by years)
l = layout.fruchterman.reingold(bsk.network)
l[,2]=histResults[[3]]$Year

# Plot the chronological co-citation network
plot(bsk.network,layout = l, vertex.label.dist = 0.5, vertex.frame.color = 'blue', vertex.label.color = 'black', vertex.label.font = 1, vertex.label = row.names(histResults[[3]]), vertex.label.cex = 0.4, edge.arrow.size=0.1)

# "Historical Network Legend"

print(histResults[[3]])



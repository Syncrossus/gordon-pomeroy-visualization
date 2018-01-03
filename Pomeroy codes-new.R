#load pomeroy
pomeroy <- read.csv("pomeroy.csv",stringsAsFactors=FALSE, header = TRUE)
#overview
View(as.data.frame(pomeroy[1:10, 1:10]))
which(is.na(pomeroy))
dim(pomeroy)
levels(pomeroy[,2])

#preprocessing
pomeroy <- t(pomeroy)
pomeroy.labels <- as.factor(pomeroy[2:nrow(pomeroy),1])
colnames(pomeroy)<- pomeroy[1,]
pomeroy <- pomeroy[2:nrow(pomeroy),2:ncol(pomeroy)]
pomeroy <- type.convert(x = pomeroy, dec = '.')

#PCA
library(rgl)
pomeroy.pca =prcomp(pomeroy)
plot(pomeroy.pca$x[,1:2],col= as.integer(pomeroy.labels))
plot3d(pomeroy.pca$x[,1:3],col= as.integer(pomeroy.labels) ,legend=TRUE, image=TRUE, size = 8,cex.legend = 1)

#LDA
library(MASS)
pomeroy.lda <- lda(formula=pomeroy.labels~., data = as.data.frame(pomeroy))
plot(pomeroy.lda, col=as.numeric(pomeroy.labels))

#find correlated variables
library(corrplot)
corrplot(cor(pomeroy), method = "color",tl.cex = 0.001,type="upper") # too much info, the pic is not clear
corrplot(cor(pomeroy[,2:100]),tl.cex = 0.001,type="upper")# see the first 100 variables correlation

#Isomap & LLE
library(RDRToolbox)
p.isomap = Isomap(data=pomeroy, dims=2, k=5)
p.lle = LLE(data=pomeroy, dim=2, k=5)
plot(p.isomap$dim2, col=as.numeric(pomeroy.labels))
plot(p.lle, col=as.numeric(pomeroy.labels))

#SOM
library(kohonen)
pomeroy.t = t(pomeroy)
## here, I tried to set the tumours as "variables" and gene as "samples" in order to figure out the key gene for tumours
data.som <- som(scale(pomeroy.t), grid= somgrid(10,5, topo= "hexagonal"))
plot(data.som)
colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}
plot(data.som, type = "counts", palette.name = colors, heatkey = TRUE)
## The method is not good for this task

# computing correlation of all variables with class
pomeroy.cor <- cor(pomeroy, as.numeric(pomeroy.labels))
# ordering dimensions as a function of their correlation to the labels
pomeroy.ordered <- pomeroy[,rev(order(abs(pomeroy.cor)))]
# We can probably assume we will have enough of 75 dimensions, especially given how few observations we have for the number of dimensions
pomeroy.select <- pomeroy.ordered[, 1:75]
# let's view the firsttwo dimensions
plot(pomeroy.select[1:42,1:2],col=as.numeric(pomeroy.labels))
library(rgl)
plot3d(pomeroy.select[1:42,1:3], col=as.numeric(pomeroy.labels),size = 10)
# The result is not very good
##PCA viz
pomeroy.cor.pca <- prcomp(pomeroy.select)
plot(pomeroy.cor.pca$x[,1:2],col= as.numeric(pomeroy.labels))
##result is not good
##LDA vis
pomeroy.sel.lda <- lda(formula=pomeroy.labels~., data = as.data.frame(pomeroy.select))
plot(pomeroy.sel.lda, col= as.numeric(pomeroy.labels))
##result is ok

#MDS
library(MASS)
p.mds <- isoMDS(dist(pomeroy.select), k=2, trace = TRUE)
plot(p.mds$points, col= as.numeric(pomeroy.labels))
# the results are not good

#The best result is from LDA after correlation analysis

#### preprocessing ####
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

#### raw visualization ####
#PCA
library(rgl)
pomeroy.pca = prcomp(pomeroy)
plot(pomeroy.pca$x[,1:2],col= as.integer(pomeroy.labels), xlab="Pomeroy PCA dimension 1", ylab="Pomeroy PCA dimension 2")
plot3d(pomeroy.pca$x[,1:3],col= as.integer(pomeroy.labels) ,legend=TRUE, image=TRUE, size = 8,cex.legend = 1)

#LDA
library(MASS)
pomeroy.lda <- lda(formula=pomeroy.labels~., data = as.data.frame(pomeroy))
plot(pomeroy.lda, col=as.numeric(pomeroy.labels))

#MDS
pomeroy.mds <- isoMDS(dist(pomeroy), k=10, trace = TRUE)
plot(pomeroy.mds$points, col=as.numeric(as.factor(pomeroy.labels)))
#same as PCA

#Isomap
library(vegan)
pomeroy.isomap <- isomap(dist = dist(pomeroy), ndim = 2, k=10, fragmentedOK = TRUE)
plot(pomeroy.isomap, col=as.numeric(pomeroy.labels), xlab="Pomeroy ISOMAP dimension 1", ylab="Pomeroy ISOMAP dimension 2")


#### Variable selection ####
# computing correlation of all variables with class
pomeroy.cor <- cor(pomeroy, as.numeric(pomeroy.labels))
# ordering dimensions as a function of their correlation to the labels
pomeroy.ordered <- pomeroy[,rev(order(abs(pomeroy.cor)))]
# After some experimentation, about 75 variables seemed optimal
pomeroy.select <- pomeroy.ordered[, 1:75]

#### final visualization ####
#PCA
pom.sel.pca <- prcomp(pomeroy.select)
plot(pom.sel.pca$x[,1:2],col= as.numeric(pomeroy.labels))
# No identifiable pattern in data

#LDA
pom.sel.lda <- lda(formula=pomeroy.labels~., data = as.data.frame(pomeroy.select))
plot(pom.sel.lda, col= as.numeric(pomeroy.labels))
#each class is linearly separable from the others

#MDS
library(MASS)
pom.sel.mds <- isoMDS(dist(pomeroy.select), k=2, trace = TRUE)
plot(pom.sel.mds$points, col= as.numeric(pomeroy.labels))
# Same as PCA

#Isomap
library(vegan)
pom.sel.isomap <- isomap(dist = dist(pomeroy.select), ndim = 2, k=10, fragmentedOK = TRUE)
plot(pom.sel.isomap, col=as.numeric(pomeroy.labels), xlab="Pomeroy ISOMAP dimension 1", ylab="Pomeroy ISOMAP dimension 2")
# No identifiable pattern in data
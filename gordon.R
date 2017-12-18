#load gordon
gordon <- read.csv("C:/Users/Syncrossus/Documents/Projet_Visualisation/gordon-2002_database.txt", header = TRUE, sep = "\t")
#getting an idea of what gordon is like
View(gordon[1:10, 1:10])
which(is.na(gordon))
dim(gordon)
levels(gordon[,2])

#strangely, observations seem to be columns and variables rows... also, the row labels are in the first column
gordon <- t(gordon)
gordon.labels <- as.factor(gordon[2:nrow(gordon),1])
colnames(gordon)<- gordon[1,]
gordon <- gordon[2:nrow(gordon),2:ncol(gordon)]
View(gordon[1:10, 1:10])

#Getting better understanding of gordon
summary(gordon)
boxplot(gordon[,sample(1:ncol(gordon), 5)])
gordon[,1]

# the numeric values seem to not be numeric
# I haven't found a smarter way to convert all cells to numeric without losing column names, row names, or 2D structure.
gordon <- type.convert(x = gordon, dec = '.')
View(gordon[1:10, 1:10])

#Trying again
summary(gordon)
boxplot(gordon[,sample(1:ncol(gordon), 5)])
gordon[,1]

# Given that the variables describe gene expression, extracting features doesn't make much sense : we want to know which genes are most correlated to a given illness.
# Therefore, we will select the features most correlated to the labels

# However, let us plot the 2 first dimensions of the PCA just to get an idea
gordon.pca <- prcomp(gordon)
plot(gordon.pca$x[, 1:2], col=as.numeric(gordon.labels))
# The classes are mostly seperate. However, the separation is not linear in the two first dimensions of the PCA and there is some overlap between classes


# Variable selection
# computing correlation of all variables with class
gordon.cor <- cor(gordon, as.numeric(gordon.labels))
# ordering dimensions as a function of their correlation to the labels
gordon.ordered <- gordon[,rev(order(abs(gordon.cor)))]

# viewing 10 variables most correlated with class
gordon.ordered[1:10,1:10]

# We can probably assume we will have enough of 100 dimensions, especially given how few observations we have for the number of dimensions
gordon.select <- gordon.ordered[, 1:100]
# let's view the first dimensions
plot(gordon.select[,1:2], col=as.numeric(gordon.labels))
# separability is better than with PCA, and the density of the classes in these dimensions is very different.

# let us view the data by reducing the dimensions by MDS
#MDS
library(MASS)
gordon.mds <- isoMDS(dist(gordon.select), k=2, trace = TRUE)
plot(gordon.mds$points, col=as.numeric(as.factor(gordon.labels)))
# the data is linearly separable.

# As we have the classes of the data points, we can try an LDA
gordon.lda <- lda(formula=gordon.labels~., data = as.data.frame(gordon.select))
plot(gordon.lda)
# The LDA reduces the space to 1 dimension with the datapoints linearly separable.
















# # removing redundant dimensions
# gordon.select <- gordon.ordered
# for (i in 1:10) {
#   gordon.select <- gordon.select[,-which(abs(cor(gordon.select)[i,])>0.60)]
# }
# gordon.select10 <- gordon.select[,1:10]
# 
# 
# 
# #PCA
# gordon.pca <- prcomp(gordon)
# plot(gordon.pca$sdev)
# plot(gordon.pca$x[, 1:2], col=as.numeric(as.factor(gordon.labels)))
# 
# #LDA
# library(MASS)
# gordon.lda <- lda(formula=gordon.labels~., data = as.data.frame(gordon))
# plot(gordon.lda, col = as.numeric(as.factor(gordon.labels)))
# plot(x=gordon.lda$scaling, y = as.numeric(as.factor(gordon.labels)))
# View(gordon.lda$scaling)
# 
# #MDS
# library(MASS)
# gordon.mds <- isoMDS(dist(gordon.ordered[,1:100]), k=2)
# plot(gordon.mds$points, col=as.numeric(as.factor(gordon.labels)))
# 
# #SOM
# library(kohonen)
# gordon.som <- som(X = gordon, grid = somgrid(xdim = 12, ydim=12, topo="hexagonal"), rlen=200)
# plot(gordon.som, type="changes")
# plot(gordon.som, type="count")
# plot(gordon.som, type="dist.neighbours")
# 
# 
# 
# # plotting
# plot(gordon.select10[,1:2], col=as.numeric(as.factor(gordon.labels)))
# pairs(gordon.select10[,1:4], col=as.numeric(as.factor(gordon.labels)))
# 
# #LDA
# library(MASS)
# nMPM <- sum(as.numeric(as.factor(gordon.labels))-1)
# gordon.s10.lda <- lda(formula = as.numeric(as.factor(gordon.labels))~., data = data.frame(gordon.ordered[,1:100]))
# gordon.s10.lda$terms
# plot(gordon.s10.lda)
# #
# 
# 
# #PCA
# library(FactoMineR)
# gordon.s10.pca <- PCA(gordon.ordered[,1:100], scale.unit = TRUE)
# plot.PCA(gordon.s10.pca)
# plot(gordon.s10.pca$ind$coord, col = as.numeric(as.factor(gordon.labels)))
# #
# 
# 
# 
# 
# 
# 
# 
# ##### doesn't work
# gordon.chisq <- t(scale(t(gordon)))
# gordon.chisq.cor <- cor(gordon.chisq, as.numeric(as.factor(gordon.labels)))
# gordon.chisq.select10 <- gordon.chisq[,rev(order(abs(gordon.chisq.cor)))[1:10]]
# plot(gordon.chisq.select10[,2:3], col=as.numeric(as.factor(gordon.labels)))
# pairs(gordon.chisq.select10[,1:5], col=as.numeric(as.factor(gordon.labels)))
# 
# 
# 
# gordon.AD <- gordon[which(gordon.labels=="AD"),]
# gordon.MPM <- gordon[which(gordon.labels=="MPM"),]
# 
# gordon.normalized <- rbind(t(scale(t(gordon.AD), center=FALSE)),t(scale(t(gordon.MPM), center=FALSE)))
# gordon.norm.cor <- cor(gordon.normalized, as.numeric(as.factor(gordon.labels)))
# gordon.norm.select10 <- gordon.normalized[, rev(order(abs(gordon.norm.cor)))[1:10]]
# plot(gordon.norm.select10[,1:2], col=as.numeric(as.factor(gordon.labels)))
# pairs(gordon.norm.select10[,1:5], col=as.numeric(as.factor(gordon.labels)))
# 
# 
# 
# 
# a.scaled<-scale(t(gordon.cor))
# a.scaled<-t(a.scaled)
# gordon.cor + attr(a.scaled, "scaled:center")
# #
# 
# 
# for(i in 1:nrow(gordon.chisq)){
#   gordon.chisq[1,]<-gordon.chisq[1,]/sd(gordon.chisq[1,])
# }
# gordon.cor = matrix(1:4, 2)
# 
# apply(x = gordon.cor, MARGIN = 1, FUN = shaz)



































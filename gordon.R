#### preprocessing ####
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
# the numeric values are strings and need to be parsed
gordon <- type.convert(x = gordon, dec = '.')


#### raw visualization ####
# Given that the variables describe gene expression, extracting features doesn't make much sense : we want to know which genes are most correlated to a given illness.
# Therefore, we will select the features most correlated to the labels

# However, let us plot the 2 first dimensions of the PCA just to get an idea
gordon.pca <- prcomp(gordon)
plot(gordon.pca$x[, 1:2], col=as.numeric(gordon.labels), xlab="Gordon PCA dimension 1", ylab="Gordon PCA dimension 2")
# The classes are mostly seperate. However, the separation is not linear in the two first dimensions of the PCA and there is some overlap between classes

#LDA
library(MASS)
# As we have the classes of the data points, we can try an LDA
gordon.lda <- lda(formula=gordon.labels~., data = as.data.frame(gordon))
plot(gordon.lda)

#MDS
gordon.mds <- isoMDS(dist(gordon), k=2, trace = TRUE)
plot(gordon.mds$points, col=as.numeric(as.factor(gordon.labels)), xlab="Gordon MDS dimension 1", ylab="Gordon MDS dimension 2")
# Same as PCA

# isomap
library(vegan)
gordon.isomap <- isomap(dist = dist(gordon), ndim = 2, k=10, fragmentedOK = TRUE)
plot(gordon.isomap, col=as.numeric(gordon.labels), xlab="Gordon ISOMAP dimension 1", ylab="Gordon ISOMAP dimension 2")


#### Variable selection ####
# computing correlation of all variables with class
gordon.cor <- cor(gordon, as.numeric(gordon.labels))
# ordering dimensions as a function of their correlation to the labels
gordon.ordered <- gordon[,rev(order(abs(gordon.cor)))]
# After some experimentation, about 75 variables seemed optimal
gordon.select <- gordon.ordered[, 1:75]

#### final visualization ####
# let us view the data by reducing the dimensions by MDS
#PCA
gordon.sel.pca <- prcomp(gordon.select)
plot(gordon.sel.pca$x[, 1:2], col=as.numeric(gordon.labels), xlab="Gordon post selection PCA dimension 1", ylab="Gordon post selection PCA dimension 2")
# the data is linearly separable.

#MDS
gordon.sel.mds <- isoMDS(dist(gordon.select), k=2, trace = TRUE)
plot(gordon.sel.mds$points, col=as.numeric(as.factor(gordon.labels)), xlab="Gordon post selection MDS dimension 1", ylab="Gordon post selection MDS dimension 2")
# same results as PCA

# As we have the classes of the data points, we can try an LDA
gordon.sel.lda <- lda(formula=gordon.labels~., data = as.data.frame(gordon.select))
plot(gordon.sel.lda)
# The LDA reduces the space to 1 dimension with linearly separable classes.

# isomap
gordon.isomap <- isomap(dist = dist(gordon.select), ndim = 2, k=10, fragmentedOK = TRUE)
plot(gordon.isomap, col=as.numeric(gordon.labels), xlab="Gordon post selection ISOMAP dimension 1", ylab="Gordon post selection ISOMAP dimension 2")
# the data is linearly separable, but no advantage compared to PCA

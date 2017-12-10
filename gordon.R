#load gordon
gordon <- read.csv("C:/Users/Syncrossus/Documents/Projet_Visualisation/gordon-2002_database.txt", header = TRUE, sep = "\t")
#getting an idea of what gordon is like
View(gordon[1:10, 1:10])
which(is.na(gordon))
dim(gordon)
levels(gordon[,2])

#strangely, observations seem to be columns and variables rows... also, the row labels are in the first column
gordon <- t(gordon)
gordon.labels <- gordon[2:nrow(gordon),1]
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



#PCA
gordon.pca <- prcomp(gordon)
plot(gordon.pca$sdev)
plot(gordon.pca$x[, 1:2], col=as.numeric(as.factor(gordon.labels)))

#LDA
library(MASS)
gordon.lda <- lda(formula=gordon.labels~., data = as.data.frame(gordon))
plot(gordon.lda, col = as.numeric(as.factor(gordon.labels)))
plot(x=gordon.lda$scaling, y = as.numeric(as.factor(gordon.labels)))
View(gordon.lda$scaling)

#SOM
library(kohonen)
gordon.som <- som(X = gordon, grid = somgrid(xdim = 12, ydim=12, topo="hexagonal"), rlen=200)
plot(gordon.som, type="changes")
plot(gordon.som, type="count")
plot(gordon.som, type="dist.neighbours")








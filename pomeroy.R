pomeroy <- read.csv("C:/Users/Syncrossus/Documents/GitHub/gordon-pomeroy-visualization/pomeroy-2002-v2_database.txt", header = TRUE, sep = "\t")
#getting an idea of what pomeroy is like
View(pomeroy[1:10, 1:10])
which(is.na(pomeroy))
dim(pomeroy)
levels(pomeroy[,2])

#strangely, observations seem to be columns and variables rows... also, the row labels are in the first column
pomeroy <- t(pomeroy)
pomeroy.labels <- pomeroy[2:nrow(pomeroy),1]
colnames(pomeroy)<- pomeroy[1,]
pomeroy <- pomeroy[2:nrow(pomeroy),2:ncol(pomeroy)]
View(pomeroy[1:10, 1:10])

#Trying to get a better understanding of pomeroy
summary(pomeroy)
boxplot(pomeroy[,sample(1:ncol(pomeroy), 5)])
pomeroy[,1]

# the numeric values seem to not be numeric
# I haven't found a smarter way to convert all cells to numeric without losing column names, row names, or 2D structure.
pomeroy <- type.convert(x = pomeroy, dec = '.')
View(pomeroy[1:10, 1:10])

#Trying again
summary(pomeroy)
boxplot(pomeroy[,sample(1:ncol(pomeroy), 5)])
pomeroy[,1]



pomeroy.pca <- prcomp(pomeroy)
plot(pomeroy.pca$sdev)
plot(pomeroy.pca$x[, 1:2], col=ifelse(pomeroy.labels=="MD", 1, 2))

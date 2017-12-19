#load pomeroy
pomeroy <- read.csv("pomeroy.csv",stringsAsFactors=FALSE, header = TRUE)
#overview
View(pomeroy[1:10, 1:10])
which(is.na(pomeroy))
dim(pomeroy)
levels(pomeroy[,2])

#preprocessing
##transpose the data and assign propoer column names
pomeroy=t(pomeroy)
colnames(pomeroy) = pomeroy[1,]
pomeroy <- pomeroy[-1,]
colnames(pomeroy)[1] <-"TumourType"
pomeroy=as.data.frame(pomeroy)
## convert the first column into character, and others into numeric
pomeroy[,1]<-as.character(pomeroy[,1])
n = ncol(pomeroy)
pomeroy[,2:n] <- lapply(pomeroy[,2:n], as.numeric)

#PCA
library(ggfortify)
pomeroy1 <- pomeroy[,2:n] #only numeric data
##2d plot
autoplot(prcomp(pomeroy[,2:n]), data = pomeroy, colour = 'TumourType')
##3d plot
library(rgl)
pomeroy.pca <- prcomp(pomeroy[,2:n])
plot3d(pomeroy.pca$x[,1:3],col= as.integer(as.factor(pomeroy$TumourType)) ,legend=TRUE, image=TRUE, size = 8,cex.legend = 1)
# legend3d("topright", legend = c('MD','Mglio','Ncer','PNET', 'Rhab'), pch = 16, col = as.integer(as.factor(pomeroy$TumourType)), cex=1, inset=c(0.02))

#LDA
library(MASS)
pomeroy.lda <- lda(formula= TumourType~., data = pomeroy)
plot(pomeroy.lda, col = as.numeric(as.factor(pomeroy$TumourType)))
View(pomeroy.lda$scaling)

#find correlated variables
library(corrplot)
corrplot(cor(pomeroy1), method = "color",tl.cex = 0.001,type="upper") # too much info, the pic is not clear
corrplot(cor(pomeroy[,2:100]),tl.cex = 0.001,type="upper")# see the first 100 variables correlation

library(Hmisc)
pval = rcorr(as.matrix(pomeroy[,2:n]), type = c("pearson","spearman")) #calc p-value of the correlations
sig = pval$r[which(pval$P<=0.05)] # find the significant correlations with signifiance level = 0.05
length(sig)
sigcor = sig[abs(sig)>0.7] #find the significantly strong correlations (absolute cor val > 0.7)
length(sigcor)


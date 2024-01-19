library(Hmisc)
library(psych)

df=read.table(file.choose(),header=TRUE, sep=";",dec=",")
FC=df
# Calculate fold change
for (i in 2:11){
  FC[,i]=df[,i]/df$Control
}
# Compute log2 of the FC of the drugs
log2_FC=cbind(df$cytokines,log2(FC[,3:11]))
colnames(log2_FC)[1]="cytokine"
par(mfrow = c(2, 2))
boxplot(cyclosporineA ~ cytokine,data=log2_FC,ylab="Log2 FC",main= 'Cyclosporine A',border = c(1:9))
boxplot(tacrolimus ~ cytokine,data=log2_FC,ylab="Log2 FC",main= 'Tacrolimus',border = c(1:9))
boxplot(tofacitinib ~ cytokine,data=log2_FC,ylab="Log2 FC",main= 'Tofacitinib',border = c(1:9))
boxplot(ruxolitinib ~ cytokine,data=log2_FC,ylab="Log2 FC",main= 'Ruxolitinib',border = c(1:9))



cor(log2_FC[,2:10], method ="pearson") #correlation coefficients
rcorr(as.matrix(log2_FC[,2:10]), type="pearson")$P*36    #multiple comparison using Bonferroni correction


#compute PCA
pca = prcomp(log2_FC[,2:10], scale = TRUE)
pca
summary(pca)
pca$sdev^2


#Extract the number of components
pr=principal(log2_FC[,2:10], nfactors = 2, rotate = "none")
pr

pca$x[,2]
plot(pca$x[,2],log2_FC$tacrolimus,xlab="PC2", ylab="Log2_FC Tacrolimus", main = 'Scatter plot of Loading score of PC2 vs Tacrolimus')
cor(pca$x[,2],log2_FC$tacrolimus, method ="pearson") #correlation between the variables



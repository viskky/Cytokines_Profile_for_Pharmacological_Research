cor_dist=1-cor(log2_FC[,2:10], method ="pearson")
clust=hclust(as.dist(cor_dist))
plot(clust,ylab="1-Pearson's correlation")


library(pheatmap)
pheatmap(as.matrix(log2_FC[2:10]),clustering_method="average")

h=dist(dh, method = "euclidean")
hp=hclust(h,method="average")
plot(hp,ylab="Euclidean distance")
abline(h=1,lty="dashed")

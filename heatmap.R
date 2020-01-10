install.packages("gplots")
install.packages("pheatmap")
library("gplots")
Onco <- read.csv("C:/Users/Dell/Desktop/Semester 8/STB/STB article work/Data of genes/DataExtractedOnco.csv")
View(Onco)
Onco <- Onco[order(Onco$Features),]
rnames <- Onco[,1]
rownames(Onco) = rnames
Onco[2:13,2:43] <- log(Onco[2:13,2:42], 2)
onc = data.matrix(Onco[2:13,2:42])
onc[is.nan(onc)] <- 0
onc[is.na(onc)] <- 0
onc <- replace(onc, onc == -Inf, 0)
onc
Oscilating <- read.csv("C:/Users/Dell/Desktop/Semester 8/STB/STB article work/Data of genes/DataExtractedOscilating.csv")
Oscilating <- Oscilating[order(Oscilating$Features),]
rnames <- Oscilating[,1]
rownames(Oscilating) = rnames
rnames
Oscilating[2:13,2:12] <- log(Oscilating[2:13,2:12],2)
x = data.matrix(Oscilating[2:13,3:12])
x[is.nan(x)] <- 0
x[is.na(x)] <- 0
x <- replace(x, x == -Inf, 0)
x
arsh = merge(onc,x,by=0)
View(arsh)
arsh <- arsh[order(arsh$Row.names),]
rnames <- arsh[,1]
rownames(arsh) = rnames
rnames
f=data.matrix(arsh[2:11,2:53])
View(f)
library(pheatmap)
callback = function(hc, mat){
  sv = svd(t(mat))$v[,1]
  dend = reorder(as.dendrogram(hc), wts = sv)
  as.hclust(dend)
}
pheatmap(f,scale="column",clustering_callback = callback,  margins=c(2,5), color = colorRampPalette(c("navy", "white", "red"))(100))
pheatmap(f,  main = "Cluster analysis of Oncogenes- Oscillating genes using R statistical software
based on their genotranscriptomic data drawn from COSMIC database v86",clustering_distance_rows = "euclidean",clustering_distance_cols = "euclidean", clustering_method = "complete",
         scale="column", margins=c(5,10), color = colorRampPalette(c("navy", "white", "firebrick3"))(100), cellheight = 12, fontsize = 8)

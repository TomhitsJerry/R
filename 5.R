rm(list=ls())
library(ggplot2)
library(reshape2)
setwd("C:\\Users\\wubin\\Desktop\\train")
read.table(file="5_Community_Heatmap\\genus.taxon.Abundance.xls",
           header = T,
           fill=T,
           check.names = F,
           stringsAsFactors=F,
           row.names = 1)->p
p=p[-nrow(p),]
p=p[order(p$Abundance,decreasing = T),]
index=which(colnames(p)=="SY3_4")
n=16#取最大的16个
p=p[1:n,1:index]
title=rownames(p)
relative_abundance=sapply(1:ncol(p), function(t){p[,t]/sum(p[,t])*100})
rownames(relative_abundance)=title
colnames(relative_abundance)=colnames(p)

layout(matrix(c(0, 1, 2, 3, 0, 4), nrow = 3, byrow = TRUE),width=c(1.2,3),height=c(1.5,4,1))
layout.show(4)

#heatmap
hc=hclust(dist(t(p),method = "euclidean"),method = "complete")#列聚类#上        variable 20
col_order=hc$order
hc=as.dendrogram(hc)
par(mar=c(0,0,1,7.5))
plot(hc,xpd=T,leaflab = "none",axes=F,   xaxs = "i",)

hc=hclust( dist(p,method = "euclidean"),method="complete")#行聚类 左   id 16
row_order=hc$order
hc=as.dendrogram(hc)
par(mar=c(1,1,0,0))
plot(hc,leaflab = "none",horiz = T,axes = F,xaxs="i",yaxs="i")#左,hang=-1对齐

p=p[row_order,col_order]#16,20
color <- colorRampPalette(c("darkblue","darkgreen","yellow","darkred"))(100)
par(mar=c(1,0,0,7.5))
image(x = 1:ncol(p), #20
      y = 1:nrow(p), #16
      z = data.matrix(t(p)),#按照列填充,所以要转置 
      xlab="",
      ylab="",
      axes = FALSE,
      col = color,
      yaxs="i")
axis(side = 1,at = 1:ncol(p),  labels = colnames(p), tick = FALSE,las = 2 )#下面的variable name
axis(side = 4, at = 1:nrow(p), labels = rownames(p), las = 1, tick = FALSE)#右边的id name
#p$id=rownames(p)
#dp=melt(p,id.vars = "id")
#ggp<-ggplot(dp,aes(variable,id))+geom_tile(aes(fill=value))+theme(axis.text.x=element_text(angle = 90),legend.position="bottom")+scale_fill_gradient(low = "blue", high = "red")#底坐标旋转90,渐变颜色
#ggp
#legend调整大小
min_data <- min(p)
max_data <- max(p)
par(mar=c(2,0,4,7.5))
data_legend <- matrix(seq(min_data, max_data, 10), ncol = 1)
image(data_legend, axes = FALSE, col = color, xlab = "Relative abundance of community(%)")
axis(side = 1 , at = seq(0, 1, length.out = 5), labels = seq(min_data, max_data, length.out = 5))#分5段




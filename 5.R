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
index=which(colnames(p)=="SY3_4")
p=p[-nrow(p),1:index]
title=rownames(p)

relative_abundance=sapply(1:ncol(p), function(t){p[,t]/sum(p[,t])*100})
rownames(relative_abundance)=title
colnames(relative_abundance)=colnames(p)

layout(matrix(c(1:4),nr=2),widths = c(2,9),heights = c(2,9))
layout.show(4)
#heatmap
hc=hclust( dist(p))#行聚类
row_order=hc$order
plot.new()
par()
plot(hc,sub = "",main="",xlab="",ylab = "",hang = -1,xpd=T)#左,hang=-1对齐
hc=hclust(dist(t(p)))#列聚类
col_order=hc$order
p=p[row_order,col_order]#
plot(hc,sub = "",main="",xlab="",ylab = "",xpd=T)#上
p$id=rownames(p)
dp=melt(p,id.vars = "id")
ggp<-ggplot(dp,aes(variable,id))+geom_tile(aes(fill=value))+theme(axis.text.x=element_text(angle = 90),legend.position="bottom")+scale_fill_gradient(low = "blue", high = "red")#底坐标旋转90,渐变颜色
ggp
#legend调整大小



rm(list=ls())
setwd("C:\\Users\\wubin\\Desktop\\train\\")
read.table(file="6_Community_Treebar\\genus.taxon.Abundance.xls",
           header=T,
           row.names = 1,
           fill=T,
           sep="\t",
           check.names = F,
           stringsAsFactors=F)->p
n=16#只显示16行
index=which(colnames(p)=="SY3_4")
p[-nrow(p),1:index]->p
rownames(p)->title

relative_abundance = sapply(1:ncol(p),function(t) {p[,t]/sum(p[,t])})#lapply返回list sapply返回
data.frame(relative_abundance,row.names = title)->relative_abundance
t = sapply(1:nrow(relative_abundance),function(t) {sum(relative_abundance[t,])})#lapply返回list sapply返回
data.frame(t,row.names = title)->t
rownames(t)[order(t[,1],decreasing=T)][1:n]->title#相对丰度由大到小的16个
p[title,]->p#
relative_abundance[title,]->relative_abundance
colnames(relative_abundance) = colnames(p)
rownames(relative_abundance) = rownames(p)
#colordataframe=data.frame(color=rainbow(length(title)),row.names = title)#
as.matrix(relative_abundance)->dat#matrix
color=rainbow(length(title))


#row<-nrow(dat)
#c<-ncol(dat)
#colmatrix<-matrix(0,nrow=row,ncol=c)
#for (i in 1:row){
#  for (j in 1:c){
#    colmatrix[i,j]<-colordataframe[rownames(relative_abundance)[i],]
#  }
#}
layout(matrix(c(1,2,3),nr = 1),widths = c(1,4,2))
#layout.show(3)
hc<-hclust(dist(dat),method="average") #对行进行聚类
rowInd<-hc$order #将聚类后行的顺序存为rowInd
hc<-hclust(dist(t(dat)),method="average")  #对矩阵进行转置，对原本的列进行聚类
colInd<-hc$order  #将聚类后列的顺序存为colInd
dat<-dat[rowInd,colInd] #将数据按照聚类结果重排行和列
#dp=melt(data)  #对数据进行融合，适应ggplot的数据结构，以进行热图的绘制
hc=as.dendrogram(hc)
par(mar = c(1, 1, 1, 0))
plot(hc,leaflab = "none",horiz = T,axes = F,yaxs = "i")##################
#axis(side=4)
par(mar = c(1, 6.5, 1, 0))
barplot(dat,col=color,horiz = T,ylab=NULL,xlab=NULL,names.arg = NULL,border=NA, xaxt = "n",yaxs = "i")#xaxt下面的axis无
#axis(side=2)
plot.new()
par(mar = c(1,0,1,1), xpd = T,adj=0)#adj0左对齐

legend("left", "center",  legend=title,fill=color,bty="n",ncol=1)#inset 设置距离margin的距离 ncol分列 insert=0.05

##  1
read.table(file="C:\\Users\\wubin\\Desktop\\train\\1_QC_seq-distribut\\reads.clean.length.distribution.xls",header=FALSE,sep = "\t")->da
barplot(da[,2])
##
##  2
##xls读入报错，说line结尾不完整。因此另存为csv
read.csv(file="C:\\Users\\wubin\\Desktop\\train\\2_CommunityStructure_Barplot\\genus.taxon.Abundance.csv",header=TRUE)->da
da[2:68,]->dat
da[1,]->No_Rank 
da[69,]->all
dat['Kb0']->height
as.matrix(height,ncol=1)->height
barplot(height,beside = TRUE,space=1)
##  3
read.table(file="C:\\Users\\wubin\\Desktop\\train\\3_Community_Pieplot\\genus.taxon.Abundance.csv",header=TRUE,sep=',')->da
da["Kb0"]->dat


rm(list=ls())
setwd("C:\\Users\\wubin\\Desktop\\train\\")
read.table("4_Community_Barplot\\genus.taxon.Abundance.xls",
           header=T,
           row.names = 1, 
           sep = "\t",
           check.names = FALSE,
           fill = T
)->p
which(colnames(p)=="SY3_4")->index
p[-nrow(p),1:index]->x
relative_abundance = sapply(1:ncol(plotdata),function(x) {plotdata[,x]/sum(plotdata[,x])})
as.matrix(x)->dat#matrix

unique(rownames(x))->c
col<-data.frame(col=rainbow(length(c)),row.names = c)
row<-nrow(dat)
c<-ncol(dat)

colmatrix<-matrix(0,nrow=row,ncol=c)
for (i in 1:row){
  for (j in 1:c){
    colmatrix[i,j]<-col[rownames(dat)[i],]
  }
}
layout(matrix(c(1, 2 ), 2, 1, byrow = TRUE),heights = c(2,1))#widths指定横向比例，heights指定纵向比例
layout.show(2)#2行2列的矩形 
barplot(dat,col=colmatrix,ylab = "Realitive abundance(%)",xlab="",space=0.5,border=NA,)->b
legend("", legend = rownames(dat), fill = colmatrix[,1], bty = "n")#legend定义内容,fill定义box被填充的颜色,bty定义框住legend的有无



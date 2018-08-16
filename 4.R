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
relative_abundance = sapply(1:ncol(x),function(t) {x[,t]/sum(x[,t])})
colnames(relative_abundance) = colnames(x)
rownames(relative_abundance) = rownames(x)
as.matrix(relative_abundance)->dat#matrix

unique(rownames(dat))->c
###############################
#col<-data.frame(col=rainbow(length(c)),row.names = c)
#row<-nrow(dat)
#c<-ncol(dat)

#colmatrix<-matrix(0,nrow=row,ncol=c)
#for (i in 1:row){
#  for (j in 1:c){
#    colmatrix[i,j]<-col[rownames(dat)[i],]
#  }
#}
#################################
col<-rep(c(46 ,58, 59 ,60 ,61, 62, 63 ,64 ,65, 66, 67 ,68, 45, 42 ,41 ,38 ,37, 35, 33 ,30, 29 ,26 ,25 ,12 ,13 ,14 ,15 ,16, 17 ,18, 19 ,20, 21 ,22,23,11,10  ,9 , 8  ,7 , 6  ,5 , 4  ,3 , 2,  1 ,24 ,27 ,28 ,31, 32 ,34 ,36 ,39, 40, 43, 44, 57 ,56 ,55, 54 ,53 ,52 ,51 ,50 ,49 ,48 ,47),1)
#########################
#col<-rainbow(length(c))
############

#layout(matrix(c(1, 2 ), 2, 1, byrow = TRUE),heights = c(2,1))#widths指定横向比例，heights指定纵向比例

#barplot(dat,col=colmatrix,ylab = "Realitive abundance(%)",xlab="",space=0.5,border=NA,)->b
#legend("", legend = rownames(dat), fill = colmatrix[,1], bty = "n")#legend定义内容,fill定义box被填充的颜色,bty定义框住legend的有无

layout(matrix(c(1,1,2,1,1,2),nr = 3))
par(mar = c(5, 5, 2.5, 2), mgp = c(3,0.6,0), xpd=T)
p = barplot(dat*100,
            beside = F,
            col = col,
            ylab = "Relative abundance (%)",
            ylim = c(0,100),
            space = 0.6,
            cex.names =1,    #字体大小 
            border = NA,      
            las = 2,                
            axes = T, 
            cex.lab = 1.3, #y lab的字体大小
            cex.axis = 1, #坐标轴字大小
            xaxs = "i")
par(mar = c(1,5,1,0), xpd = T)
plot.new()#
legend("center", 
       legend = rownames(relative_abundance), 
       fill = col, 
       ncol = 3, 
       bty = "n", 
       cex = 0.6)

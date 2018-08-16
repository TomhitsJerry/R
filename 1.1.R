##1_QC_seq-distribution
read.table(file="C:\\Users\\wubin\\Desktop\\train\\1_QC_seq-distribut\\reads.clean.length.distribution.xls",header=FALSE,sep = "\t")->input
max(input[,2])->M#找到value值最大一项
input[,2]->data
which(data==M)->index
i=10#读取前后10项
start<-index-i
end<-index+i
input[start:end,]->data
barplot(data[,2],col="blue",xlab="Length",ylab = "Number",border = NA,main="Sequence length distribution")->p
text(x = b[, 1], y = par("usr")[3], labels = data$V1, pos=1, srt = 45, xpd = TRUE)#srt label旋转角度,pos below，

rm(list=ls())
setwd("C:\\Users\\wubin\\Desktop\\train\\")
read.table("3_Community_Pieplot\\genus.taxon.Abundance.xls",
           header = T,
           row.names = 1, 
           sep = "\t",
           check.names = FALSE,
           fill = T
           )->p
numb=14#画最大的14项
rainbow(numb)->color
p[!rownames(p)%in% c("All"),]->x
which(colnames(x)=="SY3_4")->index

lapply(1:index, function(t){
 
  sum(x[[t]])->total
  x[order(x[[t]],decreasing = T),][1:numb,]->order_x
  rownames(order_x)->name
  order_x[[t]]->dat
  round(order_x[[t]]*100/total,2) ->perc
  paste(name,"(",perc,")%",sep="")->piename
  pie(dat,labels = piename,col = color)
  savePlot(filename = paste(colnames(x)[t],"genus.pieplot.pdf",sep=""),
           type = "pdf"
           )
})





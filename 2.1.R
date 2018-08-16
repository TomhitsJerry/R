rm(list=ls())
setwd("C:\\Users\\wubin\\Desktop\\2_CommunityStructure_Barplot")
input <- "genus.taxon.Abundance.xls"

x <- read.table(input, header = TRUE,
                row.names = 1,#第一列设为行名
                sep = "\t", 
                check.names = FALSE,#检查数据框里的variablesname的有效性,唯一
                comment.char = "",#设置注释的标识,若确认无注释,“”提高速度
                quote = "'\"",#指定字符串分隔符
                stringsAsFactors = FALSE,
                fill = T)
x <- x[-nrow(x), ]#把最后一行去掉
x <- x[!rownames(x) %in% c("No_Rank", "Unassigned"), ]#把NO_Rank和Unassigned两行去掉

x$rel <- x$Abundance / sum(x$Abundance) * 100#添加一列rel相对丰度



data <- x[order(x$Abundance, decreasing = TRUE), ][1:30, ]#从大到小排序,然后取出1到30行


first <- which(colnames(data) == "Abundance")+1#数据对应index
last  <- ncol(data) - 1

mycol <- c(119,132,147,454,89,404,123,529,463,461,128,139,552,28,54,84,100,258,558,376,43,652,165,31,610,477,256,588,99,632,81,503,104,562,76,96,495,598,645,507,657,33,179,107,62)
mycol = colors()[rep(mycol,20)]#将mycol重复20次，对应取出colors()颜色

lapply(first:last, function(t){#输出为List,循环一个个输入
  level   <- colnames(data)[t]
  catalog <- unique(data[[t]]) #对某一列t的值unique
  col_map <- data.frame( catalog = catalog, colors = mycol[1:length(catalog)])#设置列为catalog和colors
  
  
  colors <- unlist(  lapply(data[[t]], function(i){ col_map$colors[col_map$catalog == i]   })  )
  
  b <- barplot(data$rel, ylab = "Relative abundance(%)", xlab = "", col = colors, border = NA)
  text( x = b[,1], y = par("usr")[3], labels = rownames(data), adj = 1, srt = 45, xpd =TRUE)#xpd定义边界位置,图例绘在制图区外，必须设置参数xpd=TRUE，否则命令正确也不会出图，因为默认xpd=F
  legend("topright", legend = catalog, fill = col_map$colors, bty = "n")#legend定义内容,fill定义box被填充的颜色,bty定义框住legend的有无
  
})

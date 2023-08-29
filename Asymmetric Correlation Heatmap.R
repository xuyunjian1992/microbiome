library(psych)
library(reshape2)
library(pheatmap)
phy <- read.table(file = "phy.xls", sep = "\t", header = T, row.names=1)  #读取微生物丰度信息表
met <- read.table(file = "met.xls", sep = "\t", header = T, row.names=1)  #读取代谢物信息表
cor <- corr.test(phy, met, method = "pearson",adjust="none")              #计算相关性矩阵、p值矩阵
cmt <- cor$r
pmt <- cor$p
cmt.out<-cbind(rownames(cmt),cmt)
pmt.out<-cbind(rownames(pmt),pmt)
write.table(cmt.out,file="cor.txt",sep="\t",row.names=F)       #输出相关系数表格
write.table(pmt.out,file="pvalue.txt",sep="\t",row.names=F)    #输出p值表格
df <- melt(cmt,value.name="cor")
df$pvalue <- as.vector(pmt)
head(df)
write.table(df,file="cor-p.txt",sep="\t")          #输出：相关系数、p值表格

 if (!is.null(pmt)) {
ssmt <- pmt < 0.01
pmt[ssmt] <- '**'
smt <- pmt > 0.01& pmt <0.05
pmt[smt] <- '*'
pmt[!ssmt&!smt] <- ''
} else {
  pmt <- F
 }

pmt
pheatmap(cmt, scale = "none", cluster_row = F, cluster_col = F,                
         display_numbers = pmt, fontsize_number = 12, number_color = "white",
         cellwidth = 20, cellheight = 20,filename="heatmap.pdf")               #输出：相关性热图pdf文件
pheatmap(cmt, scale = "none",cluster_row = F, cluster_col = F,
	   display_numbers = pmt, fontsize_number = 12, number_color = "white",
         cellwidth = 20, cellheight = 20)  



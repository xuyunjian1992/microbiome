library(psych)
library(reshape2)
library(pheatmap)
phy <- read.table(file = "phy.xls", sep = "\t", header = T, row.names=1)  #��ȡ΢��������Ϣ��
met <- read.table(file = "met.xls", sep = "\t", header = T, row.names=1)  #��ȡ��л����Ϣ��
cor <- corr.test(phy, met, method = "pearson",adjust="none")              #��������Ծ���pֵ����
cmt <- cor$r
pmt <- cor$p
cmt.out<-cbind(rownames(cmt),cmt)
pmt.out<-cbind(rownames(pmt),pmt)
write.table(cmt.out,file="cor.txt",sep="\t",row.names=F)       #������ϵ������
write.table(pmt.out,file="pvalue.txt",sep="\t",row.names=F)    #���pֵ����
df <- melt(cmt,value.name="cor")
df$pvalue <- as.vector(pmt)
head(df)
write.table(df,file="cor-p.txt",sep="\t")          #��������ϵ����pֵ����

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
         cellwidth = 20, cellheight = 20,filename="heatmap.pdf")               #������������ͼpdf�ļ�
pheatmap(cmt, scale = "none",cluster_row = F, cluster_col = F,
	   display_numbers = pmt, fontsize_number = 12, number_color = "white",
         cellwidth = 20, cellheight = 20)  


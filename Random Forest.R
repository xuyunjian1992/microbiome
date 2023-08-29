
rm(list = ls())
setwd("F:/~/")#�����ļ��洢λ��
library(openxlsx)
library(dplyr)
pro <- read.xlsx("protein.xlsx",rowNames = TRUE)
#����
pro=pro[which(apply(pro,1,function(x){return(sum(x>0))})>ncol(pro)*0.2),]
pro[is.na(pro)] <- 0
pro_norm <- scale(pro, center=T,scale=T) #���ݱ�׼��
pro_norm <- as.data.frame(pro_norm)
#�������
group <- read.csv("Group.csv")
#ɸѡML��������
group_ml <- filter(group,group == "1" | group == "2")
ml_select <- group_ml$Sample
ml_select
ml_select <- as.character(ml_select)
class(ml_select)
pro_ml <- pro_norm[,ml_select]

#��ת�ñ������
pro_ml <- data.frame(t(pro_ml))

#�ϲ����鼰��������
ml_all <- cbind(pro_ml,group_ml)
ml_all <- ml_all[,-which(colnames(ml_all) %in% c("Sample"))]
ml_all$group <- factor(ml_all$group,levels = c("1", "2"))
#�������ݼ���Ϊѵ������ռ 75%���Ͳ��Լ���ռ 25%��
set.seed(1234)
select_train <- sample(69, 69*0.75)
ml_train <- ml_all[select_train, ]
ml_test <- ml_all[-select_train, ]
#---���ɭ�ֹ���---
#randomForest �������ɭ��
library(randomForest)
#BiocManager::install("ROCR")
library(pROC)
library(ROCR)
#���ɭ�ּ��㣨���� 1000 �þ�������
set.seed(1234)
ml_train.forest <- randomForest(group ~ ., data = ml_train, importance = TRUE,proximity=TRUE,)
ml_train.forest
#ѵ������������
train_predict <- predict(ml_train.forest, ml_train)
compare_train <- table(train_predict, ml_train$group)
compare_train
sum(diag(compare_train)/sum(compare_train))

train_predict <- predict(ml_train.forest, ml_train, type = 'prob')
predicts <- t(apply(train_predict,1,function(v){v/sum(v)}))
colnames(predicts) <- colnames(train_predict)
predicts <- data.frame(predicts,check.names=F)
predicts$predicted <- apply(predicts,1,function(v){names(v)[max(v)==v]})
predicts$observed <- ml_train$group
sum(predicts$predicted==predicts$observed)
#############################
library(ggrepel)
da.va <- data.frame(t(ml_train))
df2 <- da.va[,row.names(predicts)]
df2 <- df2[-382,]
df2=apply(df2,2,as.numeric)
mean <- apply(df2,2,mean,na.rm=T)
mean
data <- data.frame(mean,predicts=predicts$`1`,sample2=row.names(predicts),type2=predicts$observed )
a <- ggplot(data,aes(x=predicts,y=mean,group=type2,color=type2))+ 
    geom_point(size = 4)+
    geom_vline(xintercept = 0.5 ,linetype="dotted")+
    xlab("predict value")+
    ylab("Average moleculer intersity")+
    ggtitle("ALL_train_pointplot")+
    xlim(0,1)+
    theme(legend.text = element_text(size = 15,color = "black"),legend.position = 'right',
          legend.title = element_blank() ,
          panel.grid.major =element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))+
    theme(panel.grid =element_blank())+
    theme(axis.text = element_text(size = 10,color = "black"))+
    theme(axis.text.x = element_text( hjust = 1,angle = 45))+
    theme(plot.subtitle=element_text(size=30, hjust=0, color="black"))+
    theme(axis.title.x=element_text(size=17, hjust=0.5, color="black"))+
    theme(axis.title.y=element_text(size=17, hjust=0.5, color="black"))+  
    scale_color_manual(limits=c("1","2"), values=c("#E29827","#922927"))+
    geom_text_repel(aes(label=sample2,vjust = -0.8, hjust = 0.5),color = "black",show.legend = FALSE)
a






#����ROC����
ROC <- roc(predicts$observed, as.numeric(predicts$`1`))
plot(ROC, print.auc=TRUE)

#ʹ�ò��Լ�����
test_predict <- predict(ml_train.forest, ml_test)
compare_test <- table(ml_test$group, test_predict, dnn = c('Actual', 'Predicted'))
compare_test
sum(diag(compare_test)/sum(compare_test))

test_predict <- predict(ml_train.forest, ml_test, type = 'prob')
predicts <- t(apply(test_predict,1,function(v){v/sum(v)}))
colnames(predicts) <- colnames(test_predict)
predicts <- data.frame(predicts,check.names=F)
predicts$predicted <- apply(predicts,1,function(v){names(v)[max(v)==v]})
predicts$observed <- ml_test$group
#����ROC����
ROC <- roc(predicts$observed, as.numeric(predicts$`1`))
plot(ROC, print.auc=TRUE)
sum(predicts$predicted==predicts$observed)
#############################
da.va <- data.frame(t(ml_test))
df2 <- da.va[,row.names(predicts)]
df2 <- df2[-382,]
df2=apply(df2,2,as.numeric)
mean <- apply(df2,2,mean,na.rm=T)
mean
data <- data.frame(mean,predicts=predicts$`1`,sample2=row.names(predicts),type2=predicts$observed )
a <- ggplot(data,aes(x=predicts,y=mean,group=type2,color=type2))+ 
  geom_point(size = 4)+
  geom_vline(xintercept = 0.5 ,linetype="dotted")+
  xlab("predict value")+
  ylab("Average molecular intersity")+
  ggtitle("ALL_test_pointplot")+
  xlim(0,1)+
  theme(legend.text = element_text(size = 15,color = "black"),legend.position = 'right',
        legend.title = element_blank() ,
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(panel.grid =element_blank())+
  theme(axis.text = element_text(size = 10,color = "black"))+
  theme(axis.text.x = element_text( hjust = 1,angle = 45))+
  theme(plot.subtitle=element_text(size=30, hjust=0, color="black"))+
  theme(axis.title.x=element_text(size=17, hjust=0.5, color="black"))+
  theme(axis.title.y=element_text(size=17, hjust=0.5, color="black"))+  
  scale_color_manual(limits=c("1","2"), values=c("#E29827","#922927"))+
  geom_text_repel(aes(label=sample2,vjust = -0.8, hjust = 0.5),color = "black",show.legend = FALSE)
a

#����ROC����
ran_roc <- roc(ml_test$group,as.numeric(test_predict))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='���ɭ��ģ��ROC����,ntree=1000')


##������֤����ѡ���ض������� OTUs
#10���ظ�ʮ�۽�����֤
set.seed(123)
ml_train.cv <- replicate(10, rfcv(ml_train[-ncol(ml_train)], ml_train$group, cv.fold = 10), simplify = FALSE)
ml_train.cv

#��ȡ��֤�����ͼ
ml_train.cv <- data.frame(sapply(ml_train.cv, '[[', 'error.cv'))
ml_train.cv$ID <- rownames(ml_train.cv)
ml_train.cv <- reshape2::melt(ml_train.cv, id = 'ID')
ml_train.cv$ID <- as.numeric(as.character(ml_train.cv$ID))

#�����ͼ
library(ggplot2)
library(splines)  #������ geom_smooth() ����������ߣ�����ʹ�� geom_line() ��� geom_smooth() ������ͨ����

p <- ggplot(ml_train.cv, aes(ID, value)) +
  geom_smooth(se = FALSE,  method = 'glm', formula = y~ns(x, 6)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
  labs(title = '',x = 'Number of IDs', y = 'Cross-validation error')

p
#��ȡǰ 11 ����Ҫ�ĵ���
p + geom_vline(xintercept = 11)

#��ͼչʾ ǰ 11 ����Ҫ�� ����
varImpPlot(ml_train.forest,sort=TRUE, n.var = min(11, nrow(ml_train.forest$importance)), main = 'Top 8 - variable importance')
result <- data.frame(importance(ml_train.forest,type=1))
result$ID <- row.names(result)
result <- data.frame( result[order(result$MeanDecreaseAccuracy,decreasing = T),])
write.table(result, 'importance_ID.txt', sep = '\t', col.names = NA, quote = FALSE)
##��Լ������

#ѡ�� top11 ��Ҫ�� OTUs�����������Ѿ����ݡ�Mean Decrease Accuracy���������
ID_select <- result$ID [1:11]

#�����Ӽ���ѵ�����Ͳ��Լ�
ml_train_top11 <- ml_train[ ,c(ID_select, 'group')]
ml_test_top11 <- ml_test[ ,c(ID_select, 'group')]

#���ɭ�ֹ���
set.seed(1234)
ml_train.forest_11 <- randomForest(group ~ ., data = ml_train_top11, importance = TRUE)
ml_train.forest_11

#ѵ������������

train_predict_11 <- predict(ml_train.forest_11, ml_train_top11)
compare_train <- table(train_predict_11, ml_train_top11$group)
compare_train
sum(diag(compare_train)/sum(compare_train))


train_predict_11 <- predict(ml_train.forest_11, ml_train_top11, type = 'prob')
predicts_11 <- t(apply(train_predict_11,1,function(v){v/sum(v)}))
colnames(predicts_11) <- colnames(train_predict_11)
predicts_11 <- data.frame(predicts_11,check.names=F)
predicts_11$predicted <- apply(predicts_11,1,function(v){names(v)[max(v)==v]})
predicts_11$observed <- ml_train_top11$group
ROC <- roc(predicts_11$observed, as.numeric(predicts_11$`1`))
sum(predicts_11$predicted==predicts_11$observed)
#############################
library(ggplot2)
da.va <- data.frame(t(ml_train_top11))
df2 <- da.va[,row.names(predicts_11)]
df2 <- df2[-12,]
df2=apply(df2,2,as.numeric)
mean <- apply(df2,2,mean,na.rm=T)
mean
data <- data.frame(mean,predicts=predicts_11$`1`,sample2=row.names(predicts_11),type2=predicts_11$observed )
a <- ggplot(data,aes(x=predicts,y=mean,group=type2,color=type2))+ 
  geom_point(size = 4)+
  geom_vline(xintercept = 0.5 ,linetype="dotted")+
  xlab("predict value")+
  ylab("Average moleculer intersity")+
  ggtitle("Top8_train_pointplot")+
  xlim(0,1)+
  theme(legend.text = element_text(size = 15,color = "black"),legend.position = 'right',
        legend.title = element_blank() ,
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(panel.grid =element_blank())+
  theme(axis.text = element_text(size = 10,color = "black"))+
  theme(axis.text.x = element_text( hjust = 1,angle = 45))+
  theme(plot.subtitle=element_text(size=30, hjust=0, color="black"))+
  theme(axis.title.x=element_text(size=17, hjust=0.5, color="black"))+
  theme(axis.title.y=element_text(size=17, hjust=0.5, color="black"))+  
  scale_color_manual(limits=c("1","2"), values=c("#E29827","#922927"))+
  geom_text_repel(aes(label=sample2,vjust = -0.8, hjust = 0.5),color = "black",show.legend = FALSE)
a
#ggsave(paste0(title,"_pointplot.pdf"),plot=a,width=4,height=8)



#����ROC����
plot(ROC, print.auc=TRUE)

#ʹ�ò��Լ�����
test_predict_11 <- predict(ml_train.forest_11, ml_test_top11)
compare_test <- table(test_predict_11, ml_test_top11$group)
compare_test
sum(diag(compare_test)/sum(compare_test))


test_predict_11 <- predict(ml_train.forest_11, ml_test_top11, type = 'prob')
predicts_11 <- t(apply(test_predict_11,1,function(v){v/sum(v)}))
colnames(predicts_11) <- colnames(test_predict_11)
predicts_11 <- data.frame(predicts_11,check.names=F)
predicts_11$predicted <- apply(predicts_11,1,function(v){names(v)[max(v)==v]})
predicts_11$observed <- ml_test_top11$group
ROC <- roc(predicts_11$observed, as.numeric(predicts_11$`1`))
sum(predicts_11$predicted==predicts_11$observed)
#############################
library(ggplot2)
da.va <- data.frame(t(ml_test_top11))
df2 <- da.va[,row.names(predicts_11)]
df2 <- df2[-12,]
df2=apply(df2,2,as.numeric)
mean <- apply(df2,2,mean,na.rm=T)
mean
data <- data.frame(mean,predicts=predicts_11$`1`,sample2=row.names(predicts_11),type2=predicts_11$observed )
a <- ggplot(data,aes(x=predicts,y=mean,group=type2,color=type2))+ 
  geom_point(size = 4)+
  geom_vline(xintercept = 0.5 ,linetype="dotted")+
  xlab("predict value")+
  ylab("Average moleculer intersity")+
  ggtitle("Top11_train_pointplot")+
  xlim(0,1)+
  theme(legend.text = element_text(size = 15,color = "black"),legend.position = 'right',
        legend.title = element_blank() ,
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(panel.grid =element_blank())+
  theme(axis.text = element_text(size = 10,color = "black"))+
  theme(axis.text.x = element_text( hjust = 1,angle = 45))+
  theme(plot.subtitle=element_text(size=30, hjust=0, color="black"))+
  theme(axis.title.x=element_text(size=17, hjust=0.5, color="black"))+
  theme(axis.title.y=element_text(size=17, hjust=0.5, color="black"))+  
  scale_color_manual(limits=c("1","2"), values=c("#E29827","#922927"))+
  geom_text_repel(aes(label=sample2,vjust = -0.8, hjust = 0.5),color = "black",show.legend = FALSE)
a
#ggsave(paste0(title,"_pointplot.pdf"),plot=a,width=4,height=8)




#����ROC����
plot(ROC, print.auc=TRUE)
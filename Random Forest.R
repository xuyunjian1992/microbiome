
rm(list = ls())
setwd("F:/~/")#运行文件存储位置
library(openxlsx)
library(dplyr)
pro <- read.xlsx("protein.xlsx",rowNames = TRUE)
#过滤
pro=pro[which(apply(pro,1,function(x){return(sum(x>0))})>ncol(pro)*0.2),]
pro[is.na(pro)] <- 0
pro_norm <- scale(pro, center=T,scale=T) #数据标准化
pro_norm <- as.data.frame(pro_norm)
#导入分组
group <- read.csv("Group.csv")
#筛选ML所需样本
group_ml <- filter(group,group == "1" | group == "2")
ml_select <- group_ml$Sample
ml_select
ml_select <- as.character(ml_select)
class(ml_select)
pro_ml <- pro_norm[,ml_select]

#先转置表达矩阵
pro_ml <- data.frame(t(pro_ml))

#合并分组及表达数据
ml_all <- cbind(pro_ml,group_ml)
ml_all <- ml_all[,-which(colnames(ml_all) %in% c("Sample"))]
ml_all$group <- factor(ml_all$group,levels = c("1", "2"))
#将总数据集分为训练集（占 75%）和测试集（占 25%）
set.seed(1234)
select_train <- sample(69, 69*0.75)
ml_train <- ml_all[select_train, ]
ml_test <- ml_all[-select_train, ]
#---随机森林构建---
#randomForest 包的随机森林
library(randomForest)
#BiocManager::install("ROCR")
library(pROC)
library(ROCR)
#随机森林计算（生成 1000 棵决策树）
set.seed(1234)
ml_train.forest <- randomForest(group ~ ., data = ml_train, importance = TRUE,proximity=TRUE,)
ml_train.forest
#训练集自身测试
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






#绘制ROC曲线
ROC <- roc(predicts$observed, as.numeric(predicts$`1`))
plot(ROC, print.auc=TRUE)

#使用测试集评估
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
#绘制ROC曲线
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

#绘制ROC曲线
ran_roc <- roc(ml_test$group,as.numeric(test_predict))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='随机森林模型ROC曲线,ntree=1000')


##交叉验证帮助选择特定数量的 OTUs
#10次重复十折交叉验证
set.seed(123)
ml_train.cv <- replicate(10, rfcv(ml_train[-ncol(ml_train)], ml_train$group, cv.fold = 10), simplify = FALSE)
ml_train.cv

#提取验证结果绘图
ml_train.cv <- data.frame(sapply(ml_train.cv, '[[', 'error.cv'))
ml_train.cv$ID <- rownames(ml_train.cv)
ml_train.cv <- reshape2::melt(ml_train.cv, id = 'ID')
ml_train.cv$ID <- as.numeric(as.character(ml_train.cv$ID))

#拟合线图
library(ggplot2)
library(splines)  #用于在 geom_smooth() 中添加拟合线，或者使用 geom_line() 替代 geom_smooth() 绘制普通折线

p <- ggplot(ml_train.cv, aes(ID, value)) +
  geom_smooth(se = FALSE,  method = 'glm', formula = y~ns(x, 6)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
  labs(title = '',x = 'Number of IDs', y = 'Cross-validation error')

p
#提取前 11 个重要的蛋白
p + geom_vline(xintercept = 11)

#作图展示 前 11 个重要的 蛋白
varImpPlot(ml_train.forest,sort=TRUE, n.var = min(11, nrow(ml_train.forest$importance)), main = 'Top 8 - variable importance')
result <- data.frame(importance(ml_train.forest,type=1))
result$ID <- row.names(result)
result <- data.frame( result[order(result$MeanDecreaseAccuracy,decreasing = T),])
write.table(result, 'importance_ID.txt', sep = '\t', col.names = NA, quote = FALSE)
##简约分类器

#选择 top11 重要的 OTUs，例如上述已经根据“Mean Decrease Accuracy”排名获得
ID_select <- result$ID [1:11]

#数据子集的训练集和测试集
ml_train_top11 <- ml_train[ ,c(ID_select, 'group')]
ml_test_top11 <- ml_test[ ,c(ID_select, 'group')]

#随机森林构建
set.seed(1234)
ml_train.forest_11 <- randomForest(group ~ ., data = ml_train_top11, importance = TRUE)
ml_train.forest_11

#训练集自身测试

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



#绘制ROC曲线
plot(ROC, print.auc=TRUE)

#使用测试集评估
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




#绘制ROC曲线
plot(ROC, print.auc=TRUE)

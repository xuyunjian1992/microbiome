# 载入R包：
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)


# 创建一个数据框，给出你个人的层次结构 
set.seed(1234)
d1 <- data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2 <- data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
edges <- rbind(d1, d2)

# 在叶子(个体)之间创建一个相互连接的数据框
all_leaves <- paste("subgroup", seq(1,100), sep="_")
connect <- rbind( 
  data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), 
  data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), 
  data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), 
  data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
# 子叶之间相互连接的程度
connect$value <- runif(nrow(connect))

# 创建顶点数据框，层次结构中的每个对象一行：
vertices  <-  data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(111)
) 

# 用每个名称的组添加一个列，这对后面的色点是有用的  
vertices$group  <-  edges$from[match(vertices$name, edges$to)]

# 添加关于我们将要添加的标签的信息:角度，水平调整和潜在翻转计算标签的角度  
vertices$id <- NA
myleaves <- which(is.na( match(vertices$name, edges$from) ))
nleaves <- length(myleaves)
vertices$id[ myleaves ] <- seq(1:nleaves)
vertices$angle <- 150 - 360 * vertices$id / nleaves

# 计算标签的对齐方式:左对齐还是右对齐  
# 如果在图的左侧，标签当前的角度为< -90  
vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)

# 翻转角度BY使它们可读
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)


# 创建graph对象
mygraph <- igraph::graph_from_data_frame( edges, vertices=vertices )

# 连接对象必须引用叶节点的id:  
from  <-  match( connect$from, vertices$name)
to  <-  match( connect$to, vertices$name)

# 基本常用参数
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", width=0.9) +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust), size=1.5, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))

ggsave("circle.pdf", height = 10, width = 10)


ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, width=0.9, aes(colour=..index..)) +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.25, y=y*1.25, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) +
  
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))


ggsave("circle2.pdf", height = 10, width = 10)





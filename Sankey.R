############## 桑基图 ######################
# 搞清楚需要的数据结构：
nodes = data.frame("name" = 
                     c("Node A", # Node 0
                       "Node B", # Node 1
                       "Node C", # Node 2
                       "Node D"))# Node 3

links = as.data.frame(matrix(c(
  0, 1, 10, # Each row represents a link. The first number
  0, 2, 20, # represents the node being conntected from. 
  1, 3, 30, # the second number represents the node connected to.
  2, 3, 40),# The third number is the value of the node
  byrow = TRUE, ncol = 3))

names(links) = c("source", "target", "value")

head(nodes)
head(links)

################### 基础绘图 ############################################
# install.packages("networkD3")
library(networkD3)
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)



############### 使用自己的数据 #############################################
links <- read.csv("links.csv",header = T, fileEncoding = "UTF-8-BOM")
nodes <- read.csv("nodes.csv", header = F, fileEncoding = "UTF-8-BOM")

links$target <- links$target -1
colnames(nodes) <- "name"

sankeyNetwork(Links = links, Nodes = nodes, 
              # 指定source、target、value和name：
              Source = "source",
              Target = "target", 
              Value = "value", 
              NodeID = "name", # 节点的名字
              # 调整配置：
              fontSize = 12, # 节点的字体大小
              nodeWidth = 30, # 节点的宽度
              nodePadding = 8 # 节点之间的距离
)


######################### 复杂桑基图 ######################################
nodes <- read.csv("nodes1.csv",row.names = 1, header = T, fileEncoding = "UTF-8-BOM")
links <- read.csv("links1.csv",header = T, fileEncoding = "UTF-8-BOM")

# Plot
sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 8, nodeWidth = 30)

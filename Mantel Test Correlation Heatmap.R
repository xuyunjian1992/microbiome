#################### 加载R包和查看示例数据 ####################
# 加载R包
library(tidyverse)
library(ggcor)
library(vegan)

# 查看数据：
data("varechem", package = "vegan")
varechem[1:5,1:10]
#       N    P     K    Ca    Mg    S    Al   Fe    Mn   Zn
# 18 19.8 42.1 139.9 519.4  90.0 32.3  39.0 40.9  58.1  4.5
# 15 13.4 39.1 167.3 356.7  70.7 35.2  88.1 39.0  52.4  5.4
# 24 20.2 67.7 207.1 973.3 209.1 58.1 138.0 35.4  32.1 16.8
# 27 20.6 60.8 233.7 834.0 127.2 40.7  15.4  4.4 132.0 10.7
# 23 23.8 54.5 180.6 777.0 125.8 39.5  24.2  3.0  50.1  6.6

data("varespec", package = "vegan")
varespec[1:5,1:10]
#    Callvulg Empenigr Rhodtome Vaccmyrt Vaccviti Pinusylv Descflex Betupube
# 18     0.55    11.13     0.00     0.00    17.80     0.07      0.0        0
# 15     0.67     0.17     0.00     0.35    12.13     0.12      0.0        0
# 24     0.10     1.55     0.00     0.00    13.47     0.25      0.0        0
# 27     0.00    15.13     2.42     5.92    15.97     0.00      3.7        0
# 23     0.00    12.68     0.00     0.00    23.73     0.03      0.0        0
# Vacculig Diphcomp
# 18     1.60     2.07
# 15     0.00     0.00
# 24     0.00     0.00
# 27     1.12     0.00
# 23     0.00     0.00

#################### mantel test ####################
# mantel test
mantel <- mantel_test(varespec, varechem,  # 传入需要检测的两个矩阵
                      # 这里很多人不明白，其实意思就是把44种植物分成4类，对应于varespec的44列：
                      spec.select = list(Spec01 = 1:7,
                                         Spec02 = 8:18,
                                         Spec03 = 19:37,
                                         Spec04 = 38:44)) %>% 
  # 下面就是检测的一些阈值，这个保持不变即可：
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))


#################### 快速出图 ####################
quickcor(varechem, type = "lower") +  # 传入的第一个数据是环境相关数据，type参数也可以是upper，
  geom_square() +
  # 这里的data传入mantel检验的结果：方块的颜色根据pd变化，大小根据rd变化；
  anno_link(aes(colour = pd, size = rd), data = mantel) +
  scale_size_manual(values = c(0.5, 1, 2))+  # 大小变化范围
  # 图例：guides函数不了解的可以看我B站的绘图教程：
  guides(size = guide_legend(title = "Mantel's r",
                             order = 2),  # 图例排序
         colour = guide_legend(title = "Mantel's p", 
                               order = 1),  # 图例排序
         fill = guide_colorbar(title = "Pearson's r", order = 3))

ggsave("mantel_corplot1.pdf", height = 7, width = 9)


#################### 美化 ####################
quickcor(varechem, type = "lower", show.diag = FALSE) +  # 传入的第一个数据是环境相关数据，type参数也可以是upper，
  geom_square() +
  # 这里的data传入mantel检验的结果：方块的颜色根据pd变化，大小根据rd变化；
  anno_link(aes(colour = pd, size = rd), data = mantel,
            curvature = -0.2) +  # 变曲线
  scale_size_manual(values = c(0.5, 1, 2))+  # 大小变化范围
  # 修改颜色属性：gradientn -- 渐变色； manul -- 分类变量颜色
  scale_fill_gradientn(values = seq(0,1,0.2),
                       colors = c("#610214", "#d05646", "#f5f4f4", "#569cc7", "#0b3b71")) +
  scale_colour_manual(values = c("#d85c01", "#29d300", "#A2A2A288")) +
  # 图例：guides函数不了解的可以看我B站的绘图教程：
  guides(size = guide_legend(title = "Mantel's r",
                             order = 2),  # 图例排序
         colour = guide_legend(title = "Mantel's p", 
                               order = 1),  # 图例排序
         fill = guide_colorbar(title = "Pearson's r", order = 3))

ggsave("mantel_corplot2.pdf", height = 7, width = 9)



#################### 花式修改 ####################
quickcor(varechem, type = "lower", show.diag = FALSE) +  # 传入的第一个数据是环境相关数据，type参数也可以是upper，
  geom_star(n=5) +  # 把正方形改成五角星
  # 这里的data传入mantel检验的结果：方块的颜色根据pd变化，大小根据rd变化；
  anno_link(aes(colour = pd, size = rd), data = mantel,
            curvature = -0.2) +  # 变曲线
  scale_size_manual(values = c(0.5, 1, 2))+  # 大小变化范围
  # 修改颜色属性：gradientn -- 渐变色； manul -- 分类变量颜色
  scale_fill_gradientn(values = seq(0,1,0.2),
                       colors = c("#610214", "#d05646", "#f5f4f4", "#569cc7", "#0b3b71")) +
  scale_colour_manual(values = c("#d85c01", "#29d300", "#A2A2A288")) +
  # 图例：guides函数不了解的可以看我B站的绘图教程：
  guides(size = guide_legend(title = "Mantel's r",
                             order = 2),  # 图例排序
         colour = guide_legend(title = "Mantel's p", 
                               order = 1),  # 图例排序
         fill = guide_colorbar(title = "Pearson's r", order = 3))

ggsave("mantel_corplot3.pdf", height = 7, width = 9)


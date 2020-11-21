# 功能：拟合标准曲线，计算各样品8-iso浓度
# 输出：
# 1. 标准曲线图（包括回归方程及拟合优度），result文件夹下的std_curve_xxx.jpg
# 2. 各实验对象尿样的8-iso浓度表（包括三个小组全部样品），data文件夹下的biomarker.csv

library(tidyverse)

# PART0 原始数据
## 标准管浓度
c_std = c(800, 400, 200, 100, 50)

## 标准管吸光度
abs_std_group_B = c(1.787,1.1154,0.6308,0.2728,0.1478)
abs_std_group_AC = c(1.589, 1.1978, 0.8116, 0.4093, 0.2267)

## 空白管吸光度
abs_blank_group_B = 0.0527
abs_blank_group_AC = 0.4673

# PART1 拟合标准曲线
## 按照说明书，以吸光度O.D.值为X轴，标准品浓度的自然对数为Y轴，拟合标准曲线
x_B = abs_std_group_B - abs_blank_group_B
x_AC = abs_std_group_AC - abs_blank_group_AC
fit_B = lm(log(c_std)~x_B) %>% summary()
fit_AC = lm(log(c_std)~x_AC) %>% summary()

## 绘制标准曲线并保存，注意拟合方程及绘图的Y轴是对数值
### 如下绘制AC两组的标准曲线
plot_AC = ggplot() + 
  geom_point(aes(x=x_AC, y=log(c_std))) + 
  geom_smooth(aes(x=x_AC, y=log(c_std)), method="lm", se=F) + 
  geom_text(aes(hjust=0, x=0.6, y=4.5, label=paste0("R-square=", round(fit_AC$r.squared*10000)/10000))) + 
  geom_text(aes(hjust=0, x=0.6, y=4.7, label=paste0("ln(y) = ", round(fit_AC$coefficients[1]*10000)/10000, " + ", round(fit_AC$coefficients[2]*10000)/10000, " * x"
  ))) + 
  xlab("O.D.") + 
  ylab("ln(8-iso)") + 
  ggpubr::theme_pubr()

### 如下绘制B组的标准曲线
plot_B = ggplot() + 
  geom_point(aes(x=x_B, y=log(c_std))) + 
  geom_smooth(aes(x=x_B, y=log(c_std)), method="lm", se=F) + 
  geom_text(aes(hjust=0, x=1, y=4.5, label=paste0("R-square=", round(fit_B$r.squared*10000)/10000))) + 
  geom_text(aes(hjust=0, x=1, y=4.7, label=paste0("ln(y) = ", round(fit_B$coefficients[1]*10000)/10000, " + ", round(fit_AC$coefficients[2]*10000)/10000, " * x"
  ))) + 
  xlab("O.D.") + 
  ylab("ln(8-iso)") + 
  ggpubr::theme_pubr()

### 保存绘图结果
ggsave("result/std_curve_AC.jpg", plot=plot_AC)
ggsave("result/std_curve_B.jpg", plot=plot_B)

# PART3 根据标准曲线，计算样品浓度
## 分别读取AC组、B组样品样品吸光度原始数据
raw_AC = read_csv("data/raw_8isoPGF_group_AC.csv")
raw_B = read_csv("data/raw_8isoPGF_group_B.csv")

## 计算样品中8-iso浓度
raw_AC$PGF = exp(raw_AC$OD * fit_AC$coefficients[2] + fit_AC$coefficients[1])
raw_B$PGF = exp(raw_B$OD * fit_B$coefficients[2] + fit_B$coefficients[1])

## 合并两张数据表
df = bind_rows(raw_AC, raw_B)
df$OD = NULL

## 保存计算结果，所有人（包括ABC三组）的8-iso浓度都在biomarker.csv表里
write_csv(df, "data/biomaker.csv")
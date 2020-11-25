library(tidyverse)
library(lme4)

# 定义一个计算平均PM2.5浓度的函数
calc_average = function(df, doi, lag) {
  df %>% 
    mutate(time_diff=difftime(df$datetime, doi, units="mins")) %>%
    filter(time_diff >= -lag*60 & time_diff <= 0) %>%
    group_by(subject) %>%
    summarise(pm1 = mean(pm1), pm25 = mean(pm25), pm10 = mean(pm10))
}

# 载入数据
load("data/pm_data.RData")
biomarker_data = read_csv("data/biomaker.csv")
meta_data = read_csv("data/metadata.csv")

# 两个实验阶段结束日期
doi1 = strptime("20-11-8 08-00", "%y-%m-%d %H-%M")
doi2 = strptime("20-11-16 08-00", "%y-%m-%d %H-%M")

# 计算平均PM.5浓度的时间窗（4天平均）
lag = 4 * 24

tdf1 = calc_average(pm_data, doi1, lag) %>%
  mutate(stage = "stage1")
tdf2 = calc_average(pm_data, doi2, lag) %>%
  mutate(stage = "stage2")

pm = bind_rows(tdf1, tdf2)

effect = spread(biomarker_data, date, PGF) %>%
  mutate(stage1 = .$`2020/11/8` - .$`2020/11/4`, 
         stage2 = .$`2020/11/16` - .$`2020/11/13`) %>%
  select(subject, stage1, stage2) %>%
  inner_join(meta_data, by="subject") %>% 
  gather("stage", "delta", 2:3) %>%
  inner_join(pm, by=c("subject", "stage")) %>%
  mutate(treatment = (stage=="stage1"&sequence=="A")|(stage=="stage2"&sequence=="B"))

# 线性混合效应模型
lmer(delta ~ pm25 + stage + treatment + (1|subject), data=effect) %>% summary()

# 交叉设计方差分析
## 做正态、方差齐检验！
aov(delta ~ subject + stage + treatment, data=effect) %>% summary()

# 分对照和处理看PM2.5的效应
lm(delta ~ pm25 + stage, data = effect %>% filter(treatment==T)) %>% summary()
lm(delta ~ pm25 + stage, data = effect %>% filter(treatment==F)) %>% summary()
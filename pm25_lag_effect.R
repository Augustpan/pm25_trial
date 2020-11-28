# 这部分代码赶工完成的，极其丑陋

library(tidyverse)
library(lme4)
library(broom)
library(ggpubr)
library(latex2exp)

source("std_curve.R")
source("pm25.R")

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
baseline = read_csv("data/baseline.csv")
baseline$dob = as.Date(baseline$dob, "%Y/%m/%d") %>% as.POSIXct()
baseline$age = difftime(Sys.time(), baseline$dob, units = "days") / 365

# 两个实验阶段结束日期
doi1 = strptime("20-11-8 08-00", "%y-%m-%d %H-%M")
doi2 = strptime("20-11-16 08-00", "%y-%m-%d %H-%M")

# 4 days average
lag = 4 * 24
tdf1 = calc_average(pm_data, doi1, lag) %>%
  mutate(stage = "stage1")
tdf2 = calc_average(pm_data, doi2, lag) %>%
  mutate(stage = "stage2")
pm_4days = bind_rows(tdf1, tdf2)
pm_4days$lag = "4 days"

# 2 days average
lag = 2 * 24
tdf1 = calc_average(pm_data, doi1, lag) %>%
  mutate(stage = "stage1")
tdf2 = calc_average(pm_data, doi2, lag) %>%
  mutate(stage = "stage2")
pm_2days = bind_rows(tdf1, tdf2)
pm_2days$lag = "2 days"

# 1 day average
lag = 1 * 24
tdf1 = calc_average(pm_data, doi1, lag) %>%
  mutate(stage = "stage1")
tdf2 = calc_average(pm_data, doi2, lag) %>%
  mutate(stage = "stage2")
pm_1days = bind_rows(tdf1, tdf2)
pm_1days$lag = "1 day"

pm = bind_rows(pm_1days, pm_2days, pm_4days)
effect = spread(biomarker_data, date, PGF) %>%
  mutate(stage1 = .$`2020/11/8` - .$`2020/11/4`, 
         stage2 = .$`2020/11/16` - .$`2020/11/13`) %>%
  select(subject, stage1, stage2) %>%
  inner_join(meta_data, by="subject") %>% 
  gather("stage", "delta", 2:3) %>%
  inner_join(pm, by=c("subject", "stage")) %>%
  mutate(treatment = (stage=="stage1"&sequence=="A")|(stage=="stage2"&sequence=="B")) %>%
  inner_join(baseline, by="subject")

sfomula = "delta ~ pm25 + stage + age + sex + pm10 + allergic_disease"
summ = bind_rows(
  (lm(as.formula(sfomula), data = effect %>% filter(lag == "1 day", treatment==T)) %>% tidy())[2,],
  (lm(as.formula(sfomula), data = effect %>% filter(lag == "2 days", treatment==T)) %>% tidy())[2,],
  (lm(as.formula(sfomula), data = effect %>% filter(lag == "4 days", treatment==T)) %>% tidy())[2,],
  (lm(as.formula(sfomula), data = effect %>% filter(lag == "1 day", treatment==F)) %>% tidy())[2,],
  (lm(as.formula(sfomula), data = effect %>% filter(lag == "2 days", treatment==F)) %>% tidy())[2,],
  (lm(as.formula(sfomula), data = effect %>% filter(lag == "4 days", treatment==F)) %>% tidy())[2,])

summ$term = c("Treatment", "Treatment", "Treatment", "Control", "Control", "Control")
summ$lag = c("1 day", "2 days", "4 days", "1 day", "2 days", "4 days")
write_csv(summ, "result/effect_lag.csv")

plot_effect_size = ggplot(summ, aes(x=lag, y=estimate, shape=term)) + 
  geom_errorbar(position=position_dodge(0.4), aes(ymin=estimate-std.error*1.96, ymax=estimate+std.error*1.96), width=.1) +
  geom_point(size=4, position=position_dodge(0.4)) + 
  geom_hline(aes(yintercept=0), linetype="dashed") + 
  ylab("Percentage change (%)") +
  xlab("") + 
  ylim(c(-20, 10)) + 
  scale_shape_manual(values=c(1, 16)) + 
  theme_pubr() + 
  guides(shape = guide_legend(""))
ggsave("result/effect_size_adj_lag.jpg", width=4, height=3, plot=plot_effect_size)
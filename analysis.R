library(tidyverse)
library(lme4)
library(broom)
library(ggpubr)

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
  mutate(treatment = (stage=="stage1"&sequence=="A")|(stage=="stage2"&sequence=="B")) %>%
  inner_join(baseline, by="subject")

# 对两阶段实验后8-iso变化绘图
plot_delta = qplot(x=stage,y=delta,data=effect, geom="boxplot") + 
  facet_wrap(~sequence) + 
  xlab("") + 
  ylab("Change in uric 8-iso PGF") +
  theme_pubr()
ggsave("result/delta.jpg", plot=plot_delta)

# 分对照和处理看PM2.5的效应
fit_trt = lm(delta ~ pm25, data = effect %>% filter(treatment==T))
fit_crtl = lm(delta ~ pm25, data = effect %>% filter(treatment==F))

summ = bind_rows(tidy(fit_trt)[2,], tidy(fit_crtl)[2,])
summ$term = c("Treatment", "Control")
write_csv(summ, "result/effect.csv")

plot_effect_size = ggplot(summ, aes(x=term, y=estimate)) + 
  geom_errorbar(aes(ymin=estimate-std.error*1.96, ymax=estimate+std.error*1.96), width=.1) +
  geom_point() + 
  geom_hline(aes(yintercept=0), linetype="dashed") + 
  ylab("Estimated effect of PM2.5") +
  xlab("") + 
  theme_pubr()

ggsave("result/effect_size.jpg", plot=plot_effect_size)

# 混杂控制
fit_trt = lm(delta ~ pm25 + stage + age + sex + pm10 + allergic_disease, data = effect %>% filter(treatment==T))
fit_crtl = lm(delta ~ pm25 + stage + age + sex + pm10 + allergic_disease, data = effect %>% filter(treatment==F))

summ = bind_rows(tidy(fit_trt)[2,], tidy(fit_crtl)[2,])
summ$term = c("Treatment", "Control")
write_csv(summ, "result/effect_adj.csv")

plot_effect_size = ggplot(summ, aes(x=term, y=estimate)) + 
  geom_errorbar(aes(ymin=estimate-std.error*1.96, ymax=estimate+std.error*1.96), width=.1) +
  geom_point() + 
  geom_hline(aes(yintercept=0), linetype="dashed") + 
  ylab("Estimated effect of PM2.5") +
  xlab("") + 
  theme_pubr()

ggsave("result/effect_size_adj.jpg", plot=plot_effect_size)

# 配对T检验，看处理前后有没有差异
temp = effect %>% 
  select(subject, sequence, stage, delta) %>% 
  spread(stage, delta) %>%
  na.omit()

xa = temp %>% filter(sequence=="A")
xb = temp %>% filter(sequence=="B")

s1 = c(xa$stage1, xb$stage2)
s2 = c(xa$stage2, xb$stage1)

tdf = bind_rows(tibble(delta=s1, group="Treatment"), tibble(delta=s2, group="Control"))

tt = t.test(s1, s2, paired = T)

plot_t_test = ggplot(tdf, aes(x=group, y=delta)) + 
  geom_boxplot() + 
  geom_jitter() + 
  geom_text(x=1.9, y= 120, label=paste0("paired t test p = ",(round(tt$p.value*10000)/10000))) + 
  xlab("") + 
  ylab("Change in uric 8-iso PGF") + 
  theme_pubr()

ggsave("result/t_test.jpg", plot=plot_t_test)

# 交叉设计方差分析
## 做正态、方差齐检验！
r_anova = aov(delta ~ subject + stage + treatment, data=effect)
write_csv(tidy(r_anova), "result/anova.csv")

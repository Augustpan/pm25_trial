library(tidyverse)

load("data/pm_data.RData")
biomarker_data = read_csv("data/biomaker.csv")
treatment_data = read_csv("data/treatment.csv")



# 定义一个函数来算平均PM2.5浓度
## doi是采样日期
## lag是需要平均的小时数
calc_average = function(df, doi, lag) {
  df %>% 
    mutate(time_diff=difftime(df$datetime, doi, units="mins")) %>%
    filter(time_diff >= -lag*60 & time_diff <= 0) %>%
    group_by(subject) %>%
    summarise(pm25 = mean(pm25), n = n())
}

# doi: date of interest
## doi1 一阶段结束
doi1 = strptime("20-11-8 08-00", "%y-%m-%d %H-%M")
doi2 = strptime("20-11-16 08-00", "%y-%m-%d %H-%M")

# lag time (hour)
lag = 8

calc_average(df, doi1, lag)
library(tidyverse)

# PART1 读取全部pm2.5个体暴露数据
## 这些数据储存在data/PM2.5文件夹下，文件名必须是实验对象的编号如A1.txt等
## 扫描data/PM2.5文件
flist = list.files("data/PM2.5")
sp = strsplit(flist, "\\.")

## 重新定义英文列名
cname = c("index", "date", "time", "pm1", "pm25", "pm10", "pm1_skip", "pm25_skip", "pm10_skip", "voltage", "empty")
## 定义变量类型
ctype = cols(
  index = col_double(),
  date = col_character(),
  time = col_character(),
  pm1 = col_double(),
  pm25 = col_double(),
  pm10 = col_double(),
  pm1_skip = col_skip(),
  pm25_skip = col_skip(),
  pm10_skip = col_skip(),
  voltage = col_skip(),
  empty = col_skip()
)

## 循环读入所有人的暴露数据，读入的数据存储在变量df里
df = tibble()
for (i in 1:length(sp)) {
  if (sp[[i]][2] == "txt") {
    tdf = read_tsv(paste0("data/PM2.5/", sp[[i]][1], ".txt"), col_names = cname, col_types = ctype, skip=1)
    tdf$object = sp[[i]][1]
    
    date_temp = as.Date(tdf$date, "%y-%m-%d")
    date_alter = as.Date(tdf$date, "%Y/%m/%d")

    mask = is.na(date_temp)
    date_temp[mask] = date_alter[mask]
    
    tdf$datetime = paste(date_temp, tdf$time) %>% strptime("%Y-%m-%d %H-%M") %>% as.POSIXct()
    df = bind_rows(df, tdf)
  }
}

# PART2 计算每次采样前6、12、24、48、72小时平均PM2.5浓度
## 定义一个函数来算平均PM2.5浓度
## doi是采样日期
## lag是需要平均的小时数
calc_average = function(df, doi, lag) {
  df %>% 
    mutate(time_diff=difftime(df$datetime, doi, units="mins")) %>%
    filter(time_diff >= -lag*60 & time_diff <= 0) %>%
    group_by(object) %>%
    summarise(pm25 = mean(pm25), n = n())
}

# doi: date of interest
## doi1 一阶段结束
doi1 = strptime("20-11-8 08-00", "%y-%m-%d %H-%M")
doi2 = strptime("20-11-16 08-00", "%y-%m-%d %H-%M")

# lag time (hour)
lag = 8

calc_average(df, doi1, lag)
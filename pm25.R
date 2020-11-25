library(tidyverse)

# PART1 读取全部pm2.5个体暴露数据
## 这些数据储存在data/PM2.5文件夹下，文件名必须是实验对象的编号如A1.txt等
## 扫描data/PM2.5文件夹
flist = list.files("data/PM2.5")
sp = strsplit(flist, "\\.")

## 重新定义英文列名
cname = c("index", "date", "time", "pm1", "pm25", "pm10", "pm1_skip", "pm25_skip", "pm10_skip", "voltage", "empty")
## 定义变量类型
ctype = cols(
  index = col_skip(),
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

## 循环读入所有人的暴露数据，读入的数据存储在变量pm_data里
pm_data = tibble()
for (i in 1:length(sp)) {
  if (sp[[i]][2] == "txt") {
    tdf = read_tsv(paste0("data/PM2.5/", sp[[i]][1], ".txt"), col_names = cname, col_types = ctype, skip=1)
    tdf$subject = sp[[i]][1]
    
    date_temp = as.Date(tdf$date, "%y-%m-%d")
    date_alter = as.Date(tdf$date, "%Y/%m/%d")

    mask = is.na(date_temp)
    date_temp[mask] = date_alter[mask]
    
    tdf$datetime = paste(date_temp, tdf$time) %>% strptime("%Y-%m-%d %H-%M") %>% as.POSIXct()
    pm_data = bind_rows(pm_data, tdf)
  }
}

pm_data$date = NULL

save(pm_data, file="data/pm_data.RData")
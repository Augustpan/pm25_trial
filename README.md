# pm25_trial
环境医学综合实验（环境卫生学部分），运动干预与PM2.5诱导的氧化应激反应。

## 数据
- data/baseline.csv：基线调查数据（只选取部分数据）
- data/raw_8isoPGF_group_AC.csv：A、C组尿8-isoPGF吸光度原始数据
- data/raw_8isoPGF_group_B.csv：B组尿8-isoPGF吸光度原始数据
- data/metadata.csv：分组情况（先运动、后运动组）
- data/PM2.5/*.txt：PM2.5个体暴露原始数据

## 代码
- std_curve.R：处理尿8-isoPGF酶标测定的原始数据（O.D.值），拟合标准曲线，并计算8-isoPGF浓度。生成如下结果文件：
	- result/std_curve_B.jpg：B组标准曲线
	- result/std_curve_AC.jpg：A、C组标准曲线
	- data/biomaker.csv：转换后的8-isoPGF浓度数据
- pm25.R：处理PM2.5个体暴露的原始数据，数据清洗等。生成如下结果文件：
	- data/pm_data.RData：所有实验对象的PM2.5暴露数据
- analysis.R：主要的分析过程在此进行。生成如下结果文件：
	- result/anova.csv：交叉设计方差分析
	- result/effect.csv：PM2.5效应的估计值（未控制混杂）
	- result/effect_adj.csv：PM2.5效应的估计值（控制混杂）
	- result/effect_size.jpg：PM2.5效应的组间差异（未控制混杂）
	- result/effect_size_adj.jpg：PM2.5效应的组间差异（控制混杂）
	- result/t_test.jpg：交叉设计配对t检验
	- result/delta.jpg：实验效应
- pm25_lag_effect.R：分析了1天、2天、4天平均PM2.5暴露对结果的可能影响
	- effect_size_adj_lag.jpg
	- effect_lag.csv
- baseline_report.Rmd：基线数据、暴露数据的描述性统计。生成如下结果文件：
	- baseline_report.html

## 结果
都在result文件夹下，参见生成这些结果的代码，不具体描述了
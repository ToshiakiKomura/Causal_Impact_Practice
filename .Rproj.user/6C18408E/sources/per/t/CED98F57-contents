#(1)ライブラリの読み出し
library("tidyverse")
library("broom")
#(2)データの読み込み
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")
#(3)ルールによるメールの配信を行ったログを作成
##データの整形とrunning varialeの追加
male_data <- email_data %>%
filter(segment %in% c("Mens E-Mail", "No-E-Mail"))%>%
mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0),
history_log = log(history))
##cut-offの値を指定
threshold_value <- 5.5
##ルールによる介入を再現したデータの作成
##cut-offによりrunninbg variableが大きければ配信されたデータのみ残す
##逆の場合は配信されなかったデータのみ残す
##running variableを0.1単位で区切ったグループ分けの変数を追加
rdd_data  <- male_data %>%
mutate(history_log_grp = round(history_log/0.1,0)*0.1)  %>%
filter(((history_log > threshold_value)&
(segment == "Mens E-Mail")) |
(history_log <= threshold_value)&
(segment == "No E-Mail"))
##RDDデータでの比較
rdd_data_table <- rdd_data %>%
group_by(treatment)%>%
summarise(count= n(),
visit_rate = mean(visit))
#(6)回帰分析による分析
##線形回帰による分析
rdd_lm_reg <- rdd_data %>%
mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))%>%
lm(data = ., formula = visit ~ treatment +  history_log) %>%
tidy() %>%
filter(term == "treatment")
## 非線形回帰による分析
library("rddtools")
nonlinear_rdd_data <- rdd_data(y = rdd_data$visit,
x = rdd_data$history_log,
cutpoint = 5.5)
nonlinear_rdd_ord4 <- rdd_reg_lm(rdd_object=nonlinear_rdd_data, order=4)
nonlinear_rdd_ord4
#(8)nonparametric RDD
##ライブラリの読み込み
install.packages("rdd")
library("rdd")
##non-parametric RDDの実行
rdd_result <- RDestimate(data  = rdd_data,
formula = visit ~ history_log,
cutpoint = 5.5)
##結果のレポート
summary(rdd_result)
##結果のプロット
plot(rdd_result)

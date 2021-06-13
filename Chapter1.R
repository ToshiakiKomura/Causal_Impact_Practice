install.packages("tidyverse")
library("tidyverse")
email_data <-read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")
#データの準備
#女性向けメールは配信されたデータを削除したデータを作成
male_df <- email_data %>%
#女性向けメールが配信されたデータを削除
filter(segment !="Womens E-Mail") %>%
#介入を表すtreatment変数を追加
mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))
#集計による比較
##group_by()とsummarise()を使って集計
summary_by_segment <-male_df %>%
#データのグループ化
group_by(treatment) %>%
#グループごとのconversionの平均
summarise(conversion_rate = mean(conversion),
#グループごとのspendの平均
spend_mean = mean(spend),
#グループごとのデータ数
count = n())
summary_by_segment
##男性向けメールが配信されたグループの購買データを得る
mens_mail <- male_df %>%
filter(treatment ==1) %>%
pull(spend)
##男性向けメールが配信されなかったグループの購買データを得る
no_mail <- male_df %>%
filter(treatment==0) %>%
pull(spend)
##メール配信の差に対して有意さ検定を実行
rct_ttest <-t.test(mens_mail, no_mail, var.equal = TRUE)
rct_ttest
biased_data
##セレクションバイアスのあるデータを作成
##再現性確保のため乱数シードを固定
set.seed(1)
##条件に反応するサンプルの量を半分にする
obs_rate_c <-0.5
obs_rate_t<-0.5
##バイアスのあるデータを作成
biased_data <- male_df %>%
mutate(obs_rate_c = if_else(
history>300|(recency<6)|(channel=="Multichannel"),obs_rate_c,1),
obs_rate_t = if_else(
history>300|(recency<6)|(channel=="Multichannel"),obs_rate_t,1),
random_number = runif(n=NROW(male_df))) %>%
filter((treatment == 0 & random_number < obs_rate_c) |
treatment == 1 & random_number < obs_rate_t)
#セレクションバイアスのあるデータで平均を比較
##group_byとsummarise_byを使って集計(Biased)
summary_by_segment_biased <- biased_data %>%
group_by(treatment) %>%
summarise(conversion_ratee = mean(conversion),
spend_mean = mean(spend),
count = n())
#t.ttestを使ってt検定を行う(Biased)
##男性向けメールが配信されたグループの購買データを得る
mens_mail_biased <- biased_data %>%
filter(treatment == 1) %>%
pull(spend)
##メールが配信されなかったグループの購買データを得る
no_mail_biased <- biased_data %>%
filter(treatment == 0) %>%
pull(spend)
#平均の差に対して有意差検定を実行
rct_ttest_biased <- t.test(mens_mail_biased, no_mail_biased, var.equal = T)
summary_by_segment_biased
rct_ttest_biased

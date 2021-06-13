#バイアスのあるデータでの回帰分析
##回帰分析の実行
biased_reg <- lm(data = biased_data,
formula = spend ~ treatment + history)
##分析結果のレポート
summary(biased_reg)
#ライブラリの読み出し
library("broom")
##推定されたパラメータを取り出す
biased_reg_coef <- tidy(biased_reg)
biased_reg_coef
#RCTデータでの回帰分析とバイアスのあるデータでの回帰分析の比較
##RCTデータでの単回帰
rct_reg <- lm(data=male_df, formula = spend ~ treatment)
rct_reg_coef <- summary(rct_reg)
##バイアスのあるデータでの単回帰
nonrct_reg <- lm(data = biased_data, formula = spend ~ treatment)
nonrct_reg <- summary(nonrct_reg)
rct_reg_coef
nonrct_reg
#バイアスのあるデータでの重回帰
nonrct_mreg <- lm(data = biased_data,
formula = spend ~ treatment + recency + channel + history)
nonrct_mreg_coef <- tidy(nonrct_mreg)
nonrct_mreg_coef
#OVBの確認(broomを利用した場合)
##broomの読み出し
library(broom)
#モデル式のベクトルを用意
formula_vec <-c(spend ~ treatment + recency + channel, #モデルA
spend ~ treatment + recency + channel + history, #モデルB
history~ treatment + channel + recency)#モデルC
##formulaに名前を付ける
names(formula_vec) <- paste("reg", LETTERS[1:3], spe = "_")
#モデル式のデータフレーム化
models <- formula_vec %>%
enframe(name = "model_index", value = "formula")
##まとめて回帰分析を実行
df_models <- models %>%
mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
mutate(lm_result = map(.x = model, .f = tidy))
##モデルの結果を整形
df_results <- df_models %>%
mutate(formula = as.character(formula)) %>%
select(formula, model_index, lm_result) %>%
unnest(cols = c(lm_result))
#モデルA,B,Cでのtreatmentのパラメータを抜き出す
treatment_coef <- df_results %>%
filter(term == "treatment") %>%
pull(estimate)
#モデルBからhistoryのパラメータを抜き出す
history_coef <- df_results %>%
filter(model_index == "reg_B",
term == "history") %>%
pull(estimate)
##OVBの確認
OVB <- history_coef*treatment_coef[3]
coef_gap <- treatment_coef[1] - treatment_coef[2]
OVB # beta_2*gamma_1
coef_gap # alpha_1 - beta_1
OVB
coef_gap
#入れてはいけない変数を入れてみる
#visitと介入との相関
cor_visit_treatment <- lm(
data = biased_data,
formula = treatment ~ visit + channel + recency + history) %>%
tidy()
cor_visit_treatment
#visitを入れた回帰分析を実行
bad_control_reg <-  lm(
data = biased_data,
formula = spend ~ treatment + channel + recency + history + visit) %>%
tidy()
bad_control_reg

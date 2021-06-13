#傾向スコアの推定
ps_model <- glm(data = biased_data,
formula = treatment ~ recency + history + channel,
family = binomial)
#傾向スコアマッチング
##ライブラリの読み込み
install.packages("MatchIt")
library("MatchIt")
##傾向スコアを利用したマッチング
m_near <- matchit(formula = treatment ~ recency + history + channel,
data = biased_data,
method = "nearest",
replace = TRUE)
##マッチング後のデータを作成
matched_data <- match.data(m_near)
matched_data
install.packages("dplyr")
library(dplyr)
##マッチング後のデータで効果の推定
PSM_result <- matched_data %>%
lm(spend  ~ treatment, data = .)
PSM_result
#逆確率重み付き推定(IPW)
##ライブラリの読み込み
install.packages("WeightIt")
library("WeightIt")
##重みの推定
weighting <- weightit(formula = treatment ~ recency + history + channel,
data = biased_data,
method = "ps",
estimand = "ATE")
##重み付きデータでの効果の推定
IPW_result <- lm(data = biased_data,
formula = spend ~ treatment,
weights = weighting$weights)
IPW_result
#統計モデルを用いたメールの配信ログの分析
##学習データと配信ログを作るデータに分割
set.seed(1)
train_flag <- sample(NROW(male_df), NROW(male_df)/2, replace = FALSE)
male_df_train <- male_df[train_flag,]%>%
filter(treatment == 0)
male_df_test <- male_df[-train_flag,]
##売上が発生する確率を予測するモデルの作成
predict_model <- glm(
data = male_df_train,
formula = conversion ~ recency + history_segment + channel + zip_code,
family = binomial
)
##売上の発生確率からメールの配信確率を決定
pred_cv <- predict(predict_model,
newdata = male_df_test,
type = "response")
pred_cv_rank <- percent_rank(pred_cv)
##配信確率をもとにメールの配信を決定
mail_assign <- sapply(pred_cv_rank, rbinom, n=1, size=1)
##配信ログを作成
ml_male_df <- male_df_test %>%
mutate(mail_assign = mail_assign,
ps = pred_cv_rank) %>%
filter((treatment == 1 & mail_assign ==1)|
(treatment == 1 & mail_assign ==0))
ml_male_df
##実験をしていた場合の平均の差を確認
rct_male_lm <- lm(data = male_df_test, formula = spend ~ treatment)
rct_male_lm
##平均の比較
ml_male_lm <- lm(data = ml_male_df, formula = spend ~ treatment)
ml_male_lm
#Matchingパッケージをインストール
install.packages("Matching")
library(Matching)
PSM_result <- Match(Y = ml_male_df$spend,
Tr = ml_male_df$treatment,
X = ml_male_df$ps,
estimand = "ATT")
PSM_result
##IPWの推定
w.out <- weighit(treatment ~ recency + history _segment + channel + zip_code,
data = ml_male_df
ps = ml_male_df$ps
mathod = "ps"
estimand = "ATE"
)
##IPWの推定
w.out <- weighit(treatment ~ recency + history _segment + channel + zip_code,
data = ml_male_df
ps = ml_male_df$ps
mathod = "ps"
estimand = "ATE"
)
##重み付けしたデータでの共変量のバランスを確認
love.plot(W.out,
threshold = .1)
##重み付けしたデータでの効果の分析
IPW_result <- ml_male_df %>%
lm(data = .,
spend ~ treatment,
weights = W.out$weights)
#havenパッケージのインストール
install.packages("haven")
#ライブラリの読み込み
library("tidyverse")
library("haven")
library("broom")
library("MatchIt")
library("WeightIt")
library("cobalt")
# NBER archiveからデータを読み込む
cps1_data <- read_dta("http://users.nber.org/~rdehejia/data/cps_controls.dta")
cps3_data <- read_dta("http://users.nber.org/~rdehejia/data/cps_controls3.dta")
nswdw_data <- read_dta("http://users.nber.org/~rdehejia/data/nsw_dw.dta")
#データセットの準備
##NSWデータから介入グループだけ取り出してCPS1における介入グループとして扱う
cps1_nsw_data <- nswdw_data %>%
filter(treat == 1) %>%
rbind(cps1_data)
##NSWデータから介入グループだけ取り出してCPS3における介入グループとして扱う
cps3_nsw_data <- nswdw_data %>%
filter(treat == 1) %>%
rbind(cps3_data)
#RCTデータでの分析
##共変量付きの回帰分析
nsw_cov <- nswdw_data %>%
lm(data = .,
re78 ~ treat + re74 + re75  + age + education + black + hispanic + nodegree + married) %>%
filter(term == "treat")
#バイアスのあるデータでの回帰分析
##CPS1の分析結果
cps1_reg <- cps1_nsw_data %>%
lm(data = .,
re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married)%>%
tidy()%>%
filter(term == "treat")
##CPS3の分析結果
cps3_reg <- cps3_nsw_data %>%
lm(data = .,
re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married)%>%
tidy()%>%
filter(term == "treat")
cps1_reg
cps3_reg
#傾向スコアマッチングによる効果推定
##傾向スコアを用いたマッチング
m_near <- matchit(treat ~ age + education + black + hispanic + nodegree + married + re74 + re75 + I(re74^2) + I(re75^2),
data = cps1_nsw_data,
method = "nearest")
##共変量のバランスを確認
love.plot(m_near,
threshold = .1)
##マッチング後のデータを作成
matched_data <- match.data(m_near)
##マッチング後のデータで効果の推定
PSM_result_cps1 <- matched_data %>%
lm(re78 ~ treat, data = .)%>%
tidy()
PSM_result_cps1
#IPWによる効果推定
##重みの推定
weighting <- weightit(treat ~ age + education + black + hispanic + nodegree + married +  re74 + re75 + I(re74^2) + I(re75^2),
data = cps1_nsw_data,
method = "ps",
estimand = "ATE")
##共変量のバランスを確認
love.plot(weighting, threshold = .1)
##重み付きデータでの効果の推定
IPW_result <- cps1_nsw_data %>%
lm(data = ., formula = re78 ~ treat,
weights = weighting$weights) %>%
tidy()
IPW_result


# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第5部 10章
# 応用：広告の効果はどれだけ持続するか



# この章で使うパッケージ --------------------
install.packages("KFAS")
install.packages("forecast")
install.packages("ggplot2")
install.packages("ggfortify")

library(KFAS)
library(forecast)
library(ggplot2)
library(ggfortify)


# シミュレーションデータの作成 --------------------

# シミュレーションにおけるサンプルサイズ
n_sample <- 450

# 乱数の種
set.seed(10)

# 時間によって変化する広告の効果
true_reg_coef <- -log(1:50)*2 + 8

# ランダムウォークする水準値
mu <- cumsum(rnorm(n = n_sample, sd = 0.5)) + 15

# 水準値＋広告効果として状態を作る
x <- mu + c(rep(0, 200), true_reg_coef, rep(0, 200))

# 観測誤差を作る
obs_error <- rnorm(n = n_sample, sd=2)

# 広告効果が入った売り上げデータ
sales_ad <- x + obs_error

# 説明変数としての広告フラグ(1なら広告有り)
ad_flg <- numeric(n_sample)
ad_flg[201:250] <- 1

# シミュレーションデータの図示
ggtsdisplay(ts(sales_ad), main = "シミュレーションデータ")



# KFASによる時変係数モデル --------------------

# Step1：モデルの構造を決める
build_reg <- SSModel(
  H = NA,
  sales_ad ~ 
    SSMtrend(degree = 1, Q = NA) +
    SSMregression( ~ ad_flg , Q = NA)
)

# Step2 パラメタ推定
fit_reg <- fitSSM(build_reg, inits = c(1, 1, 1))

# Step3、4：フィルタリング・スムージング
result_reg <- KFS(
  fit_reg$model, 
  filtering = c("state", "mean"),
  smoothing = c("state", "mean")
)


# 変化する広告効果の図示 --------------------

# 信頼区間付きの回帰係数
interval_coef <- predict(fit_reg$model, states = "regression", 
                         interval = "confidence", level = 0.95)

# データの整形
coef_df <- cbind(
  data.frame(time = 201:250, reg_coef = true_reg_coef),
  as.data.frame(interval_coef[201:250, ])
)

# 図示
ggplot(data = coef_df, aes(x = time, y = reg_coef)) + 
  labs(title="広告の効果の変化") +
  geom_point(alpha = 0.6, size = 0.9) +
  geom_line(aes(y = fit), size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3)










# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第5部 9章
# 実装：変化するトレンドのモデル化



# この章で使うパッケージ --------------------
install.packages("KFAS")
install.packages("forecast")
install.packages("ggplot2")
install.packages("ggfortify")

library(KFAS)
library(forecast)
library(ggplot2)
library(ggfortify)
library(gridExtra)


# トレンドと観測値の関係 --------------------
# シミュレーションにおけるサンプルサイズ
n_sample <- 450

# 変化しないトレンド
t0 <- 0.2

# トレンドの累積和が、観測値となる
constant_trend <- cumsum(rep(t0, n_sample))

# 変化するトレンド
t1 <- 0.2
t2 <- 0.4
t3 <- 0
t4 <- -0.2

trend <- c(rep(t1, 100), rep(t2, 100), rep(t3, 100), rep(t4, 150))

# トレンドの累積和が、観測値となる
change_trend <- cumsum(trend)

# 図示
p1 <- autoplot(ts(constant_trend), 
               xlab = "Time", main = "変化しないトレンド(トレンド= 0.2)")
p2 <- autoplot(ts(change_trend), 
               xlab = "Time", main = "変化するトレンド")
grid.arrange(p1, p2)

# シミュレーションデータの作成 --------------------

# 水準の過程誤差を作る
set.seed(12)
system_noise <- rnorm(n = n_sample)

# 真の水準値
alpha_true <- numeric(n_sample + 1)
for(i in 1:n_sample){
  alpha_true[i + 1] <- alpha_true[i] + trend[i] + system_noise[i]
}

# cumsum関数を使っても結果は同じ
cumsum(system_noise + trend)
sum(alpha_true[-1] - cumsum(system_noise + trend))

# 観測誤差を作る
obs_noise <- rnorm(n = n_sample, sd=5)

# トレンドが変化する売り上げデータ
sales <- alpha_true[-1] + obs_noise + 11

# 架空の売り上げデータを図示
autoplot(ts(sales), main = "架空の売り上げデータ")

# 最後の50期間は、テストのために残しておく
sales_train <- sales[1:400]
sales_test <- sales[401:450]

# 途中の50期間は欠損とする
sales_train[125:175] <- NA


# KFASによるローカル線形トレンドモデル --------------------

# Step1：モデルの構造を決める
build_trend <- SSModel(
  H = NA,
  sales_train ~ SSMtrend(degree = 2, Q = c(list(NA), list(NA))) 
)

# Step2：パラメタ推定
fit_trend <- fitSSM(build_trend, inits = c(1, 1, 1))

# Step3、4：フィルタリング・スムージング
result_trend <- KFS(
  fit_trend$model, 
  filtering = c("state", "mean"),
  smoothing = c("state", "mean")
)


# 推定されたパラメタ
# 観測誤差の分散
fit_trend$model$H
# 過程誤差の分散
fit_trend$model$Q

# 補足：モデルの行列表現 --------------------
fit_trend$model$T
fit_trend$model$R
fit_trend$model$Z


# トレンドの図示 --------------------

# levelは推定された売り上げの状態。slopeはトレンド。
head(result_trend$alphahat, n = 3)

# データの整形
trend_df <- data.frame(
  time = 1:length(sales_train),
  true_trend = trend[1:length(sales_train)],
  estimate_trend = result_trend$alphahat[, "slope"]
)

# 図示
ggplot(data = trend_df, aes(x = time, y = true_trend)) + 
  labs(title="トレンドの変化") +
  geom_line(aes(y = true_trend), size = 1.2, linetype="dashed") +
  geom_line(aes(y = estimate_trend), size = 1.2)

# 補間と予測 --------------------

# 平滑化状態と予測区間
interval_trend <- predict(
  fit_trend$model, interval = "prediction", level = 0.95)

# 将来予測の結果と予測区間
forecast_trend <- predict(
  fit_trend$model, interval = "prediction", level = 0.95, n.ahead = 50)

# 過去の状態と予測結果をまとめた
estimate_all <- rbind(interval_trend, forecast_trend)


# ローカル線形トレンドモデルによる予測の考え方 --------------------

# 最後のレベルと傾きを取得
last_level <- tail(result_trend$a[, "level"], n = 1)
last_trend <- tail(result_trend$a[, "slope"], n = 1)
last_level # 水準
last_trend # トレンド

# トレンド成分を足していくだけの予測
fore <- cumsum(c(last_level, rep(last_trend, 49)))

# 参考 for構文を使った場合
fore2 <- numeric(50)
fore2[1] <- last_level
for(i in 2:50){
  fore2[i] <- fore2[i - 1] + last_trend
}
sum(fore2 - fore)

# パッケージの予測結果と比較
forecast_trend[, "fit"]
fore


# 補間と予測結果の図示 --------------------

# データの整形
df <- cbind(
  data.frame(sales = sales, time = 1:n_sample), 
  as.data.frame(estimate_all)
)

# 図示
ggplot(data = df, aes(x = time, y = sales)) + 
  labs(title="トレンドが変わる売り上げの予測") +
  geom_point(alpha = 0.6, size = 0.9) +
  geom_line(aes(y = fit), size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3)


# ARIMAによる予測 --------------------
# モデルの構築
mod_arima <- auto.arima(sales_train)
# 予測
forecast_arima <- forecast(mod_arima, h=50, level = 0.95)
#予測結果の図示
autoplot(forecast_arima, main = "ARIMAによる予測", 
         predict.colour = 1, shadecols = "gray")

# 予測精度の比較
accuracy(forecast_trend[, "fit"], sales_test)["Test set", "RMSE"]
accuracy(forecast_arima, sales_test)["Test set", "RMSE"]





# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第2部 7章
# RによるARIMAモデル



# この章で使うパッケージ --------------------
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("ggfortify")

library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)


# 分析の対象となるデータ -----------------
# R組み込みのデータ
# イギリスの交通事故死傷者数
Seatbelts

# 前方座席の死傷者数のみ抽出
front <- Seatbelts[, "front"]
front


# 対数変換 -----------------
# 対数系列
log_front <- log(front)

# 図示
ggtsdisplay(log_front, main="対数系列")


# 差分系列の作成方法 -----------------
# ラグをとる方法
# 原系列
front
# ラグをとった
lag(front, -1)

# 差分系列
front - lag(front, -1)

# diff関数を使う方法
diff(front, lag=1)

# 対数差分系列
log_diff <- diff(log_front)

# 図示
ggtsdisplay(log_diff, main="対数差分系列")


# 季節差分系列の作成方法 -----------------
# 参考 季節ごとのグラフ
ggsubseriesplot(front)

# 頻度
frequency(front)
diff(front, lag=frequency(front))

# 対数差分系列に、さらに季節差分をとる
seas_log_diff <- diff(log_diff, lag=frequency(log_diff))

# 図示
ggtsdisplay(seas_log_diff, main="季節差分系列")


# 自己相関とコレログラム -----------------
acf(seas_log_diff, plot=F, lag.max=12)
pacf(seas_log_diff, plot=F, lag.max=12)


# コレログラム
autoplot(
  acf(seas_log_diff, plot=F), 
  main="対数系列のコレログラム"
)



# 訓練データとテストデータに分ける -----------------
# 対数変換
Seatbelts_log <- Seatbelts[,c("front", "PetrolPrice", "law")]
Seatbelts_log[,"front"] <- log(Seatbelts[,"front"])
Seatbelts_log[,"PetrolPrice"] <- log(Seatbelts[,"PetrolPrice"])
Seatbelts_log

# 訓練データとテストデータに分ける
train <- window(Seatbelts_log, end=c(1983,12))
test <- window(Seatbelts_log, start=c(1984,1))

# 説明変数だけ切り出す
petro_law <- train[, c("PetrolPrice", "law")]


# ARIMAモデルの推定 -----------------

model_sarimax <- Arima(
  y = train[, "front"], 
  order = c(1, 1, 1),
  seasonal = list(order = c(1, 0, 0)), 
  xreg = petro_law
)

model_sarimax

# 差分系列とARIMAの次数の関係 -----------------
# 差分系列
Arima(
  y = log_diff, order =c(1, 0, 0),
  include.mean = F
)

Arima(
  y = log_front, order =c(1, 1, 0)
)

# 季節差分系列
Arima(
  y = seas_log_diff, order =c(1, 0, 0),
  include.mean = F
)

Arima(
  y = log_front, order =c(1, 1, 0),
  seasonal = list(order = c(0, 1, 0))
)


# 自動モデル選択auto.arima関数 -----------------

# ◇◇◇◇注意◇◇◇◇
# 以下の関数を実行する前にお使いのPCのコア数を確認してください。
# num.coresの値は、お使いのPCに合わせて変更してください
sarimax_petro_law <- auto.arima(
  y = train[, "front"], 
  xreg = petro_law,
  ic = "aic",
  max.order = 7,
  stepwise = F,
  approximation = F,
  parallel = T,
  num.cores = 4
)

# bestモデル
sarimax_petro_law


# 定常性・反転可能性のチェック -----------------
# AR項
abs(polyroot(c(1,-coef(sarimax_petro_law)[c("ar1", "ar2")])))
# MA項
abs(polyroot(c(1,coef(sarimax_petro_law)[c("ma1", "ma2", "ma3")])))
# Seasonal AR項
abs(polyroot(c(1,-coef(sarimax_petro_law)[c("sar1","sar2")])))


# 残差のチェック -----------------

# 残差の自己相関のチェック
# 帰無仮説は「残差に自己相関がない」
checkresiduals(sarimax_petro_law)

# 残差の正規性の検定
# 帰無仮説は「正規分布に従う」
jarque.bera.test(resid(sarimax_petro_law))



# ARIMAによる予測 -----------------
petro_law_test <- test[, c("PetrolPrice", "law")]
sarimax_f <- forecast(
  sarimax_petro_law, 
  xreg = petro_law_test,
  h = 12,
  level = c(95, 70)
)
sarimax_f
autoplot(sarimax_f, predict.colour=1, main = "ARIMAによる予測")

# 過去の石油価格の平均値を使う
petro_law_mean <- data.frame(
  PetrolPrice=rep(mean(train[, "PetrolPrice"]),12),
  law=rep(1, 12)
)
sarimax_f_mean <- forecast(sarimax_petro_law, xreg=petro_law_mean)

# 直近の石油価格を使う
petro_law_tail <- data.frame(
  PetrolPrice=rep(tail(train[, "PetrolPrice"], n=1),12),
  law=rep(1, 12)
)
sarimax_f_tail <- forecast(sarimax_petro_law, xreg=petro_law_tail)

# ナイーブ予測 -----------------
# 過去の平均値を予測値として使う
naive_f_mean <- meanf(train[, "front"], h = 12)
naive_f_mean
mean(train[, "front"])

# 過去の最新の値を、予測値として使う
naive_f_latest <- rwf(train[, "front"], h = 12)
naive_f_latest
tail(train[, "front"], n = 1)

# 予測の評価 -----------------

# SARIMXのRMSE
sarimax_rmse <- sqrt(
  sum((sarimax_f$mean - test[, "front"])^2) / 
    length(sarimax_f$mean)
)

sarimax_rmse


# 未来の石油価格がわかっている前提の予測
accuracy(sarimax_f, x=test[, "front"])

# テストデータのRMSEのみを抽出
accuracy(sarimax_f, x=test[, "front"])["Test set", "RMSE"]



# 石油価格の平均値を使用
accuracy(sarimax_f_mean, x=test[, "front"])["Test set", "RMSE"]

# 直近の石油価格を使用
accuracy(sarimax_f_tail, x=test[, "front"])["Test set", "RMSE"]


# ナイーブ予測① 過去の平均値
accuracy(naive_f_mean, x=test[, "front"])["Test set", "RMSE"]

# ナイーブ予測①  直近の値
accuracy(naive_f_latest, x=test[, "front"])["Test set", "RMSE"]



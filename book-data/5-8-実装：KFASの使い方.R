
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第5部 8章
# 実装：KFASの使い方



# この章で使うパッケージ --------------------
install.packages("KFAS")
install.packages("ggplot2")

library(KFAS)
library(ggplot2)


# 分析の対象となるデータ -----------------

# ナイル川の流量データ
Nile

# 最後の20年間はテストデータとする
nile_train <- window(Nile, end = 1950)

# 途中、20年間欠損があったとする
nile_train[41:60] <- NA

# KFASによるローカルレベルモデルの推定 --------------------

# Step1：モデルの構造を決める
build_kfas <- SSModel(
  H = NA,
  nile_train ~ SSMtrend(degree = 1, Q = NA)
)

# Step2：パラメタ推定
fit_kfas <- fitSSM(build_kfas, inits = c(1, 1))

# Step3、4：フィルタリング・スムージング
result_kfas <- KFS(
  fit_kfas$model, 
  filtering = c("state","mean"),
  smoothing = c("state", "mean")
)

# 推定結果
# 観測誤差の分散
fit_kfas$model$H

# 過程誤差の分散
fit_kfas$model$Q


# フィルタ化推定量
mu_filter_kfas <- result_kfas$a[-1]

# 平滑化推状態
mu_smooth_kfas <- result_kfas$alphahat

# 推定結果の図示 --------------------
df_filter <- data.frame(
  y         = as.numeric(Nile[1:80]), 
  time      = 1871:1950,
  mu_filter = mu_filter_kfas
)

ggplot(data = df_filter, aes(x = time, y = y)) + 
  labs(title="フィルタ化推定量") +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = mu_filter), size = 1.2) 


# KFASによる状態の推定と信頼・予測区間 --------------------

# 平滑化状態の95%信頼区間
smooth_conf <- predict(
  fit_kfas$model, interval = "confidence", level = 0.95)

head(smooth_conf, n = 3)

# 平滑化状態の95%予測区間
smooth_pred <- predict(
  fit_kfas$model, interval = "prediction", level = 0.95)

head(smooth_pred, n = 3)

head(mu_smooth_kfas)

# KFASによる予測 --------------------

# 20年先まで予測する
forecast_pred <- predict(
  fit_kfas$model, interval = "prediction", level = 0.95, n.ahead = 20)

# 未来予測の結果と過去の平滑化状態を結合する
estimate_all <- rbind(smooth_pred, forecast_pred)

## 図示
# データをまとめる
df_forecast <- cbind(
  data.frame(y = as.numeric(Nile), time = 1871:1970), 
  as.data.frame(estimate_all)
)

ggplot(data = df_forecast, aes(x = time, y = y)) +
  labs(title="平滑化状態と将来予測") +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = fit), size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3)


# 補足：ローカルレベルモデルにおける予測の出し方 --------------------

# 点推定値は、最新の状態の値と全く同じ
tail(smooth_pred, n = 1)
head(forecast_pred, n = 5)

# 予測誤差の標準偏差を取得
forecast_se <- predict(
  fit_kfas$model, interval = "prediction", level = 0.95, 
  n.ahead = 20, se.fit = T)[, "se.fit"]

# 予測誤差の分散
forecast_se^2

# 予測誤差の分散の増加量
diff(forecast_se^2)

# 過程誤差の分散の値と等しい
fit_kfas$model$Q

# 予測と補間 --------------------

# 未来の値をNAとしたもの
nile_na <- Nile
nile_na[81:100] <- NA
build_kfas_na <- SSModel(
  H = NA, nile_na ~ SSMtrend(degree = 1, Q = NA)
)
fit_kfas_na <- fitSSM(build_kfas_na, inits = c(1, 1))

# 未来の値を切り捨てたもの
nile_split <- window(Nile, end = 1950)
build_kfas_split <- SSModel(
  H = NA, nile_split ~ SSMtrend(degree = 1, Q = NA)
)
fit_kfas_split <- fitSSM(build_kfas_split, inits = c(1, 1))

# 予測と補間の結果はまったく同じになる
hokan <- predict(
  fit_kfas_na$model, interval = "confidence", level = 0.95)[81:100,]

yosoku <- predict(
  fit_kfas_split$model, interval = "confidence", level = 0.95, n.ahead = 20)

all(hokan == yosoku)








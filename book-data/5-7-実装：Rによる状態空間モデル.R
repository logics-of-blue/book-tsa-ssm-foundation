
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第5部 7章
# 実装：Rによる状態空間モデル



# この章で使うパッケージ --------------------
install.packages("dlm")
install.packages("KFAS")
install.packages("ggplot2")
install.packages("ggfortify")

library(dlm)
library(KFAS)
library(ggplot2)
library(ggfortify)


# 分析の対象と分析の流れ --------------------

# データ
Nile

# サンプルサイズ
length(Nile)

# Rで実装するカルマンフィルタ:関数を作る --------------------

kfLocalLevel <- function(y, mu_pre, P_pre, sigma_w, sigma_v) {
  ## Step1 予測
  # 状態の予測(ローカルレベルモデルなので、予測値は、前期の値と同じ)
  mu_forecast <- mu_pre
  
  # 状態の予測誤差の分散(過程誤差の分散だけ増える)
  P_forecast <- P_pre + sigma_w
  
  # 観測値の予測(ローカルレベルモデルなので、状態の予測値と同じ)
  y_forecast <- mu_forecast
  
  # 観測値の予測誤差の分散(状態の分散に加えて、観測誤差の分散だけ増える)
  F <- P_forecast + sigma_v
  
  ## Step2 フィルタリング(状態の補正)
  # カルマンゲイン( P_forecast/F としても同じです)
  K <- P_forecast / (P_forecast + sigma_v)
  
  # 観測値の予測残差
  y_resid <- y - y_forecast
  
  # カルマンゲインを使って状態を補正
  mu_filter <- mu_forecast + K * y_resid
  
  # 補正された状態の誤差の分散
  P_filter <- (1 - K) * P_forecast
  
  # 結果の格納
  result <- data.frame(
    mu_filter = mu_filter, 
    P_filter = P_filter,
    y_resid = y_resid,
    F = F,
    K = K
  )
  
  return(result)
}

# Rで実装するカルマンフィルタ:状態を推定する --------------------

# サンプルサイズ
N <- length(Nile)

# 状態の推定値
mu_filter <- numeric(N)

# 「状態」の初期値は0とします
mu_zero <- 0
mu_filter <- c(mu_zero, mu_filter)

# 状態の予測誤差の分散
P_filter <- numeric(N)

# 「状態の予測誤差の分散」の初期値は10000000にします
P_zero <- 10000000
P_filter <- c(P_zero, P_filter)

# 観測値の予測残差
y_resid <- numeric(N)

# 観測値の予測誤差の分散
F <- numeric(N)

# カルマンゲイン
K <- numeric(N)

# 過程誤差の分散
sigma_w <- 1000

# 観測誤差の分散
sigma_v <- 10000

# カルマンフィルタの逐次計算を行う
for(i in 1:N) {
  kekka <- kfLocalLevel(
    y = Nile[i], mu_pre = mu_filter[i], P_pre = P_filter[i], 
    sigma_w = sigma_w, sigma_v = sigma_v
  )
  mu_filter[i + 1] <- kekka$mu_filter
  P_filter[i + 1] <- kekka$P_filter
  y_resid[i] <- kekka$y_resid
  F[i] <- kekka$F
  K[i] <- kekka$K
}

mu_filter


# Rで実装するカルマンフィルタの対数尤度 --------------------
# 観測値の予測誤差の系列
y_resid

# 観測値の予測誤差の分散の時系列
F

# 対数尤度の計算1
# dnorm関数を使った対数尤度の計算
sum(log(dnorm(y_resid, mean = 0, sd = sqrt(F))))

# 対数尤度の計算 2
-1 * (N/2) * log(2 * pi) - 1/2 * sum(log(F) + y_resid^2 / F)

# 対数尤度の計算 3
# dlmパッケージで計算される対数尤度
1/2 * sum(log(F) + y_resid^2 / F)


# Rで実装する最尤法 --------------------
calkLogLik <- function(sigma) {
  sigma_w <- exp(sigma[1]) # 分散は負にならないのでEXPをとる
  sigma_v <- exp(sigma[2]) # 分散は負にならないのでEXPをとる

  # 変数の定義など 
  N <- length(Nile)               ;  mu_filter <- numeric(N)
  mu_zero <- 0                    ;  mu_filter <- c(mu_zero, mu_filter)
  P_filter <- numeric(N)          ;  P_zero <- 10000000
  P_filter <- c(P_zero, P_filter) ;  y_resid <- numeric(N)
  F <- numeric(N)                 ;  K <- numeric(N)
  
  # カルマンフィルタの実行
  for(i in 1:N) {
    kekka <- kfLocalLevel(
      y = Nile[i], mu_pre = mu_filter[i], P_pre = P_filter[i], 
      sigma_w = sigma_w, sigma_v = sigma_v
    )
    mu_filter[i + 1] <- kekka$mu_filter
    P_filter[i + 1] <- kekka$P_filter
    y_resid[i] <- kekka$y_resid
    F[i] <- kekka$F
    K[i] <- kekka$K
  }
  return(1/2 * sum(log(F) + y_resid^2 / F))
}

# 最適なパラメタを求める
best_sigma <- optim(calkLogLik, par = c(1,1), method = "L-BFGS")

# 計算結果
exp(best_sigma$par)

# Rで実装する平滑化 --------------------
smoothLocalLevel <- function(mu_filterd, P_filterd, r_post, s_post, 
                             F_post, y_resid_post, K_post) {
  # 状態平滑化漸化式
  r <- y_resid_post/F_post + (1 - K_post) * r_post
  mu_smooth <- mu_filterd + P_filterd * r
  
  # 状態分散平滑化漸化式
  s <- 1/F_post + (1 - K_post)^2 * s_post
  P_smooth <- P_filterd - P_filterd^2 * s
  
  # 結果の格納
  result <- data.frame(
    mu_smooth = mu_smooth,
    P_smooth = P_smooth,
    r = r,
    s = s
  )
  
  return(result)
  
}

# 平滑化状態
mu_smooth <- numeric(N + 1)

# 平滑化状態分散
P_smooth <- numeric(N + 1)

# 漸化式のパラメタ（初期値は0のままでよい）
r <- numeric(N)
s <- numeric(N)

# 最後のデータは、フィルタリングの結果とスムージングの結果が一致する
mu_smooth[N + 1] <- mu_filter[N + 1]
P_smooth[N + 1] <- P_filter[N + 1]

# 逆順でループ
for(i in N:1){
  kekka <- smoothLocalLevel(
    mu_filter[i],P_filter[i],r[i], s[i], F[i], y_resid[i], K[i]
  )
  mu_smooth[i] <- kekka$mu_smooth
  P_smooth[i] <- kekka$P_smooth
  r[i - 1] <- kekka$r
  s[i - 1] <- kekka$s
}

mu_smooth


# dlmによるカルマンフィルタ --------------------

# dlmのパラメタの設定
mod_dlm <- dlmModPoly(
  order = 1, m0 = 0, C0 = 10000000, dW = sigma_w, dV = sigma_v
)

# カルマンフィルタの実行
mu_filter_dlm <- dlmFilter(Nile, mod_dlm)

# 結果の比較
mu_filter_dlm$m
mu_filter

sum((mu_filter_dlm$m[-1] - mu_filter[-1])^2)


# dlmによる対数尤度の計算 --------------------

# 対数尤度の指標
dlmLL(Nile, mod_dlm)

# 比較
1/2 * sum(log(F) + y_resid^2 / F)


# dlmによる平滑化 --------------------

# 平滑化の実行
mu_smooth_dlm <- dlmSmooth(mu_filter_dlm)

# 結果の比較
mu_smooth_dlm$s
mu_smooth

sum((mu_smooth_dlm$s - mu_smooth)^2)


# 参考：dlmの使い方 --------------------

# Step1 モデルの構造を決める
build_local_level_dlm <- function(theta){
  dlmModPoly(order = 1, dV = exp(theta[1]), dW = exp(theta[2]))
}

# Step2 パラメタ推定
par_local_level_dlm <- dlmMLE(Nile, parm=c(1, 1), build_local_level_dlm)

# 推定された分散を使って、モデルを組みなおす
fit_local_level_dlm <- build_local_level_dlm(par_local_level_dlm$par)

# Step3 フィルタリング
filter_local_level_dlm <- dlmFilter(Nile, fit_local_level_dlm)

# Step4 スムージング
smooth_local_level_dlm <- dlmSmooth(filter_local_level_dlm)

## 推定結果
# 推定されたパラメタ
# dlmパッケージ使用
exp(par_local_level_dlm$par)
# パッケージ不使用
exp(best_sigma$par)

# その他推定結果
fit_local_level_dlm
filter_local_level_dlm$m
smooth_local_level_dlm$s

## 図示

# フィルタ化推定量の図示
autoplot(filter_local_level_dlm, fitted.colour = "black", 
         fitted.size = 1.5, main = "フィルタ化推定量")

# 平滑化状態の図示
p_nile <- autoplot(Nile)
autoplot(smooth_local_level_dlm, fitted.colour = "black", 
         colour = "black", size = 1.5, main="平滑化状態", p=p_nile)


# Rで実装する散漫カルマンフィルタ --------------------

# 状態の推定値
mu_diffuse_filter <- numeric(N + 1)

# 状態の予測誤差の分散
P_diffuse_filter <- numeric(N + 1)

# 散漫初期化を用いると、1時点目のフィルタ化推定量は以下のようになる
mu_diffuse_filter[2] <- Nile[1]
P_diffuse_filter[2] <- sigma_v

# 観測値の予測残差
y_resid_diffuse <- numeric(N)

# 観測値の予測誤差の分散
F_diffuse <- numeric(N)

# カルマンゲイン
K_diffuse <- numeric(N)

# カルマンフィルタの逐次計算を行う
for(i in 2:N) {
  kekka <- kfLocalLevel(
    y = Nile[i], mu_pre = mu_diffuse_filter[i],
    P_pre = P_diffuse_filter[i], sigma_w = sigma_w, sigma_v = sigma_v
  )
  mu_diffuse_filter[i + 1] <- kekka$mu_filter
  P_diffuse_filter[i + 1] <- kekka$P_filter
  y_resid_diffuse[i] <- kekka$y_resid
  F_diffuse[i] <- kekka$F
  K_diffuse[i] <- kekka$K
}

# カルマンフィルタの結果と比較
# 散漫カルマンフィルタ
mu_diffuse_filter
# 普通のカルマンフィルタ
mu_filter

sum((mu_diffuse_filter - mu_filter)^2)

# Rで実装する散漫対数尤度 --------------------

# dnorm関数を使った対数尤度の計算
sum(
  log(
    dnorm(y_resid_diffuse[-1], mean = 0, sd = sqrt(F_diffuse[-1]))
  )
)

# 対数尤度の計算 2
-1 * ((N - 1)/2) * log(2 * pi) - 
  1/2*sum(log(F_diffuse[-1]) + y_resid_diffuse[-1]^2 / F_diffuse[-1])


# KFASによる散漫カルマンフィルタ --------------------

# KFASのパラメタの設定
mod_kfas <- SSModel(
  H = sigma_v, Nile ~ SSMtrend(degree = 1, Q = sigma_w)
)

# 散漫カルマンフィルタの実行
mu_filter_kfas <- KFS(
  mod_kfas, filtering = c("state", "mean"), smoothing = "none"
)

# 結果の比較
sum((mu_filter_kfas$a - mu_diffuse_filter)^2)



# KFASによる散漫対数尤度の計算 --------------------

# 散漫対数尤度
logLik(mod_kfas)

# 比較
sum(log(dnorm(y_resid_diffuse[-1], mean = 0, sd = sqrt(F_diffuse[-1]))))

# 普通のカルマンフィルタにおける対数尤度
sum(log(dnorm(y_resid, mean = 0, sd = sqrt(F))))





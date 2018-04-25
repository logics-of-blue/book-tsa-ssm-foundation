
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第6部 3章
# 実装：Stanの使い方



# この章で使うパッケージ --------------------

install.packages("rstan", dependencies = TRUE)
install.packages("ggplot2")


library(rstan)
library(ggplot2)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# シミュレーションデータの作成 --------------------

# data
n_sample <- 100           # サンプルサイズ
y <- numeric(n_sample)    # 観測値

# parameters
mu_zero <- 100            # 状態の初期値
mu <- numeric(n_sample)   # 状態の推定値
s_w <- 1000               # 過程誤差の分散
s_v <- 5000               # 観測誤差の分散

# model

set.seed(1)
# 状態の初期値から最初の時点の状態が得られる
mu[1] <- rnorm(n = 1, mean = mu_zero, sd = sqrt(s_w))

# 状態方程式に従い、状態が遷移する
for(i in 2:n_sample) {
  mu[i] <- rnorm(n = 1, mean = mu[i-1], sd = sqrt(s_w))
}

# 観測方程式に従い、観測値が得られる
for(i in 1:n_sample) {
  y[i] <- rnorm(n = 1, mean = mu[i], sd = sqrt(s_v))
}

# Stanによるローカルレベルモデルの推定 --------------------

# データの準備
data_sim <- list(y = y, n_sample = n_sample)

# モデルの推定
fit_stan_1 <- stan(
  file = "6-3-local-level-model.stan",
  data = data_sim,
  iter = 550,
  warmup = 50,
  thin = 1,
  chains = 4,
  seed = 1
)

# 結果の出力と収束の判定 --------------------

# すべての結果を出力
options(max.print=100000)
print(fit_stan_1)

# 過程誤差・観測誤差の分散
print(
  fit_stan_1,                       # 推定結果
  digits = 1,                       # 小数点桁数
  pars = c("s_w", "s_v", "lp__"),   # 表示するパラメタ
  probs = c(0.025, 0.5, 0.975)      # 区間幅の設定
)

# トレースプロット
traceplot(fit_stan_1, pars = c("s_w", "s_v"))


# 収束をよくするための調整 --------------------

# モデルの推定
fit_stan_2 <- stan(
  file = "6-3-local-level-model.stan",
  data = data_sim,
  iter = 5000,
  warmup = 2500,
  thin = 5,
  chains = 4,
  seed = 1
)

# トレースプロット
traceplot(fit_stan_2, pars = c("s_w", "s_v"))

# 結果の出力
print(
  fit_stan_2,                       # 推定結果
  digits = 1,                       # 小数点桁数
  probs = c(0.025, 0.5, 0.975)      # 区間幅の設定
)


# ベクトル化による効率的な実装 --------------------

# モデルの推定
fit_stan_3 <- stan(
  file = "6-3-local-level-model-vector.stan",
  data = data_sim,
  iter = 5000,
  warmup = 2500,
  thin = 5,
  chains = 4,
  seed = 1
)

# トレースプロット
traceplot(fit_stan_3, pars = c("s_w", "s_v"))

print(
  fit_stan_3,                       # 推定結果
  digits = 1,                       # 小数点桁数
  pars = c("s_w", "s_v", "lp__"),   # 表示するパラメタ
  probs = c(0.025, 0.5, 0.975)      # 区間幅の設定
)



# 乱数として得られた多数のパラメタの取り扱い --------------------

# 生成された乱数を格納
sampling_result <- rstan::extract(fit_stan_2)

# 一つの推定値あたりに、得られた乱数の個数
length(sampling_result$s_w)

# いくつ得られるのか計算
iter <- 5000      # 繰り返し数
warmup <- 2500    # 切り捨て期間
thin <- 5         # 間引き数
chains <- 4       # chain数

((iter - warmup)/ thin) * chains

# EAP推定量
mean(sampling_result$s_w)

# パラメタの事後分布の95%区間
quantile(sampling_result$s_w, probs=c(0.025, 0.5, 0.975))

# ヒストグラム
ggplot(data.frame(s_w = sampling_result$s_w), aes(x = s_w)) +
  geom_histogram()



# 推定結果の図示 --------------------

# 状態(100時点の状態が各々2000サンプルずつある)
sampling_result$mu

# 1時点目の状態(2000サンプルある)
sampling_result$mu[, 1]

# 1時点目の状態の95%区間と中央値
quantile(sampling_result$mu[, 1], probs=c(0.025, 0.5, 0.975))

# すべての時点の状態の、95%区間と中央値
model_mu <- t(apply(
  X = sampling_result$mu,    # 実行対象となるデータ
  MARGIN = 2,                # 列を対象としてループ
  FUN = quantile,            # 実行対象となる関数
  probs=c(0.025, 0.5, 0.975) # 上記関数に入れる引数
))
colnames(model_mu) <- c("lwr", "fit", "upr") # 列名の変更


# データの整形
stan_df <- cbind(
  data.frame(y = y, time = 1:n_sample), 
  as.data.frame(model_mu)
)

# 図示
ggplot(data = stan_df, aes(x = time, y = y)) + 
  labs(title="stanによる推定結果") +
  geom_point(alpha = 0.6, size = 0.9) +
  geom_line(aes(y = fit), size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3)





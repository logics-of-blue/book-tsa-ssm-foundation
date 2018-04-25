
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第6部 5章
# 応用：非線形な状態方程式を持つモデル



# この章で使うパッケージ --------------------

# install.packages("rstan", dependencies = TRUE)
install.packages("ggplot2")


library(rstan)
library(ggplot2)
library(ggfortify)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データの読み込み --------------------

# ファイル読み込み
data_file <- read.csv("6-5-logistic-growth-data.csv")
head(data_file, n = 3)

# ts型に変換
data_ts <- ts(data_file[, -1], start = 1961, frequency = 1)

# 図示
autoplot(data_ts, main = "架空の個体数データ")

# 弱情報事前分布 --------------------
# Kの最大値
exp(10)
# データの最大値
max(data_file$y)


# Stanによるモデルの推定 --------------------

# データの準備
data_stan <- list(
  y = data_file$y, 
  n_sample = nrow(data_file)
)

# モデルの推定
fit_stan_growth <- stan(
  file = "6-5-logistic-growth-model.stan",
  data = data_stan,
  iter = 5000,
  thin = 5,
  chains = 4,
  seed = 1
)

# 結果の確認 --------------------

# 参考：すべての推定結果の出力
options(max.print=100000)
print(fit_stan_growth, probs = c(0.025, 0.5, 0.975), digits = 1)

# 結果の出力
print(
  fit_stan_growth,
  digits = 2,
  probs = c(0.025, 0.5, 0.975),
  pars = c("r", "K")
)

# 個体数の理論上の上限値
sampling_result <- rstan::extract(fit_stan_growth)
exp(quantile(sampling_result$K, probs = c(0.1, 0.5, 0.9)))

# 平滑化された個体数の図示 --------------------

# 誤差を取り除いた状態の中央値
model_lambda_smooth <- apply(
  X = sampling_result$lambda_smooth,
  MARGIN = 2,
  FUN = median
)

# データの整形
stan_df <- data.frame(
  y = data_file$y, 
  fit = model_lambda_smooth,
  time = data_file$time
)

# 図示
ggplot(data = stan_df, aes(x = time, y = y)) + 
  labs(title="誤差を取り除いた個体数の変動") +
  geom_line() +
  geom_line(aes(y = fit), size = 1.2)


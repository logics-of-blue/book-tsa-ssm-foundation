
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第6部 4章
# 応用：複雑な観測方程式を持つモデル



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
data_file <- read.csv("6-4-animal_catch_data.csv")
head(data_file, n = 3)

# ts型に変換
data_ts <- ts(data_file[, -1], start = 1911, frequency = 1)

# 図示
autoplot(data_ts)

# Stanによるモデルの推定 --------------------

# データの準備
data_stan <- list(
  y = data_file$y, 
  catch_y = data_file$catch_y, 
  n_sample = nrow(data_file)
)

# モデルの推定
fit_stan_count <- stan(
  file = "6-4-count-model.stan",
  data = data_stan,
  iter = 8000,
  thin = 10,
  chains = 4,
  seed = 1
)

# 結果の確認 --------------------

# 参考：すべての推定結果の出力
options(max.print=100000)
print(fit_stan_count, probs = c(0.025, 0.5, 0.975), digits = 1)

# 結果の出力
print(
  fit_stan_count,
  digits = 2,
  probs = c(0.025, 0.5, 0.975),
  pars = c("trend", "coef_catch_y", "best_catch_y")
)

# 平滑化された個体数の図示 --------------------


# 観測誤差を取り除いた状態の、95%区間と中央値
sampling_result <- rstan::extract(fit_stan_count)
model_lambda_smooth <- t(apply(
  X = sampling_result$lambda_smooth,
  MARGIN = 2,
  FUN = quantile,
  probs=c(0.025, 0.5, 0.975)
))
colnames(model_lambda_smooth) <- c("lwr", "fit", "upr")


# データの整形
stan_df <- cbind(
  data.frame(y = data_file$y, time = data_file$time), 
  as.data.frame(model_lambda_smooth)
)

# 図示
ggplot(data = stan_df, aes(x = time, y = y)) + 
  labs(title="観測誤差を取り除いた個体数の変動") +
  geom_point(alpha = 0.6, size = 0.9) +
  geom_line(aes(y = fit), size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) 







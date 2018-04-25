
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第3部 1章
# 見せかけの回帰とその対策



# この章で使うパッケージ --------------------

install.packages("urca")
install.packages("lmtest")
install.packages("prais")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("gridExtra") 

library(urca)
library(lmtest)
library(prais)
library(ggplot2)
library(ggfortify)
library(gridExtra)


# 通常の回帰分析 --------------------
# 一回のシミュレーションにおけるサンプルサイズ
n_sample <- 400

# 乱数の種
set.seed(1)

# シミュレーションデータの作成
y_wn <- rnorm(n = n_sample)
x_wn <- rnorm(n = n_sample)

# モデルの構築
mod_ols_wn <- lm(y_wn ~ x_wn)
# 結果の表示
summary(mod_ols_wn)


# 単位根のあるデータ同士の回帰分析 --------------------
# 乱数の種
set.seed(1)

# ランダムウォークするデータ
y_rw <- cumsum(rnorm(n = n_sample))
x_rw <- cumsum(rnorm(n = n_sample))

# モデルの構築
mod_ols_rw <- lm(y_rw ~ x_rw)
# 結果の表示
summary(mod_ols_rw)


# 図示
# データの整形
df_wn <- data.frame(x_wn = x_wn, y_wn = y_wn)
# ggplot2による図示
p_wn <- ggplot(df_wn, aes(x=x_wn, y=y_wn)) + # 外枠
  geom_point() +                             # 散布図の追加
  geom_smooth(method = "lm", colour=1) +     # 回帰直線の追加
  ggtitle("White-Noise")                     # グラフタイトル

# データの整形
df_rw <- data.frame(x_rw = x_rw, y_rw = y_rw)
# ggplot2による図示
p_rw <- ggplot(df_rw, aes(x=x_rw, y=y_rw)) + # 外枠
  geom_point() +                             # 散布図の追加
  geom_smooth(method = "lm", colour=1) +     # 回帰直線の追加
  ggtitle("Random-Walk")                     # グラフタイトル

# グラフを並べて表示
grid.arrange(p_wn, p_rw, ncol=2)

# 定常AR過程への回帰分析 --------------------
# 乱数の種
set.seed(2)
# 定常AR過程に従うデータ
y_ar <- arima.sim(
  n = n_sample,
  model = list(order = c(1,0,0), ar = c(0.8))
)

x_ar <- arima.sim(
  n = n_sample,
  model = list(order = c(1,0,0), ar = c(0.8))
)

# モデルの構築
mod_ols_ar <- lm(y_ar ~ x_ar)
# 結果の表示
summary(mod_ols_ar)



# DW検定 --------------------
# DW統計量
resid_ols <- mod_ols_rw$residuals
dw <- sum(diff(resid_ols)^2) / sum((resid_ols)^2)
dw

# DW検定
# ホワイトノイズ
dwtest(mod_ols_wn)
# ランダムウォーク
dwtest(mod_ols_rw)
# AR(1)過程
dwtest(mod_ols_ar)


# 見せかけの回帰のシミュレーション --------------------

# 参考：p値の取得方法
summary(mod_ols_wn)$coefficients
summary(mod_ols_wn)$coefficients["x_wn","Pr(>|t|)"]

# シミュレーションの回数
n_sim <- 200

# 1度のシミュレーションにおけるサンプルサイズ
n_sample <- 400

# p値を格納する変数
p_wn <- numeric(n_sim)
p_rw <- numeric(n_sim)

# シミュレーションの実行
set.seed(1)
for(i in 1:n_sim){
  # 自己相関のないシミュレーションデータ
  y_wn <- rnorm(n = n_sample)
  x_wn <- rnorm(n = n_sample)
  
  # 線形回帰分析の実行
  mod_wn <- lm(y_wn ~ x_wn)
  
  # p値を保存
  p_wn[i] <- summary(mod_wn)$coefficients["x_wn","Pr(>|t|)"]
  
  # ランダムウォークするシミュレーションデータ
  y_rw <- cumsum(rnorm(n = n_sample))
  x_rw <- cumsum(rnorm(n = n_sample))
  
  # 線形回帰分析の実行
  mod_rw <- lm(y_rw ~ x_rw)
  
  # p値を保存
  p_rw[i] <- summary(mod_rw)$coefficients["x_rw","Pr(>|t|)"]
}

# 5%水準で有意よなったか？
p_wn < 0.05

# 5%水準で有意となる割合
# ホワイトノイズ
sum(p_wn < 0.05) / n_sim
# ランダムウォーク
sum(p_rw < 0.05) / n_sim



# 単位根検定 --------------------

# ランダムウォークへのADF検定
summary(ur.df(y_rw, type = "none"))
summary(ur.df(x_rw, type = "none"))

# 定常AR(1)過程へのADF検定
summary(ur.df(y_ar, type = "none"))
summary(ur.df(x_ar, type = "none"))



# RによるGLS --------------------
# 2 step Prais-Winsten法の実行
# 定常AR(1)過程に従うデータをOLSでモデル化(再掲)
mod_ols_ar <- lm(y_ar ~ x_ar)
# 残差
resid_ols_ar <- mod_ols_ar$residuals

# 残差データを使って、ρをOLSで推定
mod_resid <- lm(resid_ols_ar[-1] ~ resid_ols_ar[-n_sample] - 1)
ro <- as.numeric(mod_resid$coefficients)
ro

# 最初の時点データを変換
y_trans_1   <- sqrt(1 - ro^2)*y_ar[1]
x_trans_1   <- sqrt(1 - ro^2)*x_ar[1]
psi_trans_1 <- sqrt(1 - ro^2)

# 2時点目以降を変換
y_trans_2   <- y_ar[-1] - ro*y_ar[-n_sample]
x_trans_2   <- x_ar[-1] - ro*x_ar[-n_sample]
psi_trans_2 <- rep(1 - ro, n_sample-1)

# 変換されたデータを結合
y_trans_all   <- c(y_trans_1, y_trans_2)
x_trans_all   <- c(x_trans_1, x_trans_2)
psi_trans_all <- c(psi_trans_1, psi_trans_2)

# Prais-Winsten推定量を求める
mod_gls_hand <- lm(y_trans_all ~ psi_trans_all + x_trans_all - 1)
summary(mod_gls_hand)


# パッケージを使った GLS --------------------
# データをdata.frame形式にまとめる
d <- data.frame(
  y_ar = y_ar,
  x_ar = x_ar
)

# 2 step Prais-Winsten法の実行
mod_gls_PW <- prais.winsten(y_ar ~ x_ar, data=d, iter = 1)
mod_gls_PW

# 差分系列への回帰分析 --------------------
mod_lm_diff <- lm(diff(y_rw) ~ diff(x_rw))
summary(mod_lm_diff)


# 共和分 --------------------

# 共和分のあるシミュレーションデータ
set.seed(10)
rw <- cumsum(rnorm(n = n_sample))
x_co <- 0.6 * rw + rnorm(n = n_sample)
y_co <- 0.4 * rw + rnorm(n = n_sample)

# 両方ともに単位根であることを棄却できない
summary(ur.df(y_co, type = "none"))
summary(ur.df(x_co, type = "none"))

# グラフを見ると、やはりランダムウォークに見える
# ただし特定の係数をかけてから和をとると、ランダムウォークには見えなくなる

# データを1つのdata.frameにまとめる
df <- data.frame(
  y_co = y_co,
  x_co = x_co,
  z = x_co - (0.6/0.4)*y_co
)

# ts型に変換
ts_df <- ts(df)

# 図示
autoplot(ts_df, facets = T)


# 共和分検定 --------------------
# データの整形
data_mat <- matrix(nrow = n_sample, ncol = 2)
data_mat[,1] <- y_co
data_mat[,2] <- x_co

# 共和分検定
summary(ca.po(data_mat, demean = "none"))


# 共和分のあるデータに、差分をとってから回帰
y_co_diff <- diff(y_co)
x_co_diff <- diff(x_co)

mod_lm_diff_cointegrate <- lm(y_co_diff ~ x_co_diff)
summary(mod_lm_diff_cointegrate)





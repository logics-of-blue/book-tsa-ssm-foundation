
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第3部 3章
# ARCH・GARCHモデルとその周辺



# この章で使うパッケージ --------------------

install.packages("xts")
install.packages("fGarch")
install.packages("rugarch")
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("gridExtra") 

library(xts); library(fGarch); library(rugarch)
library(forecast); library(tseries)
library(ggplot2); library(ggfortify); library(gridExtra)

# シミュレーションによるデータの作成 -----------------

# 1回のシミュレーションにおけるサンプルサイズ
n_sample <- 1000

# GARCH(1,1)に従うデータのシミュレーション
#モデルのパラメタの設定
spec1 <- garchSpec(
  model = list(omega = 0.001, alpha = 0.4, beta = 0.5, mu = 0.1),
  cond.dist = "norm"
)

# シミュレーションデータの生成
set.seed(1)
sim_garch <- garchSim(
  spec1, 
  n = n_sample, 
  extended = T
)

# ts型に変換
sim_garch <- ts(sim_garch)

# データの表示
head(sim_garch, n = 2)

# 図示
autoplot(sim_garch[,-3], facets = T, ylab = "")

# 原系列のコレログラムの作成
p_acf <- autoplot(
  acf(sim_garch[,"garch"], plot=F), 
  main="原系列のコレログラム"
)
# 2乗した系列のコレログラムの作成
p_acf_sq <- autoplot(
  acf(sim_garch[,"garch"]^2, plot=F), 
  main="2乗した系列のコレログラム"
)

# グラフを並べて表示
grid.arrange(p_acf, p_acf_sq, ncol=1)


# fGarchパッケージによるGARCHモデル -----------------
# モデル化
mod_fGarch <- garchFit(
  formula = ~  garch(1, 1),
  data = sim_garch[,"garch"],
  include.mean = T,
  trace = F
)

# 推定結果の表示
coef(mod_fGarch)

# rugarchパッケージによるGARCHモデル -----------------
# モデルの構造の設定
spec_rugarch1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
  mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
  distribution.model = "norm"
)

# モデルの推定
mod_rugarch <- ugarchfit(
  spec = spec_rugarch1, data = sim_garch[,"garch"], solver='hybrid'
)

# 推定結果の表示
coef(mod_rugarch)

# ARMA + GARCHモデル -----------------
#モデルのパラメタの設定
spec2 <- garchSpec(
  model = list(
    omega = 0.001, alpha = 0.5, beta = 0.4, 
    mu = 0.1, ar = -0.6, ma = -0.5
  ), 
  cond.dist = "norm"
)

# シミュレーションデータの生成
set.seed(0)
sim_arma_garch <- garchSim(
  spec2, 
  n = n_sample, 
  extended = F
)

# ARMAモデル
mod_arma <- Arima(sim_arma_garch, order=c(1,0,1))

# 残差のチェック
checkresiduals(mod_arma)
jarque.bera.test(mod_arma$residuals)


# ARMA + GARCHモデル
# モデルの構造の設定
spec_rugarch2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
  mean.model=list(armaOrder=c(1,1), include.mean=TRUE),
  distribution.model = "norm"
)
# モデルの推定
mod_arma_garch <- ugarchfit(
  spec = spec_rugarch2, data = sim_arma_garch, solver='hybrid'
)

# 結果の表示
coef(mod_arma_garch)
mod_arma_garch

# 標準化残差：ARMA+GARCH
resid_arma_garch <- residuals(mod_arma_garch) / sigma(mod_arma_garch)
# 標準化残差：ARMA
resid_arma <- mod_arma$residuals/sqrt(mod_arma$sigma2)

# 参考
residuals(mod_arma_garch, standardize = T)


# データをまとめる
d <- data.frame(
  arma_garch = resid_arma_garch,
  arma = resid_arma
)

# 図示
autoplot(ts(d), facets = T, ylab = "", main = "標準化残差")

# RによるGJRモデル -----------------

# GJRの対象となるデータ
data(spyreal)
head(spyreal, n=2)


# モデルの構造の指定
spec_rugarch3 <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
  mean.model     = list(armaOrder = c(1, 1)), 
  distribution.model = "std"
)

# GJR GARCHの推定
mod_gjr <- ugarchfit(
  spec = spec_rugarch3, data = spyreal[,1], solver='hybrid'
)

# 結果の表示
coef(mod_gjr)


# 普通のGARCHモデルの作成
spec_rugarch4 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
  mean.model     = list(armaOrder = c(1, 1)), 
  distribution.model = "std"
)
# モデルの推定
mod_standard_garch <- ugarchfit(
  spec = spec_rugarch4, data = spyreal[,1], solver='hybrid'
)

# 情報量規準を比較
infocriteria(mod_gjr)["Akaike",]
infocriteria(mod_standard_garch)["Akaike",]

# ボラティリティ
sigma(mod_gjr)

# データをまとめる
d_xts <- spyreal[,1]
d_xts$volatility <- sigma(mod_gjr)

# 図示
autoplot(d_xts, facets = T, ylab = "")

# 予測
pred <- ugarchboot(mod_gjr, n.ahead = 50, method = "Partial")
pred
plot(pred, which = 2)

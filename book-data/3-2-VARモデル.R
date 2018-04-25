
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第3部 2章
# VARモデル



# この章で使うパッケージ --------------------

install.packages("urca")
install.packages("fpp")
install.packages("vars")
install.packages("ggplot2")
install.packages("ggfortify")

library(urca)
library(fpp)
library(vars)
library(ggplot2)
library(ggfortify)


# 分析の対象となるデータ -----------------
# アメリカの個人消費・個人収入の増加率データ
usconsumption

# 図示
autoplot(usconsumption, facets = T)

# 単位根検定
# 消費の単位根検定
summary(ur.df(usconsumption[, "consumption"], type = "drift"))
# 収入の単位根検定
summary(ur.df(usconsumption[, "income"], type = "drift"))

# 相互相関係数
autoplot(
  ccf(
    usconsumption[, "consumption"],
    usconsumption[, "income"],
    plot = F
  )
)



# RによるVARモデル -----------------

# 次数の選択
select_result <- VARselect(usconsumption, lag.max = 10, type="const")
select_result

select_result$selection[1]


# モデル化
var_bestorder <- VAR(
  y = usconsumption,
  type = "const",
  p = select_result$selection[1]
)

summary(var_bestorder)



# 予測 -----------------
predict(var_bestorder, n.ahead = 4)

# 予測結果の図示
autoplot(
  predict(var_bestorder, n.ahead = 8), 
  ts.colour = 1,
  predict.colour = 1, 
  predict.linetype = 'dashed'
)


# Grangerの因果性検定 -----------------

# 収入が消費に与える影響
causality(var_bestorder, cause = "income")

# 消費が収入に与える影響
causality(var_bestorder, cause = "consumption")


# インパルス応答関数 -----------------

# インパルス応答関数を求める
irf_consumption <- irf(
  var_bestorder, 
  impulse = "consumption", 
  response = c("consumption", "income"),
  n.ahead = 12, 
  boot = T
)

# インパルス応答関数の図示
plot(irf_consumption)


# 分散分解
plot(fevd(var_bestorder, n.ahead = 12))






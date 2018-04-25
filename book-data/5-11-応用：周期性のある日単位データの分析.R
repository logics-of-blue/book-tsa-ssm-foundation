
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第5部 11章
# 応用：周期性のある日単位データの分析



# この章で使うパッケージ --------------------
install.packages("KFAS")
install.packages("xts")
install.packages("Nippon")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("gridExtra")


library(KFAS)
library(xts)
library(Nippon)
library(ggplot2)
library(ggfortify)
library(gridExtra)

# データの読み込みと整形 --------------------

# ファイルの読み込み
file_data <- read.csv("5-11-sales_data.csv")
head(file_data, n = 3)

# xts型に変換
sales <- as.xts(read.zoo(file_data))
head(sales, n = 3)

# 図示
autoplot(sales, main = "架空の売り上げデータ")


# 祝日の取り扱い --------------------

# 日付の抽出
dates <- index(sales)
head(dates, n = 5)

# 祝日の判定
head(is.jholiday(dates))

# 3月21日は日曜日で、3月22日が振り替え休日
dates[is.jholiday(dates)]

# 曜日
weekdays(dates[is.jholiday(dates)], T)

# 日曜日は取り除く
holiday_date <- 
  dates[is.jholiday(dates) & (weekdays(dates, T) != "日")]
holiday_date

# 祝日フラグ
holiday_flg <- as.numeric(dates %in% holiday_date)
holiday_flg


# KFASによる基本構造時系列モデル --------------------

# Step1：モデルの構造を決める
build_cycle <- SSModel(
  H = NA,
  as.numeric(sales) ~ 
    SSMtrend(degree = 2, c(list(NA), list(NA))) +
    SSMseasonal(period = 7, sea.type = "dummy",  Q = NA) +
    holiday_flg
)

# Step2 パラメタ推定
fit_cycle <- fitSSM(build_cycle, inits = c(1, 1, 1, 1))

# Step3、4：フィルタリング・スムージング
result_cycle <- KFS(
  fit_cycle$model, 
  filtering = c("state", "mean"),
  smoothing = c("state", "mean")
)


# 推定結果の確認 --------------------

# 平滑化状態や周期成分の図示
p_data <- autoplot(sales, main = "元データ")
p_trend <- autoplot(
  result_cycle$alphahat[, "level"], main = "トレンド＋水準")
p_cycle <- autoplot(
  result_cycle$alphahat[, "sea_dummy1"], main = "周期成分")

grid.arrange(p_data, p_trend, p_cycle)


# 推定結果の図示 --------------------

# 予測区間と平滑化状態
interval_cycle <- predict(
  fit_cycle$model, interval = "prediction", level = 0.95)

# データを整形
df <- cbind(
  data.frame(sales = as.numeric(sales), 
             time = as.POSIXct(index(sales))), 
  as.data.frame(interval_cycle)
)

# 図示
ggplot(data = df, aes(x = time, y = sales)) +
  labs(title="周期成分のある状態空間モデル") +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = fit), size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  scale_x_datetime(date_labels = "%y年%m月")


# 周期成分を取り除く --------------------

# 予測区間と平滑化状態
interval_level <- predict(
  fit_cycle$model, interval = "confidence", 
  level = 0.95, states = "level")

# データを整形
df_level <- cbind(
  data.frame(sales = as.numeric(sales), 
             time = as.POSIXct(index(sales))), 
  as.data.frame(interval_level)
)

# 図示
ggplot(data = df_level, aes(x = time, y = sales)) +
  labs(title="周期成分を取り除いた水準値のグラフ") +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = fit), size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  scale_x_datetime(date_labels = "%y年%m月")











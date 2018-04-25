
# 時系列分析と状態空間モデルの基礎：RとStanで学ぶ理論と実装
# 第2部 6章
# Rによる時系列データの取り扱い



# コメント -----------------
# コメントにハイフン「-」を4つ以上つけると、
# 次のハイフンが4つつながっているコメントまでを
# 折りたたむことができます（RStudioの機能）。


# 四則演算 -----------------
1 + 1
4 - 2
2 * 3
6 / 4


# 変数 -----------------
a <- 3
a + 1


# 関数とヘルプ -----------------
sqrt(4)

hensu <- 2
sqrt(hensu)

# 関数の定義
plusOne <- function(x){
  return(x + 1)
}

plusOne(5)

# ヘルプ
?sqrt


# ベクトル -----------------
vec <- c(1,3,5,6)
vec

# 等差数列
2:7

# 要素の抽出
vec[2]
vec[1:2]

# ベクトルの演算
vec + 2


# 行列 --------------------
mat <- matrix(
  c(1,2,3,4,5,6,7,8),
  nrow = 4
)
mat

# 変数の型
class(mat)

# 列数の指定
matrix(
  c(1,2,3,4,5,6,7,8),
  ncol = 4
)

# 用単位でデータを格納する
mat_byrow <- matrix(
  c(1,2,3,4,5,6,7,8),
  ncol = 4,
  byrow = T
)
mat_byrow

# 要素の抽出
mat_byrow[2,1]

# 行名と列名の設定
mat_with_name <- matrix(
  c(1,2,3,4,5,6,7,8),
  ncol = 4,
  byrow = T,
  dimnames = list(c("row1","row2"), c("col1","col2","col3","col4"))
)
mat_with_name


# データフレーム --------------------
dataf <- data.frame(
  X = c(1,2,3,4),
  Y = c("A", "B", "C", "D") 
)
dataf
# 変数の型
class(dataf)

# 列数
ncol(dataf)

# 行数
nrow(dataf)

# 要素の抽出
dataf$X
dataf$X[1]

dataf[1,1]

# 行列からデータフレームへの変換
dataf_by_mat <- as.data.frame(mat)
dataf_by_mat
class(dataf_by_mat)

# 逆にデータフレームを行列に変換
as.matrix(dataf)

# リスト --------------------
li <- list(
  dataf = dataf,
  mat = mat
)
li
# 変数の型
class(li)

# 要素の抽出
li$dataf
class(li$dataf)

li[[1]]


# パッケージのインストール --------------------
install.packages("xts")
install.packages("forecast")
install.packages("urca")
install.packages("ggplot2")
install.packages("ggfortify")

library(xts)
library(forecast)
library(urca)
library(ggplot2)
library(ggfortify)


# 時系列データ ts --------------------
ts_sample <- ts(1:36, start=c(2000,1), freq=12)
ts_sample

# 四半期データ
ts_freq4 <- ts(c(1,4,7,3,9,2,5,3), start=c(2000,1), freq=4)
ts_freq4

# 多変量時系列データ
ts_multi <- ts(mat_with_name, start=c(2000,1), freq=12)
ts_multi

# 要素の抽出
window(ts_freq4, start=c(2000,2), end=c(2001,1))

# R組み込みのデータ
# イギリスの交通事故死傷者数
head(Seatbelts[,], n = 3)

# 特定の列のみ抽出
Seatbelts[, "front"]

# 複数の列を抽出
Seatbelts[, c("front", "PetrolPrice")]

# 特定の月のみ抽出
subset(Seatbelts[, "front"], month = 3)

# 時系列データ xts --------------------
xts_sample <- as.xts(matrix(
  c(1,2,3,4,5),
  dimnames = list(
    c("2000-01-01","2000-01-02","2000-01-03","2000-01-04","2000-01-05")
  ),
  ncol = 1
))
xts_sample

# 要素の抽出
xts_sample["2000-01-01"]

# ある日付以降
xts_sample["2000-01-02::"]

# 範囲指定
xts_sample["2000-01-02::2000-01-04"]


# ファイルからのデータ読み込み --------------------
file_data <- read.csv("C:/data/5-2-1-timeSeries.csv")
file_data

## その他のファイル読み込み方法
# 対話形式でファイルを選ぶ
file_data_2 <- read.csv(file.choose())
# Excelで開いたデータをクリップボードから読み込む
file_data_3 <- read.delim("clipboard")

# data.frame型で読み込まれている
class(file_data)


# xtsへの変換
# いったんread.zooをかませてから読み込む
file_data_xts <- as.xts(
  read.zoo(file_data)
)
file_data_xts


# 図示 --------------------
plot(
  Seatbelts[, "front"],
  main="イギリスの交通事故死傷者数(前席)",
  xlab="年",
  ylab="死傷者数"
)


# ggplot2を使った図示
autoplot(
  Seatbelts[, "front"],
  main="イギリスの交通事故死傷者数(前席)",
  xlab="年",
  ylab="死傷者数"
)


# 単位根検定 --------------------
# KPSS検定の実行
summary(ur.kpss(log(Seatbelts[, "front"])))

# 差分をとる回数を調べる
ndiffs(log(Seatbelts[, "front"]))





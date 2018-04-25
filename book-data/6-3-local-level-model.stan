data {
  int n_sample;       // サンプルサイズ
  real y[n_sample];   // 観測値
}

parameters {
  real mu_zero;       // 状態の初期値
  real mu[n_sample];  // 状態の推定値
  real<lower=0> s_w;  // 過程誤差の分散
  real<lower=0> s_v;  // 観測誤差の分散
}

model {
  // 状態の初期値から最初の時点の状態が得られる
  mu[1] ~ normal(mu_zero, sqrt(s_w));  
  
  // 状態方程式に従い、状態が遷移する
  for(i in 2:n_sample) {
    mu[i] ~ normal(mu[i-1], sqrt(s_w));
  }
  
  // 観測方程式に従い、観測値が得られる
  for(i in 1:n_sample) {
    y[i] ~ normal(mu[i], sqrt(s_v));
  }
}

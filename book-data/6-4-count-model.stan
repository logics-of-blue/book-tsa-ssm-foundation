data {
  int n_sample;            // サンプルサイズ
  int catch_y[n_sample];   // 捕獲数
  int y[n_sample];         // 観測値
}

parameters {
  real trend;              // 個体数増加トレンド
  real coef_catch_y;       // 捕獲数がトレンドに与える影響
  real mu_zero;            // 状態の初期値
  real mu[n_sample];       // 状態の推定値
  real mu_noise[n_sample]; // 観測誤差の入った状態の推定値
  real<lower=0> s_w;       // 過程誤差の分散
  real<lower=0> s_v;       // 観測誤差の分散
}

transformed parameters{
  real delta[n_sample];    // 捕獲の影響が入ったトレンド
  real lambda[n_sample];   // ポアソン分布の期待値

  for(i in 1:n_sample){
    delta[i] = trend - coef_catch_y * catch_y[i];
  }
  
  for(i in 1:n_sample){
    lambda[i] = exp(mu_noise[i]);
  }
}

model {
  // 状態の初期値から最初の時点の状態が得られる
  mu[1] ~ normal(mu_zero, sqrt(s_w));  
  
  // 状態方程式に従い、状態が遷移する
  for (i in 2:n_sample){
    mu[i] ~ normal(mu[i - 1] + delta[i - 1], sqrt(s_w));
  }
  
  // 観測誤差が加わる
  for(i in 1:n_sample){
    mu_noise[i] ~ normal(mu[i], sqrt(s_v));
  }
  
  //  ポアソン分布に従って観測値が得られる
  for(i in 1:n_sample){
    y[i] ~ poisson(lambda[i]);
  }
}

generated quantities{
  real lambda_smooth[n_sample];       // 観測誤差の無い、個体数の期待値
  real best_catch_y;                  // 個体数の増減がない捕獲量
  
  for(i in 1:n_sample){
    lambda_smooth[i] = exp(mu[i]);
  }
  
  best_catch_y = trend / coef_catch_y;
}

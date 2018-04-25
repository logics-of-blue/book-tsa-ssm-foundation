data {
  int n_sample;            // サンプルサイズ
  int y[n_sample];         // 観測値
}

parameters {
  real<lower=0> r;         // 内的自然増加率
  real<lower=0> K;         // 環境収容力
  real mu_zero;            // 状態の初期値
  real mu[n_sample];       // 状態の推定値
  real<lower=0> s_w;       // 過程誤差の分散
}

transformed parameters{
  real<lower=0> lambda[n_sample];   // ポアソン分布の期待値

  for(i in 1:n_sample){
    lambda[i] = exp(mu[i]);
  }
}

model {
  // 状態の初期値から最初の時点の状態が得られる
  mu[1] ~ normal(mu_zero, sqrt(s_w));  
  
  // 状態方程式に従い、状態が遷移する
  for (i in 2:n_sample){
    mu[i] ~ normal(mu[i-1] + r*mu[i-1]*(1 - mu[i-1]/K), sqrt(s_w));
  }
  
  //  ポアソン分布に従って観測値が得られる
  for(i in 1:n_sample){
    y[i] ~ poisson(lambda[i]);
  }
  
  // 弱情報事前分布。Kはおよそ4～10の範囲を想定。
  K ~ normal(7, 3);
}

generated quantities{
  real mu_smooth[n_sample];     // 過程誤差の無い、状態の推定値
  real lambda_smooth[n_sample]; // 過程誤差の無い、個体数の期待値
  
  mu_smooth[1] = mu_zero;
  
  for (i in 2:(n_sample)){
    mu_smooth[i] = mu_smooth[i-1] + r*mu_smooth[i-1]*(1 - mu_smooth[i-1]/K);
  }
  
  for(i in 1:(n_sample)){
    lambda_smooth[i] = exp(mu_smooth[i]);
  }
}

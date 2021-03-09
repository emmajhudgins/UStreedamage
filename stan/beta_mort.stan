  data {

    real oneT; 
    real threeT; 
    real fiveT; 
    real eightT; 

    int subsubone_spp;
    int subone_spp; 
    int one_spp;
    int three_spp;
    int five_spp;
    int eight_spp;
    int nine_spp;
    int ten_spp;

  }
parameters {
  real<lower=0> shape; 
  real<lower=0> scale;
  real<lower=0.95, upper=1> nineT;
  real<lower=0.0000001,upper=0.01> suboneT; 
  real<lower=0,upper=suboneT> subsuboneT; 
 
}
model { 
target += -log(shape)-log(scale)+(subsubone_spp)*beta_lcdf(subsuboneT|shape, scale)+(subone_spp)*log(exp(beta_lccdf(subsuboneT | shape, scale))-exp(beta_lccdf(suboneT | shape, scale)))+(one_spp)*log(exp(beta_lccdf(suboneT | shape, scale))-exp(beta_lccdf(oneT | shape, scale)))+three_spp*log(exp(beta_lccdf(oneT | shape, scale))-exp(beta_lccdf(threeT | shape, scale)))+five_spp*log(exp(beta_lccdf(threeT | shape, scale))-exp(beta_lccdf(fiveT | shape, scale)))+eight_spp*log(exp(beta_lccdf(fiveT | shape, scale))-exp(beta_lccdf(eightT | shape, scale)))+nine_spp*log(exp(beta_lccdf(eightT | shape, scale))-exp(beta_lccdf(nineT | shape, scale)))+ten_spp*beta_lccdf(nineT | shape, scale);
}
generated quantities {
  real log_lik;
  log_lik = (subone_spp)*beta_lcdf(suboneT|shape, scale)+(one_spp)*log(exp(beta_lccdf(suboneT | shape, scale))-exp(beta_lccdf(oneT | shape, scale)))+three_spp*log(exp(beta_lccdf(oneT | shape, scale))-exp(beta_lccdf(threeT | shape, scale)))+five_spp*log(exp(beta_lccdf(threeT | shape, scale))-exp(beta_lccdf(fiveT | shape, scale)))+eight_spp*log(exp(beta_lccdf(fiveT | shape, scale))-exp(beta_lccdf(eightT | shape, scale)))+nine_spp*log(exp(beta_lccdf(eightT | shape, scale))-exp(beta_lccdf(nineT | shape, scale)))+ten_spp*beta_lccdf(nineT | shape, scale);
}

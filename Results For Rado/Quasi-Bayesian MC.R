

library(MASS)
library(lme4)
library(tictoc)

expit = function(x){
  return(1/(1 + exp(-x)))
}

logit = function(x){
  return(log(x/(1-x)))
}

tic()


# Set parameters

num_reps = 200 # Number of times to simulate from the quasi-posterior
B = 100  # Number of datasets to generate for checking coverage probabilities

## Model parameters

all_ns = c(100, 200, 500, 1000, 2000) # Sample size in each group
all_Ks = c(5, 10, 20, 50)             # Number of groups

# n = 500 # Sample size in each group
# K = 5   # Number of groups
p_conf = 3  # Number of confounders

### Fixed effects
a_0 = 0
a_1 = 1
A_2 = rep(1, times = p_conf)

b_0 = -0.5
b_1 = 1
b_2 = 1
B_3 = rep(1, times = p_conf)

### Random effects
sigma_a_0 = 0.2
sigma_a_1 = 0.2 * abs(a_1)
cor_a0_a1 = 0.2
cov_a0_a1 = sigma_a_0 * sigma_a_1 * cor_a0_a1
Sigma_a = matrix(c(sigma_a_0^2, cov_a0_a1, cov_a0_a1, sigma_a_1^2), nrow = 2, ncol = 2)

sigma_b_0 = 0.2 * abs(b_0)
sigma_b_1 = 0.2 * abs(b_1)
cor_b0_b1 = 0.2
cov_b0_b1 = sigma_b_0 * sigma_b_1 * cor_b0_b1
Sigma_b = matrix(c(sigma_b_0^2, cov_b0_b1, cov_b0_b1, sigma_b_1^2), nrow = 2, ncol = 2)



data_coverage = data.frame()


for(K in all_Ks){
  for(n in all_ns){
    if(n*K <= 20000){
      print(paste0("k = ", k, ", n = ", n))
      
all_de_CIs = list()
all_ie_CIs = list()
all_te_CIs = list()

for(i in 1:B){
  print(paste0(i, " of ", B))
  
  ## Generate data
  all_Xs = list()
  all_Ws = list()
  
  for(k in 1:K){
    X = rnorm(n, mean=0, sd=1)
    W = matrix(rnorm(n*p_conf, mean=0, sd=1), nrow = n, ncol = p_conf)
    
    all_Xs[[k]] = X
    all_Ws[[k]] = W
  }
  
  ### Generate M
  all_Ms = list()
  for(k in 1:K){
    eta_vec_fixed = a_0 + a_1*all_Xs[[k]] + all_Ws[[k]]%*%A_2
    
    ## Add random effects
    a_ran = mvrnorm(1, mu = rep(0, 2), Sigma = Sigma_a)
    eta_vec = eta_vec_fixed + a_ran[1] + a_ran[2]*all_Xs[[k]]
    
    ## Generate M
    p_M_vec = expit(eta_vec)
    M = rbinom(n, size = 1, prob = p_M_vec)
    all_Ms[[k]] = M
  }
  
  ### Generate Y
  all_Ys = list()
  for(k in 1:K){
    zeta_vec_fixed = b_0 + b_1*all_Ms[[k]] + b_2 * all_Xs[[k]] + all_Ws[[k]]%*%B_3
    
    ##### Add random effects
    b_ran = mvrnorm(1, mu = rep(0, 2), Sigma = Sigma_b)
    zeta_vec = zeta_vec_fixed + b_ran[1] + b_ran[2]*all_Xs[[k]]
    
    #### Generate Y
    p_Y_vec = expit(zeta_vec)
    Y = rbinom(n, size = 1, prob = p_Y_vec)
    all_Ys[[k]] = Y
  }
  
  
  ### Consolidate groups
  X = do.call(c, all_Xs)
  W = do.call(rbind, all_Ws)
  M = do.call(c, all_Ms)
  Y = do.call(c, all_Ys)
  group = rep(1:K, each = n)
  
  data = data.frame(Y=Y, M=M, X=X, W1 = W[,1], W2 = W[,2], W3 = W[,3], group = group)
  
  
  
  
  ## Fit models
  fit_M = glmer(M ~ X + W1 + W2 + W3 + (1 + X | group), data = data, family = binomial)
  # summary(fit_M)
  
  fit_Y = glmer(Y ~ M + X + W1 + W2 + W3 + (1 + M + X | group), data = data, family = binomial)
  # summary(fit_Y)
  
  
  ## Quasi-posterior of estimators
  a_hat = fixef(fit_M)
  a_hat_cov = vcov(fit_M)
  
  b_hat = fixef(fit_Y)
  b_hat_cov = vcov(fit_Y)
  
  
  ## Simulate estimators
  all_a_tildes = sapply(1:num_reps, function(i){
    a_tilde = mvrnorm(1, mu = a_hat, Sigma = a_hat_cov)
    return(a_tilde)
  })
  
  all_b_tildes = sapply(1:num_reps, function(i){
    b_tilde = mvrnorm(1, mu = b_hat, Sigma = b_hat_cov)
    return(b_tilde)
  })
  
  
  ## Compute mediation effects
  all_de_tildes = exp(all_b_tildes[3,])
  all_ie_tildes = exp(all_b_tildes[2,] * all_a_tildes[2,])
  all_te_tildes = all_de_tildes * all_ie_tildes
  
  
  ## Construct CIs
  de_CI = quantile(all_de_tildes, c(0.025, 0.975))
  ie_CI = quantile(all_ie_tildes, c(0.025, 0.975))
  te_CI = quantile(all_te_tildes, c(0.025, 0.975))
  
  ## Store CIs
  all_de_CIs[[i]] = de_CI
  all_ie_CIs[[i]] = ie_CI
  all_te_CIs[[i]] = te_CI


}

# all_CIs_record = list()
# all_CIs_record[["k=5,n=1000"]] = list(all_de_CIs, all_ie_CIs, all_te_CIs)

# Check coverage

true_de = exp(b_2)
true_ie = exp(b_1 * a_1)
true_te = true_de * true_ie

de_covered = sapply(all_de_CIs, function(x) x[1] < true_de & x[2] > true_de)
ie_covered = sapply(all_ie_CIs, function(x) x[1] < true_ie & x[2] > true_ie)
te_covered = sapply(all_te_CIs, function(x) x[1] < true_te & x[2] > true_te)

de_cover_rate = mean(de_covered)
ie_cover_rate = mean(ie_covered)
te_cover_rate = mean(te_covered)


this_data_coverage = data.frame(n=n, K=K, de = de_cover_rate, ie = ie_cover_rate, te = te_cover_rate)
data_coverage = rbind(data_coverage, this_data_coverage)


    }
  }
}


toc()


library(dplyr)
data_coverage %>% group_by(n) %>% summarise(de = mean(de), ie = mean(ie), te = mean(te))

# save(data_coverage, file = "Results for Rado/data_coverage.RData")

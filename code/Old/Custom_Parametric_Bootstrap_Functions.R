

### Simulate from the provided glmer logistic regression fit. 
### Returns simulated responses and a data frame of coefficients used to generate those responses.
### (both fixed effects and mixed effects by group)
### Note: Country code AG corresponds to fixed (aggregate) effects.
my_sim_merMod = function(fit){
  # Generate random effects ----
  ## Extract RE SD matrix (i.e. Cholesky factor of RE covariance matrix)
  L = getME(fit, "Lambda")

  ## Simulate "spherical" REs (i.e. standard normal REs)
  S = rnorm(nrow(L))
  
  ## Compute actual REs
  ran_coef = L %*% S
  
  
  # Compute linear predictors ----
  ## Get design matrices for fixed and random parts of our model ----
  fix_X = getME(fit, "X")
  ran_X = getME(fit, "Z")
  
  ## Get estimated fixed effects ----
  fix_coef = fixef(fit)
  
  ## Compute contributions of fixed and random parts to linear predictor ----
  fix_component = fix_X %*% fix_coef
  ran_component = ran_X %*% ran_coef
  
  ## Compute linear predictor ----
  lin_pred = fix_component + ran_component
  
  
  # Generate response ----
  ## Compute probability for each observation ----
  ## (i.e. inverse link of linear predictor)
  all_resp_means = sapply(lin_pred, inv.logit)
  
  ## Simulate Ys ----
  all_resps = rbinom(length(all_resp_means), 1, all_resp_means)
  ### Double-check that simulated Ys make sense
  # mean(all_resps[as.numeric(lin_pred) > 0]) # Should be large
  # mean(all_resps[as.numeric(lin_pred) < 0]) # Should be small
  
  ## Convert to appropriate categorical values ----
  all_levels = c("all_or_most", "no_or_some")
  all_resps_categ = all_levels[all_resps + 1]
  
  
  # Construct output ----
  ## data frame of effects ----
  ### Get indices of variables with random effects
  names_fix = names(fix_coef)
  names_ran = colnames(ranef(fit)[[1]])
  ran_var_inds = match(names_ran, names_fix)
  
  ### Fixed effects ----
  all_mix_effs = tibble(boot_eff = unlist(fix_coef), country = rep("AG", times = length(fix_coef)))
  
  ### Mixed effects by country ----
  all_countries = levels(fit@frame$country)
  
  for(i in seq_along(all_countries)){
    #### Get random effects for this country ----
    this_mix_coef = fix_coef
    this_mix_coef[ran_var_inds] = this_mix_coef[ran_var_inds] + ranef(fit)[[1]][i,]
    this_mix_coef = unlist(this_mix_coef)
    
    #### Compute and store mixed effects for this country ----
    this_country = rep(all_countries[i], times = length(this_mix_coef))
    this_mix_eff_data = tibble(boot_eff = this_mix_coef, country = this_country)
    all_mix_effs = rbind(all_mix_effs, this_mix_eff_data)
  }
  
  ## Create and return list containing simulated Ys and bootstrap effects ----
  output = list(boot_resp = all_resps_categ, boot_effs = all_mix_effs)
  return(output)
  
}



### Performs one parametric bootstrap replicate for multilevel mediation analysis. 
### Returns fixed effects and mixed effects by country, both those used to generate
### the bootstrap sample and those predicted by models fit to the bootstrap sample.
### Note: Country code AG corresponds to fixed (aggregate) effects.
one_boot_rep = function(dat.ma, mod_Y, mod_M) {
  # Bootstrap M ----
  
  ## Generate bootstrap values for M ----
  boot_results_M = my_sim_merMod(mod_M)
  
  ## Store new M values in a data frame ----
  this_dat_M = dat.ma %>% mutate(q4.src = factor(boot_results_M$boot_resp))
  
  ## Fit models for M and Y to dataset with new M values ----
  ## Note: No interaction effect (i.e. Baron and Kenny style)
  boot_mod_M = glmer(
    q4.src ~ q8.pcis + q2.dc + q3.pc +
      age_group + gender + q7.la +  q9.edu +
      (q8.pcis | country),
    data = this_dat_M,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  boot_mod_Y_intermediate = glmer(
    q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
      age_group + gender + q7.la  + q9.edu +
      (q8.pcis + q4.src | country),
    data = this_dat_M,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  
  # Bootstrap Y ----
  
  ## Generate bootstrap responses ----
  boot_results_Y = my_sim_merMod(boot_mod_Y_intermediate)
  
  ## Store new Y values in a data frame ----
  this_dat_Y = this_dat_M %>% mutate(q5.fc = factor(boot_results_Y$boot_resp))
  
  ## Fit model for Y to dataset with new values for M and Y ----
  ## Note: No interaction effect (i.e. Baron and Kenny style)
  boot_mod_Y = glmer(
    q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
      age_group + gender + q7.la  + q9.edu +
      (q8.pcis + q4.src | country),
    data = this_dat_Y,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  # Fit models to bootstrap datasets ----
  ## With interaction ----
  # boot_mod_Y = glmer(
  #   q5.fc ~ q8.pcis + q4.src + q8.pcis * q4.src + q1.cc  + q2.dc + q3.pc +
  #     age_group + gender + q7.la  + q9.edu +
  #     (q8.pcis + q4.src + q8.pcis * q4.src | country),
  #   data = this_dat_Y,
  #   family = "binomial",
  #   control = glmerControl(optimizer = "bobyqa",
  #                          optCtrl = list(maxfun = 2e5))
  # )
  # boot_mod_M = glmer(
  #   q4.src ~ q8.pcis + q2.dc + q3.pc +
  #     age_group + gender + q7.la +  q9.edu +
  #     (q8.pcis | country),
  #   data = this_dat_M,
  #   family = "binomial",
  #   control = glmerControl(optimizer = "bobyqa",
  #                          optCtrl = list(maxfun = 2e5))
  # )
  ## Without interaction ----
  # boot_mod_Y = glmer(
  #   q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
  #     age_group + gender + q7.la  + q9.edu +
  #     (q8.pcis + q4.src | country),
  #   data = this_dat_Y,
  #   family = "binomial",
  #   control = glmerControl(optimizer = "bobyqa",
  #                          optCtrl = list(maxfun = 2e5))
  # )
  # boot_mod_M = glmer(
  #   q4.src ~ q8.pcis + q2.dc + q3.pc +
  #     age_group + gender + q7.la +  q9.edu +
  #     (q8.pcis | country),
  #   data = this_dat_M,
  #   family = "binomial",
  #   control = glmerControl(optimizer = "bobyqa",
  #                          optCtrl = list(maxfun = 2e5))
  # )
  
  # Extract fixed and mixed effects from bootstrap models ----
  fix_coef_Y = fixef(boot_mod_Y)
  mix_coef_Y = coef(boot_mod_Y)[[1]]
  fix_coef_M = fixef(boot_mod_M)
  mix_coef_M = coef(boot_mod_M)[[1]]
  
  # Construct data frames for both models containing all effects ----
  ## Fixed effects ----
  boot_data_Y = tibble(est_eff = fix_coef_Y, country = "AG")
  boot_data_M = tibble(est_eff = fix_coef_M, country = "AG")
  
  ## Mixed effects by country ----
  all_countries = unique(getME(mod_Y, "flist")[[1]])
  for (i in seq_along(all_countries)) {
    this_country = all_countries[i]
    
    this_mix_Y = unlist(mix_coef_Y[i, ])
    this_mix_M = unlist(mix_coef_M[i, ])
    
    this_data_Y = tibble(est_eff = this_mix_Y, country = this_country)
    this_data_M = tibble(est_eff = this_mix_M, country = this_country)
    
    boot_data_Y = rbind(boot_data_Y, this_data_Y)
    boot_data_M = rbind(boot_data_M, this_data_M)
  }
  
  
  # Combine estimated and "true" bootstrap effects ----
  all_effects_Y = boot_results_Y$boot_effs
  all_effects_Y$est_eff = boot_data_Y$est_eff
  
  all_effects_M = boot_results_M$boot_effs
  all_effects_M$est_eff = boot_data_M$est_eff
  
  
  # Combine results into output and return ----
  output = list(Y_effs = all_effects_Y, M_effs = all_effects_M)
  return(output)
}


### Perform B bootstrap replicates of multilevel models for mediation analysis, optionally in parallel.
### If .parallel = T, a parallel backend must have already been registered and all objects passed to worker nodes.
all_boot_reps = function(B, dat.ma, mod_Y, mod_M, .parallel = F){
  
  ### Initialize Progress Bar ----
  prog = txtProgressBar(max = B, style = 3)
  prog_update = function(n)
    setTxtProgressBar(prog, n)
  DoSNOW_opts = list(progress = prog_update)
  
  
  
  # Run analysis ----
  if (!.parallel) {
    ## Serial ----
    all_boot_results = foreach(i = seq_len(B)) %do% {
      set.seed(i * 1000)
      
      this_boot_results = one_boot_rep(dat.ma, mod_Y, mod_M)
      this_boot_results$Y_effs$boot_rep = i
      this_boot_results$M_effs$boot_rep = i
      
      prog_update(i)
      
      return(this_boot_results)
    }
  } else{
    ## Parallel ----
    all_boot_results = foreach(i = seq_len(B), .options.snow = DoSNOW_opts) %dopar% {
      set.seed(i * 1000)
      
      this_boot_results = one_boot_rep(dat.ma, mod_Y, mod_M)
      this_boot_results$Y_effs$boot_rep = i
      this_boot_results$M_effs$boot_rep = i
      
      return(this_boot_results)
    }
  }
  
  # Consolidate results into one tibble per model ----
  all_boot_results_Y = tibble()
  all_boot_results_M = tibble()
  
  for(i in seq_len(B)){
    all_boot_results_Y = rbind(all_boot_results_Y, all_boot_results[[i]]$Y_effs)
    all_boot_results_M = rbind(all_boot_results_M, all_boot_results[[i]]$M_effs)
  }
  
  output = list(results_Y = all_boot_results_Y, results_M = all_boot_results_M)
  return(output)
}
  

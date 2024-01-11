


### Performs one parametric bootstrap replicate for multilevel mediation analysis. 
### Returns fixed effects and mixed effects by country, both those used to generate
### the bootstrap sample and those predicted by models fit to the bootstrap sample.
### Note: Country code AG corresponds to fixed (aggregate) effects.
one_boot_rep = function(dat.ma) {
  
  # Generate bootstrap sample ----
  data_by_country = split(dat.ma, dat.ma$country)
  
  boot_data = tibble()
  for(this_data in data_by_country){
    this_boot_data = slice_sample(this_data, replace=T, prop=1) # Sample rows with replacement
    boot_data = rbind(boot_data, this_boot_data)
  }
  
  # Fit models to bootstrap sample ----
  boot_mod_M = glmer(
    q4.src ~ q8.pcis + q2.dc + q3.pc +
      age_group + gender + q7.la +  q9.edu +
      (q8.pcis | country),
    data = boot_data,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  boot_mod_Y = glmer(
    q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
      age_group + gender + q7.la  + q9.edu +
      (q8.pcis + q4.src | country),
    data = boot_data,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  
  
  
  
  
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
  all_countries = unique(dat.ma$country)
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
  # all_effects_Y =   boot_data_Y[,"est_eff"]
  all_effects_Y =   boot_data_Y
  
  # all_effects_M = boot_data_M[,"est_eff"]
  all_effects_M = boot_data_M
  
  
  # Combine results into output and return ----
  output = list(Y_effs = all_effects_Y, M_effs = all_effects_M)
  return(output)
}


### Perform B bootstrap replicates of multilevel models for mediation analysis, optionally in parallel.
### If .parallel = T, a parallel backend must have already been registered and all objects passed to worker nodes.
all_boot_reps = function(B, dat.ma, .parallel = F){
  
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
      
      this_boot_results = one_boot_rep(dat.ma)
      this_boot_results$Y_effs$boot_rep = i
      this_boot_results$M_effs$boot_rep = i
      
      prog_update(i)
      
      return(this_boot_results)
    }
  } else{
    ## Parallel ----
    all_boot_results = foreach(i = seq_len(B), .options.snow = DoSNOW_opts) %dopar% {
      set.seed(i * 1000)
      
      this_boot_results = one_boot_rep(dat.ma)
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


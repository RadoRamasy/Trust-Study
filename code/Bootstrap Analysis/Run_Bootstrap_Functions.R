##################
### Parametric ###
##################


### A note about my_sim_merMod_par: I include the option to supply parameter estimates instead of extracting them from 
### the fitted model. This is useful when M has been replaced with a bootstrap sample in the fitted lme4 object, but we want 
### to generate a bootstrap sample for Y using the parameter estimates from the original dataset. 


### Simulate from the provided glmer logistic regression fit. 
### Returns simulated responses and a data frame of coefficients used to generate those responses.
### (both fixed effects and mixed effects by group)
### Optionally supply the fixed and random effects for simulation. See above note for typical use case.
### Note: Country code AG corresponds to fixed (aggregate) effects.
my_sim_merMod_par = function(fit, fix_coef = NULL, ran_coef_SD_mat = NULL){
  names_fix = names(fixef(fit))
  names_ran = colnames(ranef(fit)[[1]])
  
  all_countries = unique(fit@frame$country) # fit@frame is the data frame used by glmer to fit the model
  
  # Generate random effects ----
  ## Extract RE SD matrix (i.e. Cholesky factor of RE covariance matrix)
  if(is.null(ran_coef_SD_mat)){
    L = getME(fit, "Lambda")
  } else{
    L = ran_coef_SD_mat
  }
  
  ## Simulate "spherical" REs (i.e. standard normal REs)
  S = rnorm(nrow(L))
  
  ## Compute actual REs
  num_REs = length(S) / length(all_countries)
  ran_coef_raw = as.numeric(L %*% S)
  ran_coef = tibble(val = ran_coef_raw, group = rep(all_countries, each = num_REs))
  
  
  # Compute linear predictors ----
  ## Get design matrices for fixed and random parts of our model ----
  fix_X = getME(fit, "X")
  ran_X = getME(fit, "Z")
  
  
  ## Get estimated fixed effects ----
  if(is.null(fix_coef)){
    fix_coef = fixef(fit)
  }
  
  ## Compute contributions of fixed and random parts to linear predictor ----
  fix_component = fix_X %*% fix_coef
  ran_component = ran_X %*% ran_coef_raw
  
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
  ran_var_inds = match(names_ran, names_fix)  # Get indices of names_ran in names_fix
  
  ### Fixed effects ----
  all_mix_effs = tibble(boot_eff = unlist(fix_coef), country = rep("AG", times = length(fix_coef)), pred = names_fix)
  
  ### Mixed effects by country ----
  
  for(i in seq_along(all_countries)){
    this_country_name = all_countries[i]
    
    #### Get random effects for this country ----
    all_ranefs = ranef(fit)[[1]]
    this_country_ind = which(rownames(all_ranefs) == this_country_name) # Account for order of countries in all_countries not matching order of countries in all_ranefs
    this_ranefs = all_ranefs[this_country_ind,]
    
    
    #### Construct mixed effects for this country ----
    this_mix_coef = fix_coef
    this_mix_coef[ran_var_inds] = this_mix_coef[ran_var_inds] + this_ranefs
    this_mix_coef = unlist(this_mix_coef)
    
    #### Compute and store mixed effects for this country ----
    this_country = rep(all_countries[i], times = length(this_mix_coef))
    this_mix_eff_data = tibble(boot_eff = this_mix_coef, country = this_country, pred = names_fix)
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
one_boot_rep_par = function(dat.ma, mod_Y, mod_M) {
  # Bootstrap M ----
  
  ## Generate bootstrap values for M ----
  boot_results_M = my_sim_merMod_par(mod_M)
  
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

  # Fit a new model for Y using the bootstrap values for M
  # This will get combined with the coefficient vector/matrix from mod_Y for bootstrapping Y
  # This approach is very inefficient (fitting 1 un-necessary GLMM), but I expect it to work.
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
  ### Get fitted coefficients from model fit to original dataset ----
  #### Fixed Effects ----
  fix_coef = fixef(mod_Y)
  
  #### Random Effects ----
  # Specifically, the SD matrix of the REs (i.e. Cholesky factor of RE covariance matrix)
  ran_coef_SD_mat = getME(mod_Y, "Lambda")
  
  ### Run simulation ----
  boot_results_Y = my_sim_merMod_par(boot_mod_Y_intermediate, fix_coef, ran_coef_SD_mat)
  
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
  
  
  
  
  # Extract fixed and mixed effects from bootstrap models ----
  fix_coef_Y = fixef(boot_mod_Y)
  mix_coef_Y = coef(boot_mod_Y)[[1]]
  fix_coef_M = fixef(boot_mod_M)
  mix_coef_M = coef(boot_mod_M)[[1]]
  
  # Construct data frames for both models containing all effects ----
  ## Fixed effects ----
  boot_data_Y = tibble(est_eff = fix_coef_Y, country = "AG", pred = names(fix_coef_Y))
  boot_data_M = tibble(est_eff = fix_coef_M, country = "AG", pred = names(fix_coef_M))
  
  ## Mixed effects by country ----
  all_countries = levels(mod_Y@frame$country) # mod_Y@frame is the data frame used by glmer to fit the model
                                              # Using levels() instead of unique() gets the order right
  for (i in seq_along(all_countries)) {
    this_country = all_countries[i]
    
    this_mix_Y = unlist(mix_coef_Y[i, ])
    this_mix_M = unlist(mix_coef_M[i, ])
    
    this_data_Y = tibble(est_eff = this_mix_Y, country = this_country, pred = names(this_mix_Y))
    this_data_M = tibble(est_eff = this_mix_M, country = this_country, pred = names(this_mix_M))
    
    boot_data_Y = rbind(boot_data_Y, this_data_Y)
    boot_data_M = rbind(boot_data_M, this_data_M)
  }
  
  
  # Combine estimated and "true" bootstrap effects ----
  # all_effects_Y = boot_results_Y$boot_effs
  # all_effects_Y$est_eff = boot_data_Y$est_eff
  all_effects_Y = full_join(boot_results_Y$boot_effs, boot_data_Y, by = c("country", "pred"))
  # all_effects_Y %>% group_by(pred) %>% summarise(A = sd(boot_eff), B = sd(est_eff)) # Check for presence of random effects
  
  
  # all_effects_M = boot_results_M$boot_effs
  # all_effects_M$est_eff = boot_data_M$est_eff
  all_effects_M = full_join(boot_results_M$boot_effs, boot_data_M, by = c("country", "pred"))
  # all_effects_M %>% group_by(pred) %>% summarise(A = sd(boot_eff), B = sd(est_eff)) # Check for presence of random effects
  
  
  # Combine results into output and return ----
  output = list(Y_effs = all_effects_Y, M_effs = all_effects_M)
  return(output)
}


### Perform B bootstrap replicates of multilevel models for mediation analysis, optionally in parallel.
### If .parallel = T, a parallel backend must have already been registered and all objects passed to worker nodes.
### .verbose controls whether or not a progress bar is printed
all_boot_reps_par = function(B, dat.ma, mod_Y, mod_M, .parallel = F, .verbose=T){
  
  if(.verbose){
    ### Initialize Progress Bar ----
    prog = txtProgressBar(max = B, style = 3)
    prog_update = function(n)
      setTxtProgressBar(prog, n)
    DoSNOW_opts = list(progress = prog_update)
  } else{
    DoSNOW_opts = list()
  }
  
  
  # Run analysis ----
  if (!.parallel) {
    ## Serial ----
    all_boot_results = foreach(i = seq_len(B)) %do% {
      set.seed(i * 1000)
      
      this_boot_results = one_boot_rep_par(dat.ma, mod_Y, mod_M)
      this_boot_results$Y_effs$boot_rep = i
      this_boot_results$M_effs$boot_rep = i
      
      if(.verbose){
        prog_update(i)
        cat("\n")
      }
      
      return(this_boot_results)
    }
  } else{
    ## Parallel ----
    all_boot_results = foreach(i = seq_len(B), .options.snow = DoSNOW_opts) %dopar% {
      set.seed(i * 1000)
      
      this_boot_results = one_boot_rep_par(dat.ma, mod_Y, mod_M)
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




######################
### Non-Parametric ###
######################



### Performs one non-parametric bootstrap replicate for multilevel mediation analysis. 
### Returns fixed effects and mixed effects by country. Unlike the parametric sampler, 
### here we only return estimated effects from the fitted models..
### Note: Country code AG corresponds to fixed (aggregate) effects.
one_boot_rep_npar = function(dat.ma) {
  
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
  boot_data_Y = tibble(est_eff = fix_coef_Y, country = "AG", pred = names(fix_coef_Y))
  boot_data_M = tibble(est_eff = fix_coef_M, country = "AG", pred = names(fix_coef_M))
  
  ## Mixed effects by country ----
  all_countries = levels(dat.ma$country)  # Using levels() instead of unique() gets the order right
  for (i in seq_along(all_countries)) {
    this_country = all_countries[i]
    
    this_mix_Y = unlist(mix_coef_Y[i, ])
    this_mix_M = unlist(mix_coef_M[i, ])
    
    this_data_Y = tibble(est_eff = this_mix_Y, country = this_country, pred = names(this_mix_Y))
    this_data_M = tibble(est_eff = this_mix_M, country = this_country, pred = names(this_mix_M))
    
    boot_data_Y = rbind(boot_data_Y, this_data_Y)
    boot_data_M = rbind(boot_data_M, this_data_M)
  }
  
  
  
  # Combine results into output and return ----
  output = list(Y_effs = boot_data_Y, M_effs = boot_data_M)
  return(output)
}


### Perform B bootstrap replicates of multilevel models for mediation analysis, optionally in parallel.
### If .parallel = T, a parallel backend must have already been registered and all objects passed to worker nodes.
### .verbose controls whether or not a progress bar is printed
all_boot_reps_npar = function(B, dat.ma, .parallel = F, .verbose = T){
  
  if(.verbose){
    ### Initialize Progress Bar ----
    prog = txtProgressBar(max = B, style = 3)
    prog_update = function(n)
      setTxtProgressBar(prog, n)
    DoSNOW_opts = list(progress = prog_update)
  } else{
    DoSNOW_opts = list()
  }
  
  
  # Run analysis ----
  if (!.parallel) {
    ## Serial ----
    all_boot_results = foreach(i = seq_len(B)) %do% {
      set.seed(i * 1000)
      
      this_boot_results = one_boot_rep_npar(dat.ma)
      this_boot_results$Y_effs$boot_rep = i
      this_boot_results$M_effs$boot_rep = i
      
      if(.verbose) prog_update(i)
      
      return(this_boot_results)
    }
  } else{
    ## Parallel ----
    all_boot_results = foreach(i = seq_len(B), .options.snow = DoSNOW_opts) %dopar% {
      set.seed(i * 1000)
      
      this_boot_results = one_boot_rep_npar(dat.ma)
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


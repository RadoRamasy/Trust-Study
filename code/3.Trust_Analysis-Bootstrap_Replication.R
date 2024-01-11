

### Construct a single bootstrap dataset based on dat.ma
get_boot_resamp = function(dat.ma) {
  all_countries = split(dat.ma, dat.ma$country_name)
  
  full_boot_sample = tibble()
  
  for (i in seq_along(all_countries)) {
    this_country = all_countries[[i]]
    this_n = nrow(this_country)
    boot_inds = sample(1:this_n, this_n, replace = T)
    
    this_resamp = this_country[boot_inds, ]
    full_boot_sample = rbind(full_boot_sample, this_resamp)
  }
  
  return(full_boot_sample)
  
}



# ### Generate B bootstrap re-samples, analyze and return output as a list indexed by bootstrap replicate
# ### Each replicate contains global mediation effects, country-specific mediation effects, and the number of failed model fits
# ### Analysis is performed serially (i.e. not in parallel)
# some_boot_analyses = function(dat.ma, B, job_number) {
#   ### Initialize Progress Bar ----
#   prog = txtProgressBar(max = B, style = 3)
#   prog_update = function(n)
#     setTxtProgressBar(prog, n)
#   
#   # Serial Bootstrap ----
#   
#   ### Initialize Progress Bar ----
#   prog = txtProgressBar(max = B, style = 3)
#   prog_update = function(n)
#     setTxtProgressBar(prog, n)
#   
#   bootstrap_output = foreach(b = 1:B) %do% {
#     this_seed = 10000*(job_number - 1) + 100*b
#     set.seed(this_seed)
#     
#     # Track how many samples needed to be re-drawn due to failed model fitting
#     num_failed_fits = 0
#     
#     # Attempt to perform mediation analysis on a bootstrap sample.
#     # If unsuccessful, increment a counter, generate a new sample, and try again.
#     success = F
#     while (!success) {
#       # Generate sample
#       boot_resamp = get_boot_resamp(dat.ma)
#       # boot_resamp = boot_resamp[c(1:100, 22201:22300),]
#       tryCatch({
#         # Run analysis
#         analysis_results = single_dataset_analysis(boot_resamp)
#         success = T
#       }, error = function(e) {
#         # Upon failure, increment counter and print error message
#         num_failed_fits = num_failed_fits + 1
#         cat("Error in fitting a mediation model:",
#             conditionMessage(e),
#             "\n")
#       })
#     }
#     
#     prog_update(b)
#     
#     return(list(analysis_results, num_failed_fits))
#   }
#   
#   return(bootstrap_output)
#   
#   
# }


### Generate B bootstrap re-samples, analyze and return output as a list indexed by bootstrap replicate
### Models fit using the lme4 package have built-in simulation routines. Hence, supply both models fit to the original dataset.
### Each replicate contains global mediation effects, country-specific mediation effects, and the number of failed model fits
### Analysis done in parallel using the provided SNOW cluster, cl.
some_boot_analyses_par = function(dat.ma, mod_Y, mod_M, B, cl, job_number) {
  
  # Parallel Bootstrap ----
  
  ### Initialize Progress Bar ----
  prog = txtProgressBar(max = B, style = 3)
  prog_update = function(n)
    setTxtProgressBar(prog, n)
  DoSNOW_opts = list(progress = prog_update)
  
  # Load any necessary packages on each worker node
  clusterEvalQ(cl, {
    library(tibble)
    library(lme4)
  })
  
  
  # Perform bootstrap analysis ----
  bootstrap_output = foreach(b = 1:B, .options.snow = DoSNOW_opts) %dopar% {
  # bootstrap_output = foreach(b = 1:B) %do% {
    this_seed = 10000*(job_number - 1) + 100*b
    set.seed(this_seed)
    
    # Track how many samples needed to be re-drawn due to failed model fitting
    num_failed_fits = 0
    
    # Attempt to perform mediation analysis on a bootstrap sample.
    # If unsuccessful, increment a counter, generate a new sample, and try again.
    success = F
    while (!success) {
      
      ## Naive non-parametric bootstrap (i.e. resampling pairs) ----
      # dat.ma_boot = get_boot_resamp(dat.ma)
      
      ## Parametric bootstrap. See Flores-Agreda and Cantoni (2019) ----
      # Generate samples for Y and M
      boot_Y = unlist(simulate(mod_Y))
      boot_M = unlist(simulate(mod_M))

      dat.ma_Y = dat.ma
      dat.ma_Y$q5.fc = boot_Y
      dat.ma_M = dat.ma
      dat.ma_M$q4.src = boot_M
      # 
      # # boot_resamp = boot_resamp[c(1:10000, 12201:22300),]
      # # boot_resamp = boot_resamp[c(1:100, 22201:22300),]
      tryCatch({
        # # Run analysis (non-parametric)
        # this_Y_coefs = get_Y_coefs(dat.ma_boot)
        # this_M_coefs = get_M_coefs(dat.ma_boot)
        
        # Run analysis (parametric)
        this_Y_coefs = get_Y_coefs(dat.ma_Y)
        this_M_coefs = get_M_coefs(dat.ma_M)
        success = T
      }, error = function(e) {
        # Upon failure, increment counter and print error message
        num_failed_fits = num_failed_fits + 1
        cat("Error in fitting a mediation model:",
            conditionMessage(e),
            "\n")
      })
    }
  
    this_mediation = get_mediation_effects(this_Y_coefs, this_M_coefs)
  
    return(list(this_mediation, num_failed_fits))
    
    
  }
  
  return(bootstrap_output)
}

consolidate_bootstrap_output = function(bootstrap_output_list) {
  boot_global_effects = data.frame()
  boot_country_effects = data.frame()
  
  total_failed_fits = 0
  
  for (b in 1:B) {
    boot_global_effects = rbind(boot_global_effects,
                                bootstrap_output_list[[b]][[1]]$global_effects)
    boot_country_effects = rbind(boot_country_effects,
                                 bootstrap_output_list[[b]][[1]]$country_effects)
    total_failed_fits = total_failed_fits + bootstrap_output_list[[b]][[2]]
  }
  
  colnames(boot_global_effects) = c("de", "ie", "te")
  
  
  output = list(
    global_effects = boot_global_effects,
    country_effects = boot_country_effects,
    failed_fits = total_failed_fits
  )
  return(output)
}



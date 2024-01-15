##################
### Parametric ###
##################





# Get CIs from this bootstrap run ----
get_par_boot_CIs = function(boot_results, mod_Y, mod_M) {
  boot_results_Y = boot_results$results_Y %>% dplyr::select(-boot_eff)
  boot_results_M = boot_results$results_M %>% dplyr::select(-boot_eff)
  
  true_boot_results_Y  = boot_results$results_Y %>% dplyr::select(-est_eff)
  true_boot_results_M  = boot_results$results_M %>% dplyr::select(-est_eff)
  
  Y_data = boot_results_Y
  M_data = boot_results_M
  
  # Compute all estimated mediation effects ----
  all_med_effs = get_many_med_effs(boot_results_Y, boot_results_M)
  all_med_effs_tidy = all_med_effs %>%
    pivot_longer(c(de, ie, te), names_to = "type", values_to = "value")
  
  # Compute true mediation effects ----
  all_true_med_effs = get_many_med_effs(true_boot_results_Y, true_boot_results_M)
  
  
  # Estimated mediation effects by country from original dataset ----
  
  all_base_med_effs = med_effs_from_lme4(mod_Y, mod_M)
  all_base_med_effs_tidy = all_base_med_effs %>%
    pivot_longer(c(de, ie, te), names_to = "type", values_to = "estimate")
  
  
  
  # Construct CIs ----
  ## Wald type ----
  data_Wald = full_join(all_med_effs, all_true_med_effs, by = c("country", "boot_rep"), suffix = c("_est", "_true"))
  Wald_SEPs = data_Wald %>%
    pivot_longer(!c(country, boot_rep), names_to = c("med", "type"), names_sep = "_") %>%
    pivot_wider(names_from = type, values_from = value) %>%
    rename(type = med) %>%
    group_by(country, type) %>%
    summarise(var = mean((est - true)^2)) %>%
    mutate(SEP = sqrt(var), .keep = "unused")
  
  Wald_CIs = full_join(all_base_med_effs_tidy, Wald_SEPs, by = c("country", "type")) %>%
    mutate(wald_lcl = estimate - 2*SEP, wald_ucl = estimate + 2*SEP)
  
  
  
  # Construct percentile bootstrap CIs ----
  
  boot_percentile_CIs = all_med_effs_tidy %>% group_by(country, type) %>%
    summarise(pct_lcl = quantile(value, 0.025),
              pct_ucl = quantile(value, 0.975))
  
  
  
  
  # Combine Wald and percentile intervals. Add basic bootstrap intervals.
  all_boot_CIs = full_join(Wald_CIs, boot_percentile_CIs) %>%
    mutate(bas_lcl = 2 * estimate - pct_ucl,
           bas_ucl = 2 * estimate - pct_lcl)
  
  
  
  
  
  
  return(all_boot_CIs)
  
  
  
}



######################
### Non-Parametric ###
######################



# Get CIs from this bootstrap run ----
get_npar_boot_CIs = function(boot_results, mod_Y, mod_M) {
  boot_results_Y = boot_results$results_Y
  boot_results_M = boot_results$results_M
  
  
  
  
  # Compute all mediation effects ----
  
  all_med_effs = get_many_med_effs(boot_results_Y, boot_results_M)
  all_med_effs_tidy = all_med_effs %>%
    pivot_longer(c(de, ie, te), names_to = "type", values_to = "value")
  
  
  # Estimated mediation effects by group from original dataset ----
  
  all_base_med_effs = med_effs_from_lme4(mod_Y, mod_M)
  all_base_med_effs_tidy = all_base_med_effs %>%
    pivot_longer(c(de, ie, te), names_to = "type", values_to = "estimate")
  
  
  
  # Construct percentile bootstrap CIs ----
  
  boot_percentile_CIs = all_med_effs_tidy %>% group_by(country, type) %>%
    summarise(pct_lcl = quantile(value, 0.025),
              pct_ucl = quantile(value, 0.975))
  
  
  
  
  # Combine estimates with percentile intervals. Add basic bootstrap intervals.
  all_boot_CIs = full_join(all_base_med_effs_tidy, boot_percentile_CIs) %>%
    mutate(bas_lcl = 2 * estimate - pct_ucl,
           bas_ucl = 2 * estimate - pct_lcl)
  
  
  
  
  
  
  return(all_boot_CIs)
}


# Helper Functions ----

# Compute mediation effects based on coefficients in Y- and M-models
get_med_effs_old = function(Y_coefs, M_coefs) {
  t1 = Y_coefs[2]
  t2 = Y_coefs[3]
  b1 = M_coefs[2]
  
  
  DE = exp(t1)
  IE = exp(b1 * t2)
  TE = exp(t1 + b1 * t2)
  
  # output = list(DE = DE, IE = IE, TE = TE)
  output = c(DE, IE, TE)
  names(output) = c("de", "ie", "te")
  return(output)
  
}


# Input: bootstrap coefficients for Y and M models. 
# Output: One data frame with a column for each response-predictor pair and a row for each country-boot_rep pair
# Inputs are data frames containing coefficients (*_eff), country, predictor name (pred), and bootstrap replicate number (boot_rep)
get_resp_coef_data = function(Y_data, M_data){
  coef_data = full_join(Y_data, M_data, by=c("country", "pred", "boot_rep"), suffix = c("_Y", "_M")) %>%        # Combine Y and M results into one dataset
    pivot_longer(matches("\\w+_eff_\\w+"), names_prefix = "\\w*_eff_", names_to = "resp") %>%                 # Create column indicating response variable. 
                                                                                                                  # Use matches() to regex search for columns containing effects
    mutate(pred = case_match(pred, "(Intercept)" ~ "Int", "q8.pcislow" ~ "X", "q4.srcno_or_some" ~ "M")) %>%   # Rename predictors to be more manageable
    filter(!is.na(value), !is.na(pred)) %>%                                                                                 # Remove na coefficients (these correspond to predictor = response = M)
    pivot_wider(names_from = c(resp, pred), values_from = value)                                              # Create column for each predictor-response combination
  
  return(coef_data)
}


# Computes mediation effects using bootstrap coefficients for Y and M models
# Inputs are data frames containing coefficients (*_eff), country, predictor name (pred), and bootstrap replicate number (boot_rep)
get_many_med_effs = function(Y_data, M_data){
  
  coef_data = get_resp_coef_data(Y_data, M_data)
  
  # Compute mediation effects 
  med_data = coef_data %>%
    mutate(country, boot_rep, de = exp(Y_X), ie = exp(Y_M + M_X), te = de * ie, .keep = "none")
  
  return(med_data)
}

# Input: Named vectors of coefficients from Y-model and M-model (e.g. from the fixef() function in lme4)
# Output: Data frame with one row and a column for each response-predictor pair
coef_vecs_2_data = function(Y_coefs, M_coefs){
  Y_coefs_data = Y_coefs %>% as_tibble(rownames = "pred")
  M_coefs_data = M_coefs %>% as_tibble(rownames = "pred")
  
  coef_data = full_join(Y_coefs_data, M_coefs_data, by = "pred", suffix = c("_Y", "_M")) %>%
    pivot_longer(c(value_Y, value_M), names_prefix = "value_", names_to = "resp") %>%            # Create column indicating response variable
    mutate(pred = case_match(pred, "(Intercept)" ~ "Int", "q8.pcislow" ~ "X", "q4.srcno_or_some" ~ "M")) %>%  # Rename variables to be more manageable
    filter(!is.na(value), !is.na(pred)) %>%                                                                    # Remove na coefficients (these correspond to predictor = response = M)
    pivot_wider(names_from = c(resp, pred), values_from = value)                                 # Create column for each predictor-response combination
  
  return(coef_data)
  
}

# Input: Data frame with one column for each response-predictor pair. May contain one row, or a row for each country and/or bootstrap replicate
# Output: Direct, indirect and total mediation effects for each row in Input. Retains and identifying columns from Input (e.g. country, boot_rep)
coef_data_2_med_effs = function(coef_data){
  med_data = coef_data %>%
    mutate(de = exp(Y_X), ie = exp(M_X + Y_M), te = de * ie) %>%
    dplyr::select(!matches("\\w{1}_\\w{1}"))
  
  return(med_data)
}

# Computes mediation effects for each country and aggregated across countries (i.e. based on mixed and fixed effects respectively).
med_effs_from_lme4 = function(mod_Y, mod_M){
  
  # Aggregate mediation effects ----
  fix_Y = fixef(mod_Y)
  fix_M = fixef(mod_M)
  
  fix_coef_data = coef_vecs_2_data(fix_Y, fix_M)
  
  fix_med_data = coef_data_2_med_effs(fix_coef_data) %>% mutate(country = "AG", .before=1)
  
  
  # country specific mediation effects ----
  mix_Y = coef(mod_Y)[[1]]
  mix_M = coef(mod_M)[[1]]
  
  all_countries = sort(unique(mod_Y@frame$country), decreasing = F)
  all_med_data = fix_med_data
  
  for(i in seq_along(all_countries)){
    this_country = all_countries[i]
    ind_this_country = which(rownames(mix_Y) == this_country) # Order of countries in all_countries might not match order of rows in mix_Y and mix_M
    this_mix_Y = unlist(mix_Y[ind_this_country,])
    this_mix_M = unlist(mix_M[ind_this_country,])
    
    this_med_data = coef_vecs_2_data(this_mix_Y, this_mix_M) %>%
      coef_data_2_med_effs() %>%
      mutate(country = this_country, .before = 1)
    
    all_med_data = rbind(all_med_data, this_med_data)
  }
  
  return(all_med_data)
  
}


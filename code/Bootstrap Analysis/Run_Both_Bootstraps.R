


# B = 1280 # Total number of bootstrap replicates
# B = 640 # Total number of bootstrap replicates
# B = 600 # Total number of bootstrap replicates
# B = 320 # Total number of bootstrap replicates
B = 300 # Total number of bootstrap replicates
# B = 90 # Total number of bootstrap replicates
# B = 64 # Total number of bootstrap replicates
# B = 60 # Total number of bootstrap replicates
# B = 3 # Total number of bootstrap replicates

pacman::p_load(readr,
               tidyverse,
               labelled,
               gtsummary,
               flextable,
               modelsummary,
               here,
               lme4,
               foreach,
               parallel,
               doSNOW,
               tictoc,
               boot)


print(format(Sys.time(), "%F %r %Z"))


# Load other scripts ----
source(here("code", "Bootstrap Analysis", "Bootstrap_CI_Helper_Functions.R"))
source(here("code", "Bootstrap Analysis", "Run_Bootstrap_Functions.R"))
source(here("code", "Bootstrap Analysis", "Analyse_Bootstrap_Functions.R"))



# Load Data ----
load(here("data","CleanDataFile-Trust-Nov102023.RData"))



# Fit models to observed data ----
## Baron and Kenny style models ----
# mod_Y = glmer(
#   q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
#     age_group + gender + q7.la  + q9.edu +
#     (q8.pcis + q4.src | country),
#   data = dat.ma,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",
#                          optCtrl = list(maxfun = 2e5))
# )
# mod_M  =  glmer(
#   q4.src ~ q8.pcis + q2.dc + q3.pc +
#     age_group + gender + q7.la +  q9.edu +
#     (q8.pcis | country),
#   data = dat.ma,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",
#                          optCtrl = list(maxfun = 2e5))
# )


# Load fitted models
# save(mod_M, mod_Y, file = here("code", "Bootstrap Analysis", "Fitted_Models.RData"))
load(here("code", "Bootstrap Analysis", "Output", "Fitted_Models.RData"))





# Construct dataset of fitted coefficients ----
# Note: Only includes those coefficients relevant to mediation analysis

# ## Fixed effects ----
# fixef_Y = fixef(mod_Y)
# fixef_M = fixef(mod_M)
# 
# all_fixefs = coef_vecs_2_data(fixef_Y, fixef_M) %>% mutate(country = "AG", .before=1)
# 
# ## Country specific coefficients ----
# mix_Y = coef(mod_Y)[[1]]
# mix_M = coef(mod_M)[[1]]
# 
# all_countries = levels(mod_Y@frame$country)
# all_effects_data = all_fixefs
# 
# for(i in seq_along(all_countries)){
#   this_country = all_countries[i]
#   ind_this_country = which(rownames(mix_Y) == this_country)
#   this_mix_Y = unlist(mix_Y[ind_this_country,])
#   this_mix_M = unlist(mix_M[ind_this_country,])
#   
#   this_effect_data = coef_vecs_2_data(this_mix_Y, this_mix_M) %>%
#     mutate(country = this_country, .before = 1)
#   
#   all_effects_data = rbind(all_effects_data, this_effect_data)
# }
# 
# 
# 
# # Construct dataset of fitted mediation effects ----
# all_med_effects_data_raw = matrix(0, nrow = nrow(all_effects_data), ncol = 3)
# for(i in seq_len(nrow(all_med_effects_data_raw))){
#   all_med_effects_data_raw[i,] = unlist(coef_data_2_med_effs(all_effects_data[i,-1]))
# }
# all_med_effects_data = tibble(country = all_effects_data$country)
# all_med_effects_data$de = all_med_effects_data_raw[,1]
# all_med_effects_data$ie = all_med_effects_data_raw[,2]
# all_med_effects_data$te = all_med_effects_data_raw[,3]

# save(all_effects_data, all_med_effects_data, file = here("code", "Bootstrap Analysis", "Estimated_Effects.RData"))
load(here("code", "Bootstrap Analysis", "Output", "Estimated_Effects.RData"))




# Initialize Cluster ----
# n_cores = B
n_cores = detectCores() - 1
# n_cores = detectCores()
my_cluster = makeCluster(n_cores)
registerDoSNOW(cl = my_cluster)
clusterEvalQ(my_cluster,{
  pacman::p_load(tibble, lme4, here, boot, dplyr)
  
  source(here("code", "Bootstrap Analysis", "Bootstrap_CI_Helper_Functions.R"))
  source(here("code", "Bootstrap Analysis", "Run_Bootstrap_Functions.R"))
  source(here("code", "Bootstrap Analysis", "Analyse_Bootstrap_Functions.R"))
  })
clusterExport(my_cluster, c("dat.ma", "mod_Y", "mod_M"))



# Run Analysis ----
# ## Serial ----
# tic()
# boot_results_par = all_boot_reps_par(B, dat.ma, mod_Y, mod_M, .parallel = F, .verbose = T)
# boot_results_npar = all_boot_reps_npar(B, dat.ma, .parallel = F, .verbose = T)
# cat("\n")
# toc()

## Parallel ----
tic()
print("Running parametric bootstrap.")
boot_results_par = all_boot_reps_par(B, dat.ma, mod_Y, mod_M, .parallel = T, .verbose = T)
toc()
tic()
print("Parametric bootstrap complete. Running non-parametric bootstrap.")
boot_results_npar = all_boot_reps_npar(B, dat.ma, .parallel = T, .verbose = T)
print("Non-parametric bootstrap complete.")
cat("\n")
toc()


stopCluster(my_cluster)


# save(boot_results_par, boot_results_npar, file = here("code", "Bootstrap Analysis", "Both_Boots_Real_Data.RData"))
load(here("code", "Bootstrap Analysis", "Output", "Both_Boots_Real_Data.RData"))


# Construct bootstrap CIs ----
boot_CIs_par = get_par_boot_CIs(boot_results_par, mod_Y, mod_M)
boot_CIs_npar = get_npar_boot_CIs(boot_results_npar, mod_Y, mod_M)


# Plot CIs ----

data_boot_CIs = boot_CIs_par %>%
  dplyr::select(-SEP) %>%
  full_join(boot_CIs_npar, by = c("country", "type", "estimate"), suffix = c("_par", "_npar")) %>%  # Consolidate datasets
  rename(wald_lcl_par = wald_lcl, wald_ucl_par = wald_ucl) %>%  # Add appropriate suffix to Wald intervals
  pivot_longer(!c(country, type, estimate), names_pattern = "(\\w+)_(\\w+)_(\\w+)", names_to = c("CI_type", "bound", "boot_type")) %>%  # Move grouping information to columns
  pivot_wider(names_from = bound, values_from = value) %>%  # Move interval endpoints to columns
  mutate(Interval_Type = paste0(boot_type, "-", CI_type))   # Consolidate bootstrap and interval type into single label

data_boot_CIs_plot = mutate(data_boot_CIs, is_AG = country=="AG") %>% rename(med_type = type)

## Forest plot of bootstrap CIs ----
plot_boot_CIs = ggplot(data_boot_CIs_plot, aes(y = country, group = country, color = is_AG)) + geom_point(aes(x = estimate)) +
  geom_linerange(aes(xmin = lcl, xmax = ucl)) + facet_grid(rows = vars(Interval_Type), cols = vars(med_type)) +
  geom_vline(xintercept = 1) + guides(color="none")







# Plot bootstrap distributions ----

## Regression coefficients ----

### Create appropriate data frame ----
data_M_par = boot_results_par$results_M %>%
  mutate(pred = case_match(pred, "q8.pcislow" ~ "X_in_M") )%>%
  filter(pred == "X_in_M")%>%
  dplyr::select(-boot_eff)
data_Y_par = boot_results_par$results_Y %>%
  mutate(pred = case_match(pred, "q8.pcislow" ~ "X_in_Y", "q4.srcno_or_some" ~ "M_in_Y") ) %>%
  filter(pred %in% c("X_in_Y", "M_in_Y")) %>%
  dplyr::select(-boot_eff)
data_coefs_par = rbind(data_M_par, data_Y_par) %>% rename(est = est_eff) %>% mutate(boot_type = "par")

data_M_npar = boot_results_npar$results_M %>%
  mutate(pred = case_match(pred, "q8.pcislow" ~ "X_in_M") )%>%
  filter(pred == "X_in_M")
data_Y_npar = boot_results_npar$results_Y %>%
  mutate(pred = case_match(pred, "q8.pcislow" ~ "X_in_Y", "q4.srcno_or_some" ~ "M_in_Y") ) %>%
  filter(pred %in% c("X_in_Y", "M_in_Y"))
data_coefs_npar = rbind(data_M_npar, data_Y_npar) %>% rename(est = est_eff) %>% mutate(boot_type = "npar")

data_coefs = rbind(data_coefs_par, data_coefs_npar) %>% mutate(is_AG = country == "AG")


### Make plot ----
plot_coefs_dens = ggplot(data_coefs, aes(x = country, y = est, color = is_AG)) + 
  geom_violin() +   guides(color="none") + ggtitle("Bootstrap Distributions of Coefficients") +
  facet_grid(rows = vars(pred), cols = vars(boot_type)) + geom_hline(yintercept = 0)

### Add estimates from original dataset ----
all_effects_data_plotting = all_effects_data %>%
  dplyr::select(-c(Y_Int, M_Int)) %>%
  pivot_longer(cols = -"country", names_to = "pred", values_to = "est") %>%
  mutate(pred = case_match(pred, "Y_X" ~ "X_in_Y", "Y_M" ~ "M_in_Y", "M_X" ~ "X_in_M"),
         is_AG = country == "AG")

plot_coefs_dens_plus_estimates = plot_coefs_dens +
  geom_point(data = all_effects_data_plotting)


## Mediation Effects ----
boot_ests_Y_par = boot_results_par$results_Y %>% dplyr::select(-boot_eff)
boot_ests_M_par = boot_results_par$results_M %>% dplyr::select(-boot_eff)
med_effs_par = get_many_med_effs(boot_ests_Y_par, boot_ests_M_par) %>% mutate(boot_type = "par")

boot_ests_Y_npar = boot_results_npar$results_Y
boot_ests_M_npar = boot_results_npar$results_M
med_effs_npar = get_many_med_effs(boot_ests_Y_npar, boot_ests_M_npar) %>% mutate(boot_type = "npar")

data_med_effs = rbind(med_effs_par, med_effs_npar) %>% 
  mutate(is_AG = country == "AG") %>%
  pivot_longer(c(de, ie, te), names_to = "med_type", values_to = "est")

### Make plot ----
plot_med_effs_dens = ggplot(data_med_effs, aes(x = country, y = est, color = is_AG)) +
  geom_violin() +   guides(color="none") + ggtitle("Bootstrap Distributions of Mediation Effects") +
  facet_grid(rows = vars(med_type), cols = vars(boot_type)) + geom_hline(yintercept = 1)


### Add estimates from original dataset ----
all_med_effects_data_plotting = all_med_effects_data %>%
  pivot_longer(cols = -"country", names_to = "med_type", values_to = "est") %>%
  mutate(is_AG = country == "AG")

plot_med_effs_dens_plus_estimates = plot_med_effs_dens +
  geom_point(data = all_med_effects_data_plotting)

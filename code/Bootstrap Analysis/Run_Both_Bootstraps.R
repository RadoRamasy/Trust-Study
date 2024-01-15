


# B = 1280 # Total number of bootstrap replicates
# B = 640 # Total number of bootstrap replicates
# B = 600 # Total number of bootstrap replicates
# B = 320 # Total number of bootstrap replicates
# B = 300 # Total number of bootstrap replicates
B = 90 # Total number of bootstrap replicates
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

# dat.ma = dat.ma[c(1:100, 22001:22100),]  ############################################################ Comment this out before pushing to Github


# Fit models to observed data ----
## Full models ----
# mod_Y = glmer(
#   q5.fc ~ q8.pcis + q4.src + q8.pcis * q4.src + q1.cc  + q2.dc + q3.pc +
#     age_group + gender + q7.la  + q9.edu +
#     (q8.pcis + q4.src + q8.pcis*q4.src | country),
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
load(here("code", "Bootstrap Analysis", "Fitted_Models.RData"))


# Explore fitted models ----
# dat.ma.reorder = dat.ma
# dat.ma.reorder$q8.pcis = relevel(dat.ma.reorder$q8.pcis, ref = "low")
# 
# mod_Y_fix = glm(
#   q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
#     age_group + gender + q7.la  + q9.edu,
#   data = dat.ma, x=T,
#   family = "binomial",
# )
# mod_M_fix  =  glm(
#   q4.src ~ q8.pcis + q2.dc + q3.pc +
#     age_group + gender + q7.la +  q9.edu,
#   data = dat.ma,
#   family = "binomial",
# )
# 
# mod_Y_Rad = glm(
#   q5.fc ~ q8.pcis + q9.edu + q7.la + q2.dc + q3.pc + age_group + gender + country,
#   data = dat.ma.reorder,
#   family = "binomial",
# )
# mod_M_Rad  =  glm(
#   q4.src ~ q8.pcis + q9.edu + q7.la + age_group + gender + country, 
#   data = dat.ma.reorder,
#   family = "binomial",
# )
# 
# 
# with(dat.ma, table(q5.fc, q8.pcis))
# Y_preds = mod_Y_fix$x[,-1]
# 
# PCA = prcomp(Y_preds, center=T, scale.=T)



# Construct dataset of fitted coefficients ----
# Note: Only includes those coefficients relevant to mediation analysis

## Fixed effects ----
fixef_Y = fixef(mod_Y)
fixef_M = fixef(mod_M)

all_fixefs = coef_vecs_2_data(fixef_Y, fixef_M) %>% mutate(country = "AG", .before=1)

## Country specific mediation effects ----
mix_Y = coef(mod_Y)[[1]]
mix_M = coef(mod_M)[[1]]

all_countries = levels(mod_Y@frame$country)
all_effects_data = all_fixefs

for(i in seq_along(all_countries)){
  this_country = all_countries[i]
  ind_this_country = which(rownames(mix_Y) == this_country)
  this_mix_Y = unlist(mix_Y[ind_this_country,])
  this_mix_M = unlist(mix_M[ind_this_country,])
  
  this_effect_data = coef_vecs_2_data(this_mix_Y, this_mix_M) %>%
    mutate(country = this_country, .before = 1)
  
  all_effects_data = rbind(all_effects_data, this_effect_data)
}





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
# boot_results_par = all_boot_reps_par(B, dat.ma, mod_Y, mod_M, .parallel = F, .verbose = T)
# toc()
# tic()
print("Parametric bootstrap complete. Running non-parametric bootstrap.")
boot_results_npar = all_boot_reps_npar(B, dat.ma, .parallel = T, .verbose = T)
print("Non-parametric bootstrap complete.")
cat("\n")
toc()


stopCluster(my_cluster)


# save(boot_results_par, boot_results_npar, file = here("code", "Bootstrap Analysis", "Both_Boots_Real_Data.RData"))
# save(boot_results_par, file = here("code", "Bootstrap Analysis", "Par_Boot_Real_Data_Revised.RData"))
# load(here("code", "Bootstrap Analysis", "Both_Boots_Real_Data.RData"))
load(file = here("code", "Bootstrap Analysis", "Par_Boot_Real_Data_Revised.RData"))


# 
# 
# 
# Y_data = boot_results_Y
# M_data = boot_results_M

boot_CIs_par = get_par_boot_CIs(boot_results_par, mod_Y, mod_M)
boot_CIs_npar = get_npar_boot_CIs(boot_results_npar, mod_Y, mod_M)


# Plot CIs ----

data_boot_CIs = boot_CIs_par %>%
  dplyr::select(-SEP) %>%
  full_join(boot_CIs_npar, by = c("country", "type", "estimate"), suffix = c("_par", "_npar")) %>%  # Consolidate datasets
  rename(wald_lcl_par = wald_lcl, wald_ucl_par = wald_ucl) %>%  # Add appropriate suffix to Wald intervals
  pivot_longer(!c(country, type, estimate), names_pattern = "(\\w+)_(\\w+)_(\\w+)", names_to = c("CI_type", "bound", "boot_type")) %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  mutate(Interval_Type = paste0(boot_type, "-", CI_type))

data_boot_CIs_plot = mutate(data_boot_CIs, is_AG = country=="AG") %>% rename(med_type = type)

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

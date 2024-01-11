


# B = 1280 # Total number of bootstrap replicates
B = 640 # Total number of bootstrap replicates
# B = 320 # Total number of bootstrap replicates
# B = 160 # Total number of bootstrap replicates
# B = 64 # Total number of bootstrap replicates
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




# Load other scripts ----
source(here("code", "Custom_Nonparametric_bootstrap_Functions.R"))

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
# ## Baron and Kenny style models ----
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


# pacman::p_load(merTools)





# Initialize Cluster ----
# n_cores = B
# n_cores = detectCores() - 1
n_cores = detectCores()
my_cluster = makeCluster(n_cores)
registerDoSNOW(cl = my_cluster)
clusterEvalQ(my_cluster,{
  pacman::p_load(tibble, lme4, here, boot, dplyr)

  source(here("code", "Custom_Nonparametric_bootstrap_Functions.R"))
})
clusterExport(my_cluster, c("dat.ma"))



# Run Analysis ----
tic()
# boot_results = all_boot_reps(B, dat.ma, .parallel = F)
boot_results = all_boot_reps(B, dat.ma, .parallel = T)
cat("\n")
toc()
boot_results_Y = boot_results$results_Y
boot_results_M = boot_results$results_M


stopCluster(my_cluster)


save(boot_results_Y, boot_results_M, file = here("data", "Results", "Product_Custom_NP_Boot-Raw.RData"))

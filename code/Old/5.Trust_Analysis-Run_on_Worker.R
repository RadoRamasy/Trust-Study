

# Access number of current job for use in seed setting
job_number = 1
method_type = "Par Boot 2"

# B = 10 # Total number of bootstrap replicates
B = 1280 # Total number of bootstrap replicates
# 10 boots take 296.5 sec 
# 1280 boots takes 4.3694 hours

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
               tictoc)




# Load other scripts ----
source(here("code", "2.Trust_Analysis-Mediation_Analysis.R"))
source(here("code", "3.Trust_Analysis-Bootstrap_Replication.R"))

# Load Data ----
load(here("data","CleanDataFile-Trust-Nov102023.RData"))

# dat.ma = dat.ma[c(1:100, 22001:22100),]  ############################################################ Comment this out before pushing to Github

# Initialize Cluster ----

# n_cores = detectCores() - 1
n_cores = detectCores()
my_cluster = makeCluster(n_cores)
registerDoSNOW(cl = my_cluster)

# what does this function do
# it would be great if we have a session explaining ho to do parallel analysis in R
clusterEvalQ(my_cluster,{
  library(tibble)
  library(lme4)
  library(here)
  source(here("code", "2.Trust_Analysis-Mediation_Analysis.R"))
  source(here("code", "3.Trust_Analysis-Bootstrap_Replication.R"))
})

# cat("\n")

# Fit models to observed data ----
mod_Y = glmer(
  q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
    age_group + gender + q7.la  + q9.edu +
    (q8.pcis + q4.src | country),
  data = dat.ma,
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)

mod_M  =  glmer(
  q4.src ~ q8.pcis + q2.dc + q3.pc +
    age_group + gender + q7.la +  q9.edu +
    (q8.pcis | country),
  data = dat.ma,
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)
  

## Pass some objects to worker processes
clusterExport(my_cluster, c("dat.ma", "mod_Y", "mod_M"))

tic()
# Run bootstrap analysis and consolidate output ----
boot_output = some_boot_analyses_par(dat.ma, mod_Y, mod_M, B, my_cluster, job_number)
boot_results = consolidate_bootstrap_output(boot_output)
cat("\n")
toc()

stopCluster(my_cluster)


# Save results to file ----
write_csv(boot_results$global_effects, here("data", "Results", "Global_Effects", paste0(method_type, " - GE.csv")))
write_csv(boot_results$country_effects, here("data", "Results", "Country_Effects", paste0(method_type, " - CE.csv")))
write_lines(boot_results$failed_fits, here("data", "Results", "Misc", paste0(method_type, " - Num_Failures.csv")))



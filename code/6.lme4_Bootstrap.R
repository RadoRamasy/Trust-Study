
# B = 1280 # Total number of bootstrap replicates
B = 640 # Total number of bootstrap replicates - Takes 21721 seconds
# B = 3 # Total number of bootstrap replicates
# B = 64 # Total number of bootstrap replicates

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




# Load Data ----
load(here("data","CleanDataFile-Trust-Nov102023.RData"))

# dat.ma = dat.ma[c(1:100, 22001:22100),]  ############################################################ Comment this out before pushing to Github


get_coefs = function(fit_merMod){
  
  coefs_wide = coef(fit_merMod)$country
  
  coefs_long = c()
  coefs_long_names = c()
  for(i in 1:nrow(coefs_wide)){
    for(j in 1:ncol(coefs_wide)){
      # print(paste0(i, "-", j))
      coefs_long = c(coefs_long, coefs_wide[i,j])
      coefs_long_names = c(coefs_long_names, paste0(colnames(coefs_wide)[j], "_", rownames(coefs_wide)[i]))
    }
  }
  names(coefs_long) = coefs_long_names
  
  output = c(fixef(fit_merMod), coefs_long)
  
  return(output)
}


# Fit models ----
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

tic()
all_boot_coefs_Y = bootMer(mod_Y, get_coefs, nsim=B, parallel="snow", ncpus = detectCores())$t
all_boot_coefs_M = bootMer(mod_M, get_coefs, nsim=B, parallel="snow", ncpus = detectCores())$t
toc()

write_csv(as_tibble(all_boot_coefs_Y), here("data", "Results", "lme4_boot-all_Y_coefs.csv"))
write_csv(as_tibble(all_boot_coefs_M), here("data", "Results", "lme4_boot-all_M_coefs.csv"))

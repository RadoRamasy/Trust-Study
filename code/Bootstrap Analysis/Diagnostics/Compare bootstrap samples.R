
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


set.seed(1)


# Load other scripts ----
source(here("code", "Bootstrap Analysis", "Bootstrap_CI_Helper_Functions.R"))
source(here("code", "Bootstrap Analysis", "Run_Bootstrap_Functions.R"))
source(here("code", "Bootstrap Analysis", "Analyse_Bootstrap_Functions.R"))


# Load Data ----
load(here("data","CleanDataFile-Trust-Nov102023.RData"))

# Load fitted models ----
load(here("code", "Bootstrap Analysis", "Output", "Fitted_Models.RData"))

# Load dataset of fitted coefficients ----
# Note: Only includes those coefficients relevant to mediation analysis
load(here("code", "Bootstrap Analysis", "Output", "Estimated_Effects.RData"))




# Generate non-parametric bootstrap sample ----

data_by_country = split(dat.ma, dat.ma$country)

data_npar = tibble()
for(this_data in data_by_country){
  this_boot_data = slice_sample(this_data, replace=T, prop=1) # Sample rows with replacement
  data_npar = rbind(data_npar, this_boot_data)
}




# Generate parametric bootstrap sample ----

## Bootstrap M ----

### Generate bootstrap values for M ----
boot_results_M = my_sim_merMod_par(mod_M)

### Store new M values in a data frame ----
this_dat_M = dat.ma %>% mutate(q4.src = factor(boot_results_M$boot_resp))

tic()
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
toc()


## Bootstrap Y ----

### Generate bootstrap responses ----
#### Get fitted coefficients from model fit to original dataset ----
##### Fixed Effects ----
fix_coef = fixef(mod_Y)

##### Random Effects ----
# Specifically, the SD matrix of the REs (i.e. Cholesky factor of RE covariance matrix)
ran_coef_SD_mat = getME(mod_Y, "Lambda")

#### Run simulation ----
boot_results_Y = my_sim_merMod_par(boot_mod_Y_intermediate, fix_coef, ran_coef_SD_mat)

### Store new Y values in a data frame ----
data_par = this_dat_M %>% mutate(q5.fc = factor(boot_results_Y$boot_resp))








# Compare non-parametric and parametric datasets ----

data_npar
data_par

## Focus on Y and M

get_info_mat <- function(data_npar, data_par){
  Y_npar = dplyr::select(data_npar, q5.fc)
  M_npar = dplyr::select(data_npar, q4.src)
  
  Y_par = dplyr::select(data_par, q5.fc)
  M_par = dplyr::select(data_par, q4.src)
  
  
  get_prop <- function(X) mean(X == "all_or_most")
  p_Y_npar = get_prop(Y_npar)
  p_M_npar = get_prop(M_npar)
  p_Y_par = get_prop(Y_par)
  p_M_par = get_prop(M_par)
  
  info = matrix(c(p_Y_npar, p_M_npar, p_Y_par, p_M_par), nrow = 2, byrow = T)
  colnames(info) = c("Y", "M")
  rownames(info) = c("npar", "par")
  info
}


all_countries = sort(unique(dat.ma$country))

all_info_mats = lapply(all_countries, function(this_country){
  this_data_npar = filter(data_npar, country == this_country)
  this_data_par = filter(data_par, country == this_country)
  
  this_info_mat = get_info_mat(this_data_npar, this_data_par)
})

diffs_by_country = sapply(all_info_mats, function(info){
  output = apply(info, 2, function(x) abs(x[1] - x[2]))
})
colnames(diffs_by_country) = all_countries
print(diffs_by_country)

max_diff = max(diffs_by_country)

coords_max_diff = which(diffs_by_country == max_diff, arr.ind = T)
max_diff_info_mat = all_info_mats[[coords_max_diff[2]]]
max_diff_components = max_diff_info_mat[,coords_max_diff[1]]

print(paste0("Maximum difference: ", max_diff))

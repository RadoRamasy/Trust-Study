

# Load and compile results from bootstraps across separate jobs ----

results_folder = here("data", "Results")


## Global (i.e. average) Mediation Effects ----

global_results_folder = paste0(results_folder, "/Global_Effects/")
global_results_files = list.files(global_results_folder)

all_global_results = tibble()

for(name in global_results_files){
  this_file = paste0(global_results_folder, "/", name)
  this_global_results = read_csv(this_file)
  
  all_global_results = rbind(all_global_results, this_global_results)
}

print(all_global_results, n = nrow(all_global_results))



## Country-Specific Mediation Effects ----

country_results_folder = paste0(results_folder, "/Country_Effects/")
country_results_files = list.files(country_results_folder)

all_country_results = tibble()

for(name in country_results_files){
  this_file = paste0(country_results_folder, name)
  this_country_results = read_csv(this_file)
  
  all_country_results = rbind(all_country_results, this_country_results)
}

print(all_country_results, n=nrow(all_country_results))




## Check for failed model fits ----

misc_folder = paste0(results_folder, "/Misc/")
failure_count_files = list.files(misc_folder)

total_failure_counts = 0

for(name in failure_count_files){
  this_file = paste0(misc_folder, name)
  this_failure_count = as.numeric(read_lines(this_file))
  
  total_failure_counts = total_failure_counts + this_failure_count
}

print(total_failure_counts)


# Install and load package
#devtools::install_github("james-thorson/FishLife", dep=TRUE)
library( FishLife )
library(tidyverse)


species_list <- c("Hucho taimen", "Parahucho perryi")

means <- FishBase_and_Morphometrics$beta_gv %>% 
  as_tibble(rownames = "species", .name_repair = "universal") %>% 
  filter(species %in% species_list) %>% 
  rename_with(~ str_replace(., "\\.+$", "")) %>% # drop trailing periods
  rename_with(~ str_c(., "_mean"), .cols = where(is.numeric)) %>%
  select(-c(contains("base"), contains("spawning"), contains("habitat"), contains("feeding"), contains("shape")))

# function to pull Variance estimates
get_var <- function(species_name){
  diag(FishBase_and_Morphometrics$Cov_gvv[species_name,,]) %>% 
    as_tibble_row(.name_repair = "universal") %>% 
    rename_with(~ str_replace(., "\\.+$", "")) %>% # drop trailing periods
    rename_with(~ str_c(., "_var")) %>% 
    mutate(species = species_name)
}

vars <- map(species_list, get_var) %>% 
  list_rbind() %>%  
  select(-c(contains("base"), contains("spawning"), contains("habitat"), contains("feeding"), contains("shape"))) %>% 
  relocate(species)

values <- full_join(means, vars, by = join_by(species))  %>% 
  mutate(across(contains("var"),
                ~ sqrt(.x),
                .names = "{str_replace(.col, 'var', 'sd')}"
  )) %>% 
  select(-contains("var")) %>% 
  mutate(across(where(is.numeric), signif)) #trim off extra digits

# assuming values are normally distributed
# μ = mean of log(X)
# σ = var of log(X)
# mean(X) = exp(μ + σ/2)
# SD(X) =  sqrt(exp(σ) – 1) * mean(X)

# unlog_values <- full_join(means, vars, by = join_by(species)) %>%
#   # get mean based on log mean and log var
#   mutate(across(contains("log.") & contains("_mean"),
#                 ~ exp(.x + (cur_data()[[str_replace(cur_column(), "_mean", "_var")]] / 2)),
#                 .names = "{str_replace(.col, 'log.', '')}")) %>% 
#   # get SD based on mean and log var 
#   mutate(across(contains("log.") & contains("_var"),
#                 ~ sqrt(exp(.x) - 1) * cur_data()[[str_replace(str_replace(cur_column(), 'log.', ''), '_var', '_mean')]],
#                 .names = "{str_replace(str_replace(.col, 'log.', ''), '_var', '_sd')}")) %>% 
#   select(-contains("log")) %>% 
#   mutate(across(contains("var"),
#                 ~ sqrt(.x),
#                 .names = "{str_replace(.col, 'var', 'sd')}"
#                 )) %>% 
#   select(-contains("var")) %>% 
#   mutate(across(where(is.numeric), signif)) #trim off extra digits


 write_csv(values, "data/fishlife_taimen.csv")

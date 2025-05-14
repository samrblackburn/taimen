library(tidyverse)

# read in generated distributions
# restructure and remove unnecessary params
fishlife <- read_csv("data/fishlife_taimen.csv")  %>% 
  select(species, log.linf_mean = log.length_infinity_mean, log.m_mean = log.natural_mortality_mean, log.k_mean = log.growth_coefficient_mean, 
         log.linf_sd = log.length_infinity_sd, log.m_sd = log.natural_mortality_sd, log.k_sd = log.growth_coefficient_sd)

# read in posteriors 
tugur_post <- read_csv("data/tugur_posteriors.csv")
eguur_post <- read_csv("data/eguur_posteriors.csv")
itou_post <- read_csv("data/itou_posteriors.csv")


# get n random draws of Linf and K using prior
# Linf: mean=146, SD=14.9
# K: mean=0.09, SD=0.0065
# convert to log normal distribution first
# Log SD = √[log(1 + (s²/m²))]
# Log mean = log(m) - [log(1 + (s²/m²)) / 2]

log_sd <- function(mean, sd){
  sqrt(log(1 + (sd^2/mean^2)))
}

log_mean <- function(mean, sd){
  log(mean) - (log(1 + (sd^2/mean^2)) / 2)
}

linf_mean <- log_mean(146, 14.9)
linf_sd <- log_sd(146, 14.9)

k_mean <- log_mean(0.09, 0.0065)
k_sd <- log_sd(0.09, 0.0065)

rows <- nrow(tugur_post)

# create list of linf and K to match posteriors
set.seed(980)
delger_onon_params <- tibble(linf = rlnorm(rows, linf_mean, linf_sd),
                             k = rlnorm(rows, k_mean, k_sd))


# add m pulled from fishlife distribution
perryi_m_mean <- fishlife %>% 
  filter(species == "Parahucho perryi") %>% 
  pull(log.m_mean)

perryi_m_sd <- fishlife %>% 
  filter(species == "Parahucho perryi") %>% 
  pull(log.m_sd)

taimen_m_mean <- fishlife %>% 
  filter(species == "Hucho taimen") %>% 
  pull(log.m_mean)

taimen_m_sd <- fishlife %>% 
  filter(species == "Hucho taimen") %>% 
  pull(log.m_sd)

set.seed(980)
itou_m <- tibble(m = rlnorm(rows, perryi_m_mean, perryi_m_sd))
taimen_m <- tibble(m = rlnorm(rows, taimen_m_mean, taimen_m_sd))


delger_params <- delger_onon_params %>% 
  bind_cols(taimen_m) %>% 
  mutate(pop = "delger",
         species = "Hucho taimen")

onon_params <- delger_onon_params %>% 
  bind_cols(taimen_m) %>% 
  mutate(pop = "onon",
         species = "Hucho taimen")

tugur_params <- tugur_post %>% 
  bind_cols(taimen_m) %>% 
  mutate(pop = "tugur",
         species = "Hucho taimen")

eguur_params <- eguur_post %>% 
  bind_cols(taimen_m) %>% 
  mutate(pop = "eguur",
         species = "Hucho taimen")

koppi_params <- itou_post %>% 
  bind_cols(itou_m) %>% 
  mutate(pop = "koppi",
         species = "Parahucho perryi")

karibetsu_params <- itou_post %>% 
  bind_cols(itou_m) %>% 
  mutate(pop = "karibetsu",
         species = "Parahucho perryi")

bind_rows(tugur_params, koppi_params, karibetsu_params, eguur_params, delger_params, onon_params) %>% 
  write_csv("data/params.csv")



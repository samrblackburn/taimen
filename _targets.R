library(targets)

## if needed, install required packages
#install.packages(c("tidyverse", "sf", "maptiles", "terra", "colorspace", "FSA", "brms", "tidybayes", "gridExtra", "furrr", "LBSPR", "devtools"))
#devtools::install_github("james-thorson/FishLife", dep=TRUE)

tar_option_set(packages = c("tidyverse", "sf", "maptiles", "terra", "colorspace", "FishLife", "FSA", "brms", "tidybayes", "gridExtra", "furrr", "LBSPR"),
               seed = 980) 


# helper functions to convert from normal distribution to lognormal
# Log SD = √[log(1 + (s²/m²))]
# Log mean = log(m) - [log(1 + (s²/m²)) / 2]
log_sd <- function(mean, sd){
  sqrt(log(1 + (sd^2/mean^2)))
}
log_mean <- function(mean, sd){
  log(mean) - (log(1 + (sd^2/mean^2)) / 2)
}


# code files
source("R/map.R")
source("R/fishlife.R")
source("R/vb_fits.R")
source("R/param_prep.R")
source("R/montecarlo.R")
source("R/mc_plotting.R")
source("R/plotmat.R")
source("R/plotsize.R")
source("R/spr_plotting.R")

list(
  map_targets,
  fishlife_targets,
  vb_fit_targets,
  param_prep_targets,
  montecarlo_targets,
  mc_plot_targets,
  plotmat_targets,
  plotsize_targets,
  lbspr_plot_targets
)

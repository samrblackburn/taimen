library(tidyverse)
library(furrr)
library(LBSPR)

# set up parallel processing
plan(multisession)

# read in data files - assumes you are already in the directory for this project and have files in a folder called Data
# uncomment the version below to use the paths you had in the other file
rivers <- list(
  delger = as.matrix(read_csv("data/DelgerLengthAssessmentWorkshop_final.csv")),
  onon = as.matrix(read_csv("data/OnonLengthAssessmentWorkshop_final.csv")),
  eguur = as.matrix(read_csv("data/EgUurLengthAssessmentWorkshop_final.csv")),
  tugur = as.matrix(read_csv("data/TugurLengthAssessmentWorkshop_final.csv")),
  karibetsu = as.matrix(read_csv("data/KaribetsuLengthAssessmentWorkshop_final.csv")),
  koppi = as.matrix(read_csv("data/KoppiLengthAssessmentWorkshop_final.csv"))
)

# params M, K Linf. M from FishLife, K and Linf from Jensen et al. 2009, and Bayesian VB model fitting
# K and Linf are posteriors from modeling except for Delger and Onon populations, where they are randomly pulled from distribution from Jensen et al 2009
# M are randomly pulled from distribution based on species in FishLife
# In addition to the posteriors, this has pregenerated random M values, so that we can easily pull Linf and K from the same posterior and have an M ready to go
params <- read_csv("data/params.csv")


# define params
create_tpars <- function(species, m, k, linf){
  TPars <- new("LB_pars", verbose = FALSE)
  
  # Cosmetic
  TPars@Species <- species
  TPars@L_units <- "cm" # units for fish length measure
  
  # Biology
  TPars@Linf <- linf  # von Bertalanffy asymptotic length
  TPars@CVLinf <- 0.2
  TPars@L50 <- 50 # Length @ 50% maturity - 50, approx from growth curve at age 4
  TPars@L95 <- 75 # Length @ 95% maturity - 75, approx from growth curve at age 7
  TPars@MK <- m / k
  TPars@Walpha <- 0.0047 # a from vB weight equation - Jensen et al. 2009
  TPars@Wbeta <- 3.1 # b from vB weight equation - Jensen et al. 2009
  
  TPars
}


# create lb_lengths from a data matrix
# lbspr package initialization for lb_length object claims to handle matrices, but doesn't
# this uses the code from the initialization that was supposed to handle matrices
# we can't be reading in files every time, way too slow
# only handles "raw" data
create_lvec <- function(LB_pars, dat){
  lvec = new("LB_lengths", verbose = FALSE)
  dat <- as.data.frame(dat)
  lvec@Years <- as.numeric(names(dat[1:ncol(dat)]))
  lvec@NYears <- ncol(dat)
  LB_pars@BinMax <- ceiling((max(LB_pars@Linf * 1.1, 1.1 * max(dat, na.rm=TRUE)))/0.5)*0.5
  LB_pars@BinMin <- floor((min(dat, na.rm=TRUE) * 0.9)/0.5)*0.5
  LB_pars@BinWidth <- floor((1/20 * LB_pars@BinMax)/5)*5
  LBins <- seq(from=LB_pars@BinMin, by=LB_pars@BinWidth, to=LB_pars@BinMax)
  lvec@LData <- sapply(1:ncol(dat), function(x) hist(dat[,x], breaks=LBins, plot=FALSE)$counts)
  lvec@LMids <- hist(dat[,1], breaks=LBins, plot=FALSE)$mids
  if (length(LB_pars@L_units) > 0) lvec@L_units <- LB_pars@L_units
  return(lvec)
}


# run model with specified values
run_lbspr <- function(name, species, m, k, linf, data) {
  
  tpars <- create_tpars(species, m, k, linf)
  
  Lvec <- create_lvec(tpars, data)

  SPR_est <- LBSPRfit(tpars, Lvec, verbose = FALSE)
  
  tibble(river=name, year=SPR_est@Years, MK=SPR_est@MK, LInf = SPR_est@Linf, 
  SL50=SPR_est@SL50, SL95=SPR_est@SL95, FM=SPR_est@FM, SPR=SPR_est@SPR, 
  SL50.Var = SPR_est@Vars[,"SL50"], SL95.Var = SPR_est@Vars[,"SL95"], FM.Var = SPR_est@Vars[,"FM"], SPR.Var = SPR_est@Vars[,"SPR"])
}

# run model a specified number of times and bind together results
# switch to pulling in relevant param df and dropping rows after n
mc_lbspr <- function(data, name, n = 100){
  # get relevant params for populations
  pars <- params %>% 
    filter(pop == name) %>% 
    head(n) %>% 
    select(name = pop, species, m, k, linf)
  
  # iterate over M, K, LInf values and fit model, then bind together
  # setting seed for furrr because LBSPR generates random numbers during model fit
  future_pmap(pars, run_lbspr, data, .options = furrr_options(seed = TRUE)) %>%
    list_rbind()
  
}


#run simulation for all rivers and return output bound together
bind_mc_lbspr <- function(rivers, n = 100) {
  #iterate over each river , run simulation, and bind together output
  imap(rivers, ~mc_lbspr(.x, .y, n)) %>% 
    list_rbind()
}


# actually run the simulation - change the number to run more simulations
# 100 simulations takes about 15-30 seconds on my computer
# can't be more than number of bayesian iterations; currently 6,000
# may spit out warnings about package availability, this seems to be a bug: https://github.com/Calvagone/campsis/issues/157
mc_output <- bind_mc_lbspr(rivers, 100)

# save output if doing a big run
#write_csv(mc_output, "data/mc_output.csv")






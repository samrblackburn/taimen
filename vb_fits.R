library(tidyverse)
library(FSA)
library(brms)
library(tidybayes)

# read in data
itou <- read_csv("data/itou_age_length.csv")
eguur <- read_csv("data/eguur_age_length.csv")
tugur08 <- read_csv("data/tugur_age_length_2008.csv")
tugur17 <- read_csv("data/tugur_age_length_2017.csv")
tugur <- bind_rows(tugur08, tugur17)

# set up VB growth formula
formula <- brmsformula(length ~ Linf * (1 - exp(-K * (age - t0))),
                       Linf ~ 1, K ~ 1, t0 ~ 1, nl=TRUE)

# priors for itou - uninformative
itou_priors <- prior(normal(0,500), nlpar="Linf", lb=0) +
  prior(normal(0,10), nlpar="K", lb=0) +
  prior(normal( 0,10), nlpar="t0") +
  prior(student_t(3,0,40), class=sigma)

# priors for Tugur and Eguur, form Jensen et al. 2009
taimen_priors <- prior(normal(146,14.9), nlpar="Linf", lb=0) +
  prior(normal(0.09,0.0065), nlpar="K", lb=0) +
  prior(normal( 0,10), nlpar="t0") +
  prior(student_t(3,0,40), class=sigma)

# get initial values, randomly pulled from reasonable distribution
set.seed(980)
inits <- function() list(
  Linf=runif(1, 50, 500),
  K=runif(1, 0.01, 2.00),
  t0=rnorm(1, 0, 0.5)
)

# set metaparams
chains <- 3
iter <- 3000
warmup <- 1000

# run fitting and check fits for itou, eguur, and tugur
# save out posteriors to use in MC simulation
set.seed(980)
itou_fit <- brm(formula,            # calls the formula object created above
            family=gaussian(),  # specifies the error distribution
            data=itou,         # the data object
            prior=itou_priors,       # the prior probability object 
            init=inits,      # the initial values object
            chains=chains,           # number of chains, typically 3 to 4
            cores=chains,            # number of cores for multi-core processing. Typically set to match number of chains. 
            iter=iter,          # number of iterations
            warmup=warmup,        # number of warm up steps to discard
            control=list(adapt_delta=0.80,    # Adjustments to algorithm to 
                         max_treedepth=15)) # improve convergence.

plot(itou_fit)
pp_check(itou_fit,ndraws=100)  # posterior predictive checks
itou_sum <- summary(itou_fit)
itou_post <- as_draws_df(itou_fit)

itou_draws <- itou %>%
  add_predicted_draws(itou_fit) 

itou_draws %>%  # adding the posterior distribution with tidybayes
  ggplot(aes(x=age)) +  
  stat_lineribbon(aes(y=.prediction), .width=c(.95, .80, .50),  # regression line and CI
                  alpha=0.5, colour="black") +
  geom_point(aes(y = length), color="darkblue", size=3) +   # raw data
  scale_fill_brewer(palette="Greys") +
  ylab("Length (cm)") + 
  xlab("Age (years)") +
  ggtitle("Itou") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.85))

itou_post %>% 
  select(linf = b_Linf_Intercept, k = b_K_Intercept) %>% 
  write_csv("data/itou_posteriors.csv")

set.seed(980)
eguur_fit <- brm(formula,            # calls the formula object created above
                family=gaussian(),  # specifies the error distribution
                data=eguur,         # the data object
                prior=taimen_priors,       # the prior probability object 
                init=inits,      # the initial values object
                chains=chains,           # number of chains, typically 3 to 4
                cores=chains,            # number of cores for multi-core processing. Typically set to match number of chains. 
                iter=iter,          # number of iterations
                warmup=warmup,        # number of warm up steps to discard
                control=list(adapt_delta=0.80,    # Adjustments to algorithm to 
                             max_treedepth=15)) # improve convergence.

plot(eguur_fit)
pp_check(eguur_fit,ndraws=100)  # posterior predictive checks
eguur_sum <- summary(eguur_fit)
eguur_post <- as_draws_df(eguur_fit)

eguur_draws <- eguur %>%
  add_predicted_draws(eguur_fit) 

eguur_draws %>%  # adding the posterior distribution with tidybayes
  ggplot(aes(x=age)) +  
  stat_lineribbon(aes(y=.prediction), .width=c(.95, .80, .50),  # regression line and CI
                  alpha=0.5, colour="black") +
  geom_point(aes(y = length), color="darkblue", size=3) +   # raw data
  scale_fill_brewer(palette="Greys") +
  ylab("Length (cm)") + 
  xlab("Age (years)") +
  ggtitle("Eg-Uur") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.85))

eguur_post %>% 
  select(linf = b_Linf_Intercept, k = b_K_Intercept) %>% 
  write_csv("data/eguur_posteriors.csv")

set.seed(980)
tugur_fit <- brm(formula,            # calls the formula object created above
                 family=gaussian(),  # specifies the error distribution
                 data=tugur,         # the data object
                 prior=taimen_priors,       # the prior probability object 
                 init=inits,      # the initial values object
                 chains=chains,           # number of chains, typically 3 to 4
                 cores=chains,            # number of cores for multi-core processing. Typically set to match number of chains. 
                 iter=iter,          # number of iterations
                 warmup=warmup,        # number of warm up steps to discard
                 control=list(adapt_delta=0.80,    # Adjustments to algorithm to 
                              max_treedepth=15)) # improve convergence.

plot(tugur_fit)
pp_check(tugur_fit,ndraws=100)  # posterior predictive checks
tugur_sum <- summary(tugur_fit)
tugur_post <- as_draws_df(tugur_fit)

tugur_draws <- tugur %>%
  add_predicted_draws(tugur_fit) 

tugur_draws %>%  # adding the posterior distribution with tidybayes
  ggplot(aes(x=age)) +  
  stat_lineribbon(aes(y=.prediction), .width=c(.95, .80, .50),  # regression line and CI
                  alpha=0.5, colour="black") +
  geom_point(aes(y = length), color="darkblue", size=3) +   # raw data
  scale_fill_brewer(palette="Greys") +
  ylab("Length (cm)") + 
  xlab("Age (years)") +
  ggtitle("Tugur") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.85))

tugur_post %>% 
  select(linf = b_Linf_Intercept, k = b_K_Intercept) %>% 
  write_csv("data/tugur_posteriors.csv")


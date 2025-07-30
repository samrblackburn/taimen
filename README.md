# Taimen Analysis and Figure R Pipeline

This repository holds code to model populations of Hucho taimen and Parahucho perryi in Mongolia and Japan using LBSPR models. In particular, it features a Montecarlo simulation of LBSPR model fits based on known distributions of M, K, and LInf. K and LInf were modeled using Bayesian methods for some populations based on a Von Bertalanffy growth curve.
This analysis and its accompanying figures were used in <publication>.
It is setup as an automated pipeline using the [`targets`](https://books.ropensci.org/targets/) R package in order to orchestrate a modular workflow where dependency tracking determines which components need to be built. See the "Running Pipeline" section below for steps.

## Structure

-   `/data` contains the data files underlying the analysis, split into fish length data for populations in specific years. There are also age/length data used in the Von Bertalanffy growth models.
-   `/R` contains the R scripts used to build and analyse the data sets, divided out by steps of the process. 
    - `map.R` builds a map of population locations.
    - `fishlife.R` extracts LInf, M, and K distributions for the two species from the FishLife database.
    - `vb_fits.R` fits Von Bertalanffy growth models to age/length data from some of the populations and to extract LInf and K from those models. It also plots the model fits.
    - `param_prep.R` prepares a data frame of randomly pulled LInf, M, and K values from their distributions for the two species based on Fishlife, Von Bertalanffy modeling, and previous research.
    - `montecarlo.R` runs the montecarlo simulation using the randomly pulled values from `param_prep.R`
    - `mc_plotting.R` plots the montecarlo simulations results for each population.
    - `plotmat.R` and `plotsize.R` are versions of plotting functions in the LBSPR package modified to use custom aesthetics.
    - `spr_plotting.R` runs single LBSPR models for each population based on the mean values of LInf, M, and K from the distributions used for the montecarlo simulations. It plots the model results and F/M ratios for each population.

## Running Pipeline

After downloading and opening the repository, run the following snippet:

``` r
# install required packages
install.packages(c("targets", tidyverse", "sf", "maptiles", "terra", "colorspace", "FSA", "brms", "tidybayes", "gridExtra", "furrr", "LBSPR", "devtools"))
devtools::install_github("james-thorson/FishLife", dep=TRUE)

# build datasets
targets::tar_make()
```

## Using the Data

If you have questions about this pipeline, please [get in touch](mailto:srblackburn@wisc.edu)! 

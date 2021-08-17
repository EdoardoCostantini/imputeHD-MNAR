# imputeHD-comp
Repository hosting project high-dimensional imputation comparison.

## Summary
Including a large number of predictors in the imputation model underlying a 
Multiple Imputation (MI) procedure is one of the most challenging tasks 
imputers face.

A variety of high-dimensional MI techniques (HD-MICE) can facilitate this task,
but there has been limited research on their relative performance.
In this study, we investigate a wide range of extant HD-MICE techniques that 
can handle a large number of predictors in the imputation model and general 
missing data patterns.

We assess the relative performance of seven HD-MICE methods with a Monte Carlo
simulation study and a resampling study based on real survey data.
The performance of the methods is defined by the degree to which they facilitate 
unbiased and confidence-valid estimates of the parameters of complete data 
analysis models.

We find that using regularized regression to select the predictors used in the
MI model, and using principal component analysis to reduce the dimensionality 
of auxiliary data produce the best results.

# How to replicate results

## Running the simulations

We used R for these simulations.

### Simulation Study: data from a multivariate normal distribution
1. Installing Dependencies:
2. Running the simulation:
   1. Open the script [exp1_simulation_script_win.R](./code/exp1_simulation_script_win.R)
   2. Make sure the working directory is set to the location of this script (`./code/`)
   3. Define the number of clusters to be used by specifying the first argument in the
      function `makeCluster()`
   4. Run the entire script

### EVS resampling study
1. Installing Dependencies: same as above
2. Preparing the EVS population data:
   1. Download the appropriate EVS wave
   2. Store it in the data folder inside this project (at the same level as the code 
      folder)
   3. Run the script [exp4_readEVS.R](./code/exp4_readEVS.R) 
3. Running the simulation:
   1. Open the script [exp4_simulation_script_win.R](./code/exp4_simulation_script_win.R)
   2. Make sure the working directory is set to the location of this script (`./code/`)
   3. Define the number of clusters to be used by specifying the first argument in the function 
      `makeCluster()`
   4. Run the entire script

## Obtaining the plots and tables
The procedure is described for the simulation study. 
By using the scripts for "exp4", the same procedure can be followed for the EVS 
resampling study.
1. Open the script [exp1_results.R](./code/exp1_results.R) and make sure you specify 
   the name of the .rds file obtained from the simulation study run.
   This script will extract the results reported in the study.
2. Open the script [exp1_analysis.R](./code/exp1_analysis.R) and make sure you specify 
   the name of the .rds file obtained from the [exp1_results.R](./code/exp1_results.R)
   run.
   To obtain all the plots, you can play around with the parameters defining what 
   is plotted by the script. For example, by changing `pm_grep <- "0.3"` to `0.1`
   you will be able to produce the plots for the smaller proportion of missing 
   cases.

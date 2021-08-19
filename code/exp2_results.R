### Title:    Results for experiment 2
### Project:  Imputing High Dimensional Data w/ MNAR
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2021-08-19

  rm(list = ls())
  source("./init_general.R")

## Latent variables as MAR predictors (join multiple results)
  filename1 <- "exp2_simOut_20201217_1539"
  filename2 <- "exp2_simOut_20201224_1048"
  filename_lv <- "exp2_simOut_2020122417" # to save the output
  
  # Read R objects
  out_pt1 <- readRDS(paste0("../output/", filename1, ".rds"))
  out_pt2 <- readRDS(paste0("../output/", filename2, ".rds"))

  # Put together
  out_lv <- c(out_pt1[-c((out_pt1$parms$dt_rep+1):length(out_pt1))],
              out_pt2[-c((out_pt1$parms$dt_rep+1):length(out_pt2))])

  # fix parms that need to be fixed
  dt_reps_true <- length(out_lv)
  out_lv$parms <- out_pt1$parms
  out_lv$conds <- out_pt1$conds
  out_lv$parms$dt_rep <- dt_reps_true

  # append info from single runs
  out_lv$info <- list(out_pt1 = out_pt1[c(501:length(out_pt1))],
                      out_pt2 = out_pt2[c(501:length(out_pt2))])

## Items as MAR predictors (1e3)
  filename_it <- "exp2_simOut_20210728_1351" # Items as MAR predictors (1e3)

  # Read R object
  out_it <- readRDS(paste0("../output/", filename_it, ".rds"))

## Define parameters valid for all
  n_conds <- nrow(out_lv$conds)

# Extract Estimates ------------------------------------------------------

## SEM estiamtes raw data (saturated model) ##
  # Extract results per conditions
  sem_lv <- lapply(seq_along(1:n_conds),
                     function(x) res_sum(out_lv,
                                         model = "semR",
                                         condition = x,
                                         bias_sd = TRUE))
  sem_it <- lapply(seq_along(1:n_conds),
                       function(x) res_sum(out_it,
                                           model = "semR",
                                           condition = x,
                                           bias_sd = TRUE))
  
## CFA model results
  # Extract results per conditions
  CFA_lv <- lapply(seq_along(1:n_conds),
                     function(x) res_sum(out_lv,
                                         model = "CFA",
                                         condition = x))
  CFA_it <- lapply(seq_along(1:n_conds),
                   function(x) res_sum(out_it,
                                       model = "CFA",
                                       condition = x))

# Save Results ------------------------------------------------------------
  output_lv <- lapply(list(sem = sem_lv,
                           CFA  = CFA_lv),
                      function(x){
                        names(x) <- paste0("cond", seq_along(out_lv[[1]]))
                        return(x)
                      }
  )
  output_lv$parms <- out_lv$parms
  output_lv$conds <- out_lv$conds

  output_it <- lapply(list(sem = sem_it,
                           CFA  = CFA_it),
                      function(x){
                        names(x) <- paste0("cond", seq_along(out_it[[1]]))
                        return(x)
                      }
  )
  output_it$parms <- out_it$parms
  output_it$conds <- out_it$conds

  saveRDS(
    output_lv,
    paste0("../output/", filename_lv, "_res_lv.rds")
  )

  saveRDS(
    output_it,
    paste0("../output/", filename_it, "_res_it.rds")
  )

# Melted representation -----------------------------------------------------

# Reshaped representation

out_lv_reshaped <- reshame_exp2res(out = out_lv,
                                   n_reps = length(out_lv) - 3, # last three elements are session info
                                   n_conds = n_conds, # same for any repetitions, take 1 as example
                                   MAR_type = "LV")

out_it_reshaped <- reshame_exp2res(out = out_it,
                                   n_reps = length(out_it) - 3, # last three elements are session info
                                   n_conds = n_conds, # same for any repetitions, take 1 as example
                                   MAR_type = "IT")

gg_shape <- rbind(out_lv_reshaped, out_it_reshaped)

saveRDS(
  gg_shape,
  paste0("../output/", "melt.rds")
)
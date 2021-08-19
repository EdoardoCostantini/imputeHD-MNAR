### Title:    Results for experiment 2
### Project:  Imputing High Dimensional Data w/ MNAR
### Author:   Edoardo Costantini
### Created:  2020-05-19

  rm(list = ls())
  source("./init_general.R")
  
## Items as MAR predictors (1e3)
  filename_lv <- "exp2_simOut_20210728_1351" # Items as MAR predictors (1e3)
  
  # Read R object
  out_lv <- readRDS(paste0("../output/", filename_lv, ".rds"))
  
## Latent variables as MAR predictors (join multiple results)
  filename1 <- "exp2_simOut_20201217_1539"
  filename2 <- "exp2_simOut_20201224_1048"
  filename  <- "exp2_simOut_2020122417" # to save the output
  
  # Read R objects
  out_pt1 <- readRDS(paste0("../output/", filename1, ".rds"))
  out_pt2 <- readRDS(paste0("../output/", filename2, ".rds"))
  
  # Put together
  out <- c(out_pt1[-c((out_pt1$parms$dt_rep+1):length(out_pt1))], 
           out_pt2[-c((out_pt1$parms$dt_rep+1):length(out_pt2))])
  
  # fix parms that need to be fixed
  dt_reps_true <- length(out)
  out$parms <- out_pt1$parms
  out$conds <- out_pt1$conds
  out$parms$dt_rep <- dt_reps_true
  
  # append info from single runs
  out$info <- list(out_pt1 = out_pt1[c(501:length(out_pt1))],
                   out_pt2 = out_pt2[c(501:length(out_pt2))])

# Check presence
  out$conds
  out$parms$dt_rep
  out$parms$iters
  out$parms$burnin_imp_bl
  out$parms$thin
  out$parms$keep_dt
  
# Time Analyses -----------------------------------------------------------

  out_time <- sapply(1:length(names(out[[1]])), res_sem_time, out = out)
  colnames(out_time) <- names(out[[1]])
  t(out_time)
  
# Extract Estimates ------------------------------------------------------
  
## SEM estiamtes raw data (saturated model) ##
  # Extract results per conditions
  semR_res <- lapply(seq_along(1:length(out[[1]])),
                     function(x) res_sum(out, 
                                         model = "semR", 
                                         condition = x,
                                         bias_sd = TRUE))
  
  # Show results all conditions for a given data rep
  t(sapply(1:length(semR_res),
         function(x) semR_res[[x]]$validReps))
  lapply(1:length(semR_res),
         function(x) round(semR_res[[x]]$bias_raw[1:10,],2))
  lapply(1:length(semR_res),
         function(x) semR_res[[x]]$bias_sd)
  lapply(1:length(semR_res),
         function(x) semR_res[[x]]$bias_per[c(1:10),])
  lapply(1:length(semR_res),
         function(x) semR_res[[x]]$bias_per[-c(1:10),])
  lapply(1:length(semR_res),
         function(x) semR_res[[x]]$ci_cov)
  
## CFA model results
  # Extract results per conditions
  CFA_res <- lapply(1:length(out[[1]]),
                     function(x) res_sum(out, 
                                         model = "CFA", 
                                         condition = x))
  res_sum(out, 
          model = "CFA", 
          condition = 1)
  res_sum(out, 
          model = "CFA", 
          condition = 8)
  
  # Results?
  lapply(1:length(out[[1]]),
         function(x) CFA_res[[x]]$bias_per[1:10, ])
  
## SEM estaimted Scored data
  # Extract results per conditions
  semS_res <- lapply(1:length(out[[1]]),
                    function(x) res_sum(out, 
                                        model = "semS", 
                                        condition = x,
                                        bias_sd = TRUE))
  
  # Show results for a given condition
  lapply(1:length(out[[1]]),
         function(x) semS_res[[x]]$bias_raw[1:2,])
  lapply(1:length(out[[1]]),
         function(x) semS_res[[x]]$bias_sd)
  lapply(1:length(out[[1]]),
         function(x) semS_res[[x]]$bias_per[-c(1:2),])
  lapply(1:length(out[[1]]),
         function(x) semS_res[[x]]$ci_cov)

## Linear Model: Intercept and regression coefficients ##
  lm_res <- lapply(1:length(out[[1]]),
                     function(x) res_sum(out, 
                                         model = "lm", 
                                         condition = x))
  
  # Show results for a given condition
  lapply(1:length(out[[1]]),
         function(x) lm_res[[x]]$bias_per)
  lapply(1:length(out[[1]]),
         function(x) lm_res[[x]]$ci_cov)

# Save Results ------------------------------------------------------------
  output <- lapply(list(semR = semR_res,
                        CFA  = CFA_res,
                        semS = semS_res,
                        lm   = lm_res), 
                   function(x){
                     names(x) <- paste0("cond", seq_along(out[[1]]))
                     return(x)
                   }
  )
  output$parms <- out$parms
  output$conds <- out$conds
  
  saveRDS(
    output, 
    paste0("../output/", filename, "_res.rds")
  )


# Extra Details Study -----------------------------------------------------

# > Standard Deviation of estimates
  output$semR$cond4$MCMC_est[1:10, ]
  output$semR$cond4$bias_per[1:10, ]
  output$semR$cond4$bias_sd[1:10, ]
  data.frame(
    bias_per = round(colMeans(output$semR$cond4$bias_per[1:10, ]), 3)[-1],
    bias_sd = round(colMeans(output$semR$cond4$bias_sd[1:10, ]), 3),
    var_est = round(colMeans(output$semR$cond4$var_est[1:10, ]), 3)
  )
  # The average variance of the parmaters estiamtes is almost the same for the
  # different methods. In particular, bridge does not show greater variance than
  # the other methods.

# Reshaped representation

out_lv_reshaped <- reshame_exp2res(out = out_lv,
                                   n_reps = length(out_lv) - 3, # last three elements are session info
                                   n_conds = length(out_lv[[1]]), # same for any repetitions, take 1 as example
                                   MAR_type = "LV")

out_it_reshaped <- reshame_exp2res(out = out,
                                   n_reps = length(out) - 3, # last three elements are session info
                                   n_conds = length(out[[1]]), # same for any repetitions, take 1 as example
                                   MAR_type = "IT")

gg_shape <- rbind(out_lv_reshaped, out_it_reshaped)

dim(out_ggplot)
rm(out_ggplot)
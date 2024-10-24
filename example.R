#Newey and Robins DCDR run example

install.packages("purrr")
install.packages("furrr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tibble")
install.packages("SuperLearner")

require(purrr)
require(furrr)
require(dplyr)
require(tidyr)
require(tibble)
require(SuperLearner)

source("estimators.R")
set.seed(123)

#Data and variables
a <- as_tibble(read.csv("statin_sim_data_single300.csv"))
covars <- c("age", "ldl_log", "diabetes", "risk_score")
exposure <- "statin"
outcome <- "Y"

#Learners to use
#Logistic Regression
SL.glm.DCDR <- function(...){
  SL.glm(...)
}

#4 degree GAM
SL.gam4.DCDR <- function(...){
  SL.gam(..., deg.gam=4)
}

#6 degree GAM
SL.gam6.DCDR <- function(...){
  SL.gam(..., deg.gam=6)
}

#Neural Network
SL.nnet.DCDR <- function(...){
  SL.nnet(..., size=4)
}

#Random forest
SL.randomForest.DCDR <- function(...){
  SL.randomForest(...,ntree=500, nodesize=20)
}

#Empirical mean
SL.mean.DCDR <- function(...){
  SL.mean(...)
}

#Learner Library
library <- c("SL.glm.DCDR", "SL.gam4.DCDR", "SL.gam6.DCDR", "SL.nnet.DCDR", "SL.randomForest.DCDR", "SL.mean.DCDR")

#CV Control
control <- SuperLearner.CV.control(V=5)

#Make data
sims <- a %>%
  group_by(sim_id) %>%
  nest()

#Multicore
# plan(multiprocess)

#Sim DCTMLE
sim_dctmle <- function(df, curr_sim_id){
  tryCatch({
    output <- DCTMLE_Multiple(df, exposure, outcome, covars, covars, library, control, 10, curr_sim_id)
    return(output)
  }, error = function(e) {
    warning(paste("Error in sim_dctmle:", e$message))
    return(tibble(r1=NA, r0=NA, rd=NA, v1=NA, v0=NA, vd=NA, mv1=NA, mv0=NA, mvd=NA))
  })
}

sim_results <- map2(.x = sims$data, .y = sims$sim_id, .f = safely(sim_dctmle))
sims <- sims %>%
  mutate(simresultDCTMLE = sim_results)

resultsDCTMLE <- bind_rows(sims$simresultDCTMLE)

#Sim SCTMLE
sim_sctmle <- function(df, curr_sim_id){
  tryCatch({
    output <- SCTMLE_Multiple(df, exposure, outcome, covars, covars, library, control, 10, curr_sim_id)
    return(output)
  }, error = function(e) {
    warning(paste("Error in sim_sctmle:", e$message))
    return(tibble(r1=NA, r0=NA, rd=NA, v1=NA, v0=NA, vd=NA, mv1=NA, mv0=NA, mvd=NA))
  })
}

sim_results <- map2(.x = sims$data, .y = sims$sim_id, .f = safely(sim_sctmle))
sims <- sims %>%
  mutate(simresultSCTMLE = sim_results)

resultsSCTMLE <- bind_rows(sims$simresultSCTMLE)

# Saving result tables
write.csv(resultsDCTMLE, "resultsDCTMLE_single300.csv", row.names = FALSE)
write.csv(resultsSCTMLE, "resultsSCTMLE_single300.csv", row.names = FALSE)


# #Sim DCDR
# sim_dcdr <- function(df){
#   output <- DCDR_Multiple(df, exposure, outcome, covarsC, covarsZ, library, control, 10)
#   return(output)
# }
#
# sims <- sims %>%
#   mutate(simresultDCDR = future_map(data, sim_dcdr))
#
# resultsDCDR <- bind_rows(sims$simresultDCDR)
#
# #Sim IPW
# sim_ipw <- function(df){
#   output <- IPW(df, exposure, outcome, covarsC, library, control)
#   return(output)
# }
#
# sims <- sims %>%
#   mutate(simresultIPW = future_map(data, sim_ipw))
#
# resultsIPW <- bind_rows(sims$simresultIPW)
#
# #Sim gcomp
# sim_gcomp <- function(df){
#   output <- gcomp(df, exposure, outcome, covarsZ, library, control)
#   return(output)
# }
#
# sims <- sims %>%
#   mutate(simresultgcomp = future_map(data, sim_gcomp))
#
# resultsgcomp <- bind_rows(sims$simresultgcomp)
#
# #Sim AIPW
# sim_aipw <- function(df){
#   output <- AIPW(df, exposure, outcome, covarsC, covarsZ, library, control)
#   return(output)
# }
#
# sims <- sims %>%
#   mutate(simresultAIPW = future_map(data, sim_aipw))
#
# resultsAIPW <- bind_rows(sims$simresultAIPW)

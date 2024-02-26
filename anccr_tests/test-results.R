library(R.matlab)
library(calmr)
library(testthat)
library(tidyverse)
rm(list = ls(all = TRUE))
source("anccr_tests/test_helpers.R")

# Define comparison map for results
rmap <- data.frame(
  "matlab" = c(
    "DA", "ANCCR", "PRC", "SRC", "NC",
    "Rs", "Delta", "Mij", "Mi", "Eij",
    "Ei", "q.src"
  ),
  "calmr" = c(
    "das", "anccr", "prc", "src", "nc",
    "rews", "delta", "m_ij", "m_i", "e_ij", "e_i", "qs"
  )
)

# Define comparison map for parameters
pmap <- data.frame(
  "matlab" = c(
    "Tratio", "alpha", "alpha.r", "beta",
    "k", "minimumrate", "samplingperiod",
    "threshold", "w"
  ),
  "calmr" = c(
    "t_ratio", "alpha", "alpha_reward", "betas",
    "ks", "minimum_rate", "sampling_interval",
    "thresholds", "w"
  )
)

#### Simple acquisition ####
matdat <- readMat("anccr_tests/simple_acquisition.mat")
names(matdat)

df <- data.frame(
  group = "G",
  p1 = c("100A>(US)"),
  r1 = TRUE
)
pars <- get_parameters(df, model = "ANCCR")
pars <- set_reward_parameters(pars, rewards = "US")
args <- make_experiment(df, parameters = pars, model = "ANCCR")
args <- put_eventlog(matdat$eventlog, args)
res <- raw_results(run_experiment(args, debug_t = -13))[[1]]

jres <- join_results(matdat, res, rmap)
p <- assert_joint_results(jres)
ggsave("anccr_tests/simple_aquisition.png",
  height = 8, width = 8.5, units = "in",
  plot = p
)

# params
join_parameters(matdat, pars, pmap)

#### Simple discrimination ####
matdat <- readMat("anccr_tests/simple_discrimination.mat")
names(matdat)

df <- data.frame(
  group = "G",
  p1 = c("30A>(US)/30B"),
  r1 = TRUE
)
pars <- get_parameters(df, model = "ANCCR")
pars <- set_reward_parameters(pars, rewards = "US")
args <- make_experiment(df, parameters = pars, model = "ANCCR")
args <- put_eventlog(matdat$eventlog, args)
res <- raw_results(run_experiment(args, debug_t = -13))[[1]]

jres <- join_results(matdat, res, rmap)
p <- assert_joint_results(jres)
ggsave("anccr_tests/simple_discrimination.png",
  height = 8, width = 8.5, units = "in",
  plot = p
)

#### Probabilistic rewards ####
matdat <- readMat("anccr_tests/probabilistic_rewards.mat")
df <- data.frame(
  group = "G",
  p1 = c("30A>(US)/30B/30C"),
  r1 = TRUE
)
pars <- get_parameters(df, model = "ANCCR")
pars <- set_reward_parameters(pars, rewards = "US")
args <- make_experiment(df, parameters = pars, model = "ANCCR")
args <- put_eventlog(matdat$eventlog, args)
res <- raw_results(run_experiment(args, debug_t = -97))[[1]]

jres <- join_results(matdat, res, rmap)
p <- assert_joint_results(jres)
ggsave("anccr_tests/probabilistic_rewards.png",
  height = 8, width = 8.5, units = "in",
  plot = p
)

#### Simple blocking ####
matdat <- readMat("anccr_tests/simple_blocking.mat")
df <- data.frame(
  group = "G",
  p1 = c("30A>(US)"),
  r1 = TRUE,
  p2 = c("30AB>(US)"),
  r2 = TRUE
)
pars <- get_parameters(df, model = "ANCCR")
pars <- set_reward_parameters(pars, rewards = "US")
pars$ks[] <- 0.01 # for some reason k is 100 times smaller
args <- make_experiment(df, parameters = pars, model = "ANCCR")
args <- put_eventlog(matdat$eventlog, args)
res <- raw_results(run_experiment(args, debug_t = -101))[[1]]

jres <- join_results(matdat, res, rmap)
p <- assert_joint_results(jres)
ggsave("anccr_tests/simple_blocking.png",
  height = 8, width = 8.5, units = "in",
  plot = p
)
join_parameters(matdat, pars, pmap)

#### Multiple associations ####
matdat <- readMat("anccr_tests/multiple_associations.mat")
df <- data.frame(
  group = "G",
  p1 = c("1(C1)>(US1)/1(C2)>(US2)/1(C3)>(C4)>(US3)/1(C5)/1(C6)/1(C7)/1(C8)"),
  r1 = TRUE
)
pars <- get_parameters(df, model = "ANCCR")
pars$t_constant <- 12 # uses a different time constant
pars$use_exact_mean <- 1
args <- make_experiment(df, parameters = pars, model = "ANCCR")
args <- put_eventlog(matdat$eventlog, args)
res <- raw_results(run_experiment(args, debug_t = -4))[[1]]
jres <- join_results(matdat, res, rmap)
p <- assert_joint_results(jres)
ggsave("anccr_tests/multiple_associations.png",
  height = 8, width = 8.5, units = "in",
  plot = p
)
join_parameters(matdat, pars, pmap)

# That's the most complicated design really
# Seems enough to me

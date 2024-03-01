rm(list = ls(all = TRUE))
library(calmr)
future::plan(future::sequential, split = TRUE)
# future::plan(future::multisession)
# calmr_verbosity(TRUE)
calmr_verbosity(FALSE)

# LONG EXP
df <- data.frame(
  group = c("Ctrl", "Exp"),
  phase1 = c("200C>(US)", "200A>(US)"),
  r1 = FALSE,
  phase2 = c("200AB>(US)", "200AB>(US)"),
  r2 = FALSE
)


pars <- get_parameters(df, model = "ANCCR")
args <- make_experiment(df,
  parameters = pars, model = "ANCCR",
  options = get_exp_opts(iterations = 1)
)

x <- run_experiment(args, debug_t = -403, parse = FALSE)
x <- parse(x)


ps <- plot(x)
names(ps)
ps[16]
ps[9]

raw <- raw_results(x)[[1]]
timestep <- 403
exp <- experience(x)[[1]]
exp[timestep, ]
event <- exp[timestep, "stimulus"]
m_ij <- raw$m_ij
e_ij <- raw$e_ij
imcts <- raw$imcts

# clearly
m_ij[timestep, , event] +
  (e_ij[timestep, ] - m_ij[timestep, , event]) *
    imcts[timestep, event] * pars$alpha



library(ggplot2)
res <- results(x)
res$m_ij %>%
  ggplot(aes(x = trial, y = value, colour = s2)) +
  stat_summary() +
  facet_grid(s1 ~ group)

plot(with(res$m_ij, value[group == "Ctrl" & s1 == "C" & s2 == "US"]))
# profvis::profvis({
# })

pr <- parsed_results(x)[[1]]

with(pr$m_ij, value[s1 == "C" & s2 == "US"])
pr$m_ij[pr$m_ij$s1 == "C" & pr$m_ij$s2 == "US", ]
plot(with(pr$m_ij, value[s1 == "C" & s2 == "US"]))

raw <- (m_ij[, "C", "US"])

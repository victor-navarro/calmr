rm(list = ls(all = TRUE))
future::plan(future::sequential, split = TRUE)
future::plan(future::multisession)
calmr_verbosity(TRUE)

# LONG EXP
df <- data.frame(
  group = "a",
  phase1 = c("200C>(US)", "200A>(US)"),
  r1 = FALSE,
  phase2 = c("200AB>(US)", "200AB>(US)"),
  r2 = FALSE
)

pars <- get_parameters(df, model = "ANCCR")
args <- make_experiment(df,
  parameters = pars, model = "ANCCR",
  options = get_exp_opts(iterations = 20)
)

x <- run_experiment(args)
plot(x, type = "cws")

plot(x, type = "e_i")

res <- results(x)

raw_res <- raw_results(x)[[1]]

plot(
  raw_res$e_ij[, "A"],
  raw_res$m_ij[, "A", "A"]
)

plot(
  raw_res$e_i[, "A"],
  raw_res$m_i[, "A"]
)

plot(
  with(res$e_i, value[s1 == "A"]),
  with(res$m_i, value[s1 == "A"])
)


plot(
  with(res$e_ij, value[s1 == "A"]),
  with(res$m_ij, value[s1 == "A" & s2 == "A"])
)


res$e_i %>%
  filter(trial < 10) %>%
  ggplot(aes(x = time, y = value, colour = s1)) +
  geom_line()
ggsave("elegibility.png", height = 3, width = 3, units = "in")

# apply(args@arguments, 1, function(i) do.call(ANCCR, i))

plot(x, type = "psrcs")

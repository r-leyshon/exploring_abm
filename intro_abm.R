"Finished part 11, part 12 below:
https://www.youtube.com/watch?v=Gpr7gEdiN0w"
# agent_no is a label
# state = S (susceptible) or E (exposed)
# I = infected, R = recovered, D = dead
# mixing describes interaction with other agents


# deps --------------------------------------------------------------------
library(here)

model_params <- data.frame(
  pop = 1000,
  no_days = 15,
  maxmix = 5,
  s2e = 0.2,
  e2i = 0.9,
  i2d = 0.1
)

# -------------------------------------------------------------------------
# define a population of agents
"
npop = number of agents
num_e = number of agents exposed
num_i = number of agents infected
"
create_agents <- function(npop, num_e, num_i, agents = data.frame()) {
  agents <- data.frame(
    agent_no = 1:npop,
    state = "S",
    mixing = runif(npop, 0, 1),
    days_exposed = 0,
    days_infected = 0,
    stringsAsFactors = FALSE
  )
  # ensure num_e are exposed
  agents$state[1:num_e] <- "E"
  # how long were they exposed (after 14 days they recover)
  agents$days_exposed[agents$state == "E"] <- rbinom(num_e, 13, 0.5) + 1
  # ensure num_i are infected
  infected_index <- num_e + 1:num_i
  agents$state[infected_index] <- "I"
  # how long were they infected?
  agents$days_infected[infected_index] <- rbinom(num_i, 12, 0.5) + 1

  print(sprintf("Population: %s", nrow(agents)))
  print(table(agents$state))
  return(agents)
}

agents <- create_agents(model_params$pop, num_e = 5, num_i = 5)


# helper funcs ------------------------------------------------------------
# avoiding complexity hooks
# this will be used on every individual in the population (defined as i in
# `run_encounters()`)
expose_agents <- function(agent_df, encounters, individual, param_df) {
  # alter conditions on encounters
  for (j in seq_along(encounters)) {
    encounter <- agent_df[encounters[j], ]
    if (encounter$state == "E") {
      # model an infection risk based on s2e param
      infection_risk <- runif(1, 0, 1)
      if (infection_risk > param_df$s2e) {
        # only infect if above 0.5
        agent_df$state[individual] <- "E"
      }
    }
  }
  return(agent_df$state[individual])
}



# -------------------------------------------------------------------------
# run the encounters
run_encounters <- function(agent_df, param_df) {
  # grab the susceptible agents' index
  npop <- nrow(agent_df)
  sus_agents <- (1:npop)[agent_df$state == "S"]
  # when mixing, mix only with susceptible or exposed, logic is that infected
  # or recvering should be isolating, dead people shouldn't be mixed with.
  sus_exposed_agents <- (1:npop)[agent_df$state == "S" | agent_df$state == "E"]
  for (i in sus_agents) {


    # determine agents propensity to mix
    mix_likelihood <- agent_df$mixing[i]
    # find the number of agents encountered
    # small No.s get rounded to 0 and throw downstream error, therefore
    # always encounter 1
    num_encountered <- round(
      (mix_likelihood * param_df$maxmix) + 1,
      digits = 0
    )
    # retrieve the agents encountered
    agents_encountered <- sample(
      # updating to limit the population members
      sus_exposed_agents,
      size = num_encountered,
      replace = TRUE,
      prob = agent_df$mixing[sus_exposed_agents]
    )
    # alter conditions on encounters
    agent_df$state[i] <- expose_agents(
      agent_df, agents_encountered,
      individual = i, param_df = model_params
    )
  }
  # grab the exposed agents and increment their exposure duration
  exposed <- (1:npop)[agent_df$state == "E"]
  agent_df$days_exposed[exposed] <- agent_df$days_exposed[exposed] + 1
  # Recover on the 15th day
  recovering <- (1:npop)[agent_df$days_exposed > 14]
  agent_df$state[recovering] <- "R"
  # change exposed people to infected
  infecting <- (1:npop)[agent_df$state == "E" & agent_df$days_exposed > 3]
  # give a random chance of becoming infected
  for (i in infecting) {
    infection_risk <- runif(1, 0, 1)
    if (infection_risk > param_df$e2i) {
      agent_df$state[i] <- "I"
    }
  }
  # increment the time infected
  inf_incrementing <- (1:param_df$pop)[agent_df$state == "I"]
  agent_df$days_infected[inf_incrementing] <- agent_df$days_infected[
    inf_incrementing
  ] + 1
  # Infected surviving to day 15 will recover
  inf_recovering <- (1:param_df$pop)[
    agent_df$state == "I" & agent_df$days_infected > 14
  ]
  agent_df$state[inf_recovering] <- "R"
  # if not recovered, model chance of death
  still_infected <- (1:param_df$pop)[
    agent_df$state == "I" & agent_df$days_infected < 15
  ]
  agent_df$state[still_infected] <- ifelse(
    runif(length(still_infected), 0, 1) > param_df$i2d,
    "I",
    "D"
  )

  return(agent_df)
}

# moving agents through time -----------------------------------------------

run_time <- function(agent_table, out_df, param_df) {
  message(sprintf(
    "moving %s people through %s days",
    param_df$pop, param_df$no_days
  ))
  for (k in 1:param_df$no_days) {
    agent_table <- run_encounters(
      agent_df = agent_table, param_df = param_df
    )
    # format should be df as input
    # update output df
    out_df$E[k] <- length(agent_table$state[agent_table$state == "E"])
    out_df$S[k] <- length(agent_table$state[agent_table$state == "S"])
    out_df$I[k] <- length(agent_table$state[agent_table$state == "I"])
    out_df$R[k] <- length(agent_table$state[agent_table$state == "R"])
    out_df$D[k] <- length(agent_table$state[agent_table$state == "D"])
  }
  return(out_df)
}

output_df <- run_time(
  agents,
  out_df = data.frame(
    "E" = rep(0, model_params$no_days),
    "S" = rep(0, model_params$no_days),
    "I" = rep(0, model_params$no_days),
    "R" = rep(0, model_params$no_days),
    "D" = rep(0, model_params$no_days)
  ),
  model_params
)

# visualise ---------------------------------------------------------------
linwt <- 2.5

png(here("www/output_plot.png"), width = 1920, height = 1080)
plot(
  1:model_params$no_days, output_df$S,
  main = "Agent-Based Model of Disease Exposure",
  xlab = "Model duration (days)", ylab = "Number of agents",
  type = "l", col = "purple",
  ylim = c(0, model_params$pop),
  lwd = linwt
)
mysubtitle <- sprintf(
  "%s agents over %s days",
  model_params$pop, model_params$no_days
)
mtext(side = 3, line = 0.6, at = -0.09, adj = -7.3, cex = 0.9, mysubtitle)
lab_nudge <- 0.2
text(
  x = model_params$no_days + lab_nudge, y = tail(output_df$S, 1),
  label = "S", col = "purple"
)
lines(1:model_params$no_days, output_df$E, col = "orange", lwd = linwt)
text(
  x = model_params$no_days + lab_nudge, y = tail(output_df$E, 1),
  label = "E", col = "orange"
)
lines(1:model_params$no_days, output_df$I, col = "red", lwd = linwt)
text(
  x = model_params$no_days + lab_nudge, y = tail(output_df$I, 1),
  label = "I", col = "red"
)
lines(1:model_params$no_days, output_df$R, col = "seagreen", lwd = linwt)
text(
  x = model_params$no_days + lab_nudge, y = tail(output_df$R, 1),
  label = "R", col = "seagreen"
)
lines(1:model_params$no_days, output_df$D, col = "black", lwd = linwt)
text(
  x = model_params$no_days + lab_nudge, y = tail(output_df$D, 1),
  label = "D", col = "black"
)
dev.off()

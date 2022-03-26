"Finished part 9, part 10 below:
https://www.youtube.com/watch?v=0ZjT5MEARkk"
# agent_no is a label
# state = S (susceptible) or E (exposed)
# I = infected, R = recovered, D = dead
# mixing describes interaction with other agents


# deps --------------------------------------------------------------------

pop <- 10
no_days <- 100

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

agents <- create_agents(15, num_e = 5, num_i = 5)

# collecting output -------------------------------------------------------
# will collect the table summaries
output_matrix <- matrix(0, 2, no_days)

# helper funcs ------------------------------------------------------------
# avoiding complexity hooks
# this will be used on every individual in the population (defined as i in
# `run_encounters()`)
expose_agents <- function(agent_df, encounters, individual) {
  # alter conditions on encounters
  for (j in seq_along(encounters)) {
    encounter <- agent_df[encounters[j], ]
    if (encounter$state == "E") {
      # model an infection risk of 0.5
      infection_risk <- runif(1, 0, 1)
      if (as.logical(round(infection_risk, 0))) {
        # only infect if above 0.5
        agent_df$state[individual] <- "E"
      }
    }
  }
  return(agent_df$state[individual])
}



# -------------------------------------------------------------------------
# run the encounters
run_encounters <- function(agent_df, npop) {
  for (i in 1:npop) {
    # determine agents propensity to mix
    mix_likelihood <- agent_df$mixing[i]
    # find the number of agents encountered
    # small No.s get rounded to 0 and throw downstream error, therefore
    # always encounter 1
    num_encountered <- round((mix_likelihood * 3) + 1, digits = 0)
    # retrieve the agents encountered
    agents_encountered <- sample(
      1:npop,
      size = num_encountered,
      replace = TRUE,
      prob = agent_df$mixing
    )
    # # alter conditions on encounters
    agent_df$state[i] <- expose_agents(
      agent_df, agents_encountered,
      individual = i
    )
  }
  # grab the exposed agents and increment their exposure duration
  exposed <- (1:npop)[agent_df$state == "E"]
  agent_df$days_exposed[exposed] <- agent_df$days_exposed[exposed] + 1
  # Recover on the 8th day
  recovering <- (1:npop)[agent_df$days_exposed > 14]
  agent_df$state[recovering] <- "R"
  # change exposed people to infected
  infecting <- (1:npop)[agent_df$state == "E" & agent_df$state > 3]
  # give a random chance of becoming infected
  for (i in infecting) {
    infection_risk <- as.logical(round(runif(1, 0, 1), 0))
    if (infection_risk) {
      agent_df$state[i] <- "I"
    }
  }

  return(agent_df)
}

# moving agents through time -----------------------------------------------

run_time <- function(agent_table, out_df, npop, days) {
  message(sprintf("moving %s people through %s days", npop, days))
  for (k in 1:days) {
    agent_table <- run_encounters(agent_df = agent_table, npop = npop)
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
    "E" = rep(0, no_days),
    "S" = rep(0, no_days),
    "I" = rep(0, no_days),
    "R" = rep(0, no_days),
    "D" = rep(0, no_days)
  ),
  npop = pop, days = no_days
)

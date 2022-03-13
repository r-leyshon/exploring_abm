"starting part 5:
https://www.youtube.com/watch?v=uAeSykgXnhg"
# agent_no is a label
# state = S (susceptible) or E (exposed)
# mixing describes interaction with other agents


# deps --------------------------------------------------------------------

pop = 10
no_days <- 10

# -------------------------------------------------------------------------
# define a population of agents

create_agents <- function(npop, agents = data.frame()){
  for (i in 1:npop) {
    agenti <- data.frame(
      agent_no = i,
      state = if(i == 1){"E"} else {"S"},
      mixing = runif(1, 0, 1)
      )
    agents <- rbind(agents, agenti)
  }
  print(sprintf("Population: %s", nrow(agents)))
  print(table(agents$state))
  return(agents)
  
}

agents <- create_agents(pop)

# collecting output -------------------------------------------------------
# will collect the table summaries
output_matrix <- matrix(0, 2, no_days)


# -------------------------------------------------------------------------
# run the encounters
run_encounters <- function(agent_df, npop){
  for(i in 1:npop){
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
    # alter conditions on encounters
    for(j in 1:length(agents_encountered)){
      encounter <- agent_df[agents_encountered[j], ]
      if(encounter$state == "E"){
        # model an infection risk of 0.5
        infection_risk <- runif(1, 0, 1)
        if(as.logical(round(infection_risk, 0))) {
          # only infect if above 0.5
          agent_df$state[i] <- "E"
        }
      }
    }
  }
  return(agent_df)
}
# agents <- run_encounters(agents, npop = pop)

# moving agents through time -----------------------------------------------
# matrix to collect table output
# output_df <- data.frame("E" = 0, "S" = 0)

run_time <- function(agent_table, out_df, npop, days){
  message(sprintf("moving %s people through %s days", npop, days))
  agents_in_time = agent_table
  for (k in 1:days) {
    agents_in_time <- run_encounters(agent_df = agents_in_time, npop = npop)
    # format should be df as input
    tab_out <- table(agents_in_time$state)
    # if first iter, overwrite row
    out_df[k, ] <- tab_out
      
  }
  return(out_df)
}

output_df = run_time(
  agents,
  out_df = data.frame("E" = 0, "S" = 0),
  npop = pop, days = no_days
  )

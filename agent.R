# Helper function to create agents as simple vectors of their parameters:
createAgentVector <- function(sigmaF, sigmaC, sigmaN, kMax, lMin, lMax) {
   
  c(sigmaF * runif(1),    # Fundamentalist weight
    sigmaC * runif(1),    # Chartist weight Randomized Spread size
    sigmaN * runif(1),    # Noise Weight
    sample(lMin:lMax, 1), # Horizon range for Chartist momentum rules
    kMax * runif(1),      # BId/Ask spread parameter
    -1)
}

# Helper to get Agent parameters
getAgentParameter <- function(agent, param) {
  agent[, param]
}

# Helper to set Agent parameters
setAgentParameter <- function(agent, param, value) {
  switch (
    param,
    "Fundamentalist" = { agent[1] <- value },
    "Chartist" = { agent[2] <- value },
    "Noise" = { agent[3] <- value },
    "Horizon" = { agent[4] <- value },
    "Spread" = { agent[5] <- value },
    "Side" = { agent[6] <- value }
  )
  
  #Return Agent
  agent
}


# Helper function to create agents as simple vectors of their parameters:
# Fundamentalist Chartist and Noise Weights, Horizon for momentum rules, Randomized Spread size
createAgentVector <- function(sigmaF, sigmaC, sigmaN, kMax, lMin, lMax) {
  c(sigmaF * runif(1),
    sigmaC * runif(1),
    sigmaN * runif(1),
    sample(lMin:lMax, 1),
    kMax * runif(1),
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
  
  agent
}


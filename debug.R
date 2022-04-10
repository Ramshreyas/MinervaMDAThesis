library(somebm)
library(scatterplot3d)
library(ggfortify)
library(orderbook)

source("forecast.R")
source("agent.R")
source("orderbook.R")

tMax <- 1000

spot_price <- gbm(x0=100, mu=1, sigma=0.5, t0=0, t=1, n=tMax)

# Set number of agents
nAgents <- 200

# Fundamentalist weight
sigmaF <- 0

# Chartist weight
sigmaC <- 10

# Noise weight
sigmaN <- 1

# random component of spread
kMax <- 0.5

# horizons for momentum rules - bounds for how far back should they go to estimate trend
lMin <- 1
lMax <- 5

# Initialize traders list
traders <- data.frame(matrix(ncol = 6, nrow = 0))
names(traders) <- c("Fundamentalist", "Chartist", "Noise", "Horizon", "Spread", "Side")

# Create traders
for (i in 1:nAgents) {
  traders[i,] <- createAgentVector(sigmaF, sigmaC, sigmaN, kMax, lMin, lMax)
}

perp_prices <- spot_price + runif(tMax+1, -1, 1)

premia <- perp_prices - spot_price

bias <- 0.5

close_position_probability <- 0.05

sigmaE <- 0.05

t <- 250

tau <- 4

cohortSize <- 4

ob <- orderbook("orderbook.txt")

sims <- data.frame(matrix(ncol = 10, nrow = 1001))
premia_sims <- data.frame(matrix(ncol = 10, nrow = 1001))

for (sigmaN in 1:10) {
  
  for(i in (t - 2*tau):(t-1)) {
    if(i %% 2 == 0) {
      ob <- add.order(ob, spot_price[i] - sample(0:10, 1), 1, type = "BID", time = t, id = i)
    } else {
      ob <- add.order(ob, spot_price[i] + sample(0:10, 1), 1, type = "ASK", time = t, id = i)
    } 
  }

  for (t in 250:tMax) {
    # Select random traders
    cohort <- traders[sample(1:nAgents, cohortSize), ]
    
    newPrice <- NULL
    
    # Iteratively make bids and asks on the same price point, exit if trade occurs
    for (row in 1:cohortSize) {
      # Get trader
      trader <- cohort[row, ]
      
      # Get order
      order <- getOrder(trader, premia, spot_price, t = t, bias, close_position_probability, sigmaE = sigmaE)
      price <- order[[2]]
      size <- order[[3]]
      
      if(price == 0) next
      
      # Add to book as market or limit
      result <- addOrder(ob, order, t)
      ob <- result[[1]]
      newPrice <- result[[2]]
      
      # Check if trade occurs
      if (!is.null(newPrice)) {
        break()
      }
    }
    
    # If no trade occurred, set price to mid point
    if (is.null(newPrice)) {
      # Update perp price as midpoint between best ask and best bid
      newPrice <- mid.point(ob)
    }
    
    if(is.na(newPrice)) {
      #newPrice <- spot_price[t] + runif(1)
      newPrice <- perp_prices[t]
    }
    
    # Update perp_prices
    perp_prices[t+1] <- newPrice
    
    # Update premia
    premia[t+1] <- perp_prices[t+1] - spot_price[t+1]
    
    # Clean old trades 
    ob <- removeOldOrders(ob, tau, t)
  }

  sims[, sigmaN] <- perp_prices
  premia_sims[, sigmaN] <- premia
  
}

print("SIMS DONE")

for (i in 1:10) {
  premia <- sims[,i] - spot_price

  title <- paste0("Chartist : Noise :: ", sigmaC, " : ", i)

  plot(spot_price[250:tMax], type = "l", col = "red", main = title, xlab = "Timestep", ylab = "Price")
  lines(sims[250:tMax, i], col = "green")
  legend("topleft", legend=c("Spot price", "Perp price"),
         col=c("red", "green"), lty=c(1,1), cex=1)

  plot(premia[250:tMax], type = "l", main = "Premium", xlab = "Timestep", ylab = "Price")
  abline(h = 0)
}

print("Plots done")


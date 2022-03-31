library(somebm)
library(scatterplot3d)
library(ggfortify)
library(orderbook)

source("forecast.R")
source("agent.R")
source("orderbook.R")

spot_price <- gbm(x0=100, mu=1, sigma=1, t0=0, t=1, n=1000)

# Set number of agents
nAgents <- 100

# Fundamentalist weight
sigmaF <- 1

# Chartist weight
sigmaC <- 10

# Noise weight
sigmaN <- 1

# random component of spread
kMax <- 0.5

# horizons for momentum rules - bounds for how far back should they go to estimate trend
lMin <- 5
lMax <- 50

# Initialize traders list
traders <- data.frame(matrix(ncol = 6, nrow = 0))
names(traders) <- c("Fundamentalist", "Chartist", "Noise", "Horizon", "Spread", "Side")

# Create traders
for (i in 1:nAgents) {
  traders[i,] <- createAgentVector(sigmaF, sigmaC, sigmaN, kMax, lMin, lMax)
}

perp_prices <- spot_price + runif(1001, -1, 1)

premia <- perp_prices - spot_price

bias <- 0.5

close_position_probability <- 0.2

sigmaE <- 0.05

t <- 250

tMax <- 1000

tau <- 50

ob <- orderbook("orderbook.txt")

plot(spot_price)

for(i in 1:20) {
  if(i %% 2 == 0) {
    ob <- add.order(ob, spot_price[t-1] - sample(0:50, 1), 1, type = "BID", time = t, id = 200 + i)
  } else {
    ob <- add.order(ob, spot_price[t-1] + sample(0:50, 1), 1, type = "ASK", time = t, id = 200 + i)
  } 
}

show(ob)

for (t in 250:tMax) {
  # Select random trader
  trader <- traders[sample(1:nAgents, 1), ]
  
  # Get order
  order <- getOrder(trader, premia, perp_prices, t = t, bias, close_position_probability, sigmaE = sigmaE)
  price <- order[[2]]
  size <- order[[3]]
  
  # Add to book as market or limit
  result <- addOrder(ob, order, t)
  ob <- result[[1]]
  tradePrice <- result[[2]]
  
  # Get best Ask and Bid, handle NA
  best_ask <- best.ask(ob)[["price"]]
  if(is.na(best_ask)) { best_ask <- 0 }
  best_bid <- best.bid(ob)[["price"]]
  if(is.na(best_bid)) { best_bid <- 0 }
  
  # Check if trade occurs
  if (is.null(tradePrice)) {
    # Update perp price as midpoint between best ask and best bid
    tradePrice <- mid.point(ob)
    perp_prices[t+1] <- tradePrice
    
  } else {
    # Update perp_price as trade price
    perp_prices[t+1] <- tradePrice
    
  }
  
  # Update premia
  premia[t+1] <- perp_prices[t+1] - spot_price[t+1]
  
  # Clean old trades 
  ob <- removeOldOrders(ob, tau, t)
}

trader <- traders[sample(1:nAgents, 1), ]

trader <- setAgentParameter(trader, "Side", 0)

trader <- setAgentParameter(trader, "Side", 1)

trader

k <- runif(1, 0, getAgentParameter(trader, "Spread"))

price_forecast <- getForecast(trader, perp_prices, sigmaE, t)

order <- getPositionalOrder(price_forecast, perp_prices[t], trader, k)

order

adjusted_premia <- premia + abs(min(premia)) + 1

forecast <- getForecast(trader, adjusted_premia, sigmaE, t)

order <- getFundingOrder(forecast, price_forecast, adjusted_premia[t], trader, k)

order

result <- addOrder(ob, order, t)

result

mid.point(ob)

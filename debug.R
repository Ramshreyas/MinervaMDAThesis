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

nSims <- 400

tau <- 10

ob <- orderbook("orderbook.txt")

plot(spot_price)

for(i in 1:5) {
  ob <- add.order(ob, spot_price[t-1] - sample(0:10, 1), 1, type = "BID", time = t)
  ob <- add.order(ob, spot_price[t-1] + sample(0:10, 1), 1, type = "ASK", time = t) 
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

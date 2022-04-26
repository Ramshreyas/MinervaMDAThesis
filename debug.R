library(somebm)
library(scatterplot3d)
library(ggfortify)
library(orderbook)
library(qcc)  

source("forecast.R")
source("agent.R")
source("orderbook.R")

stats <- list()
count <- 1

for(bias in c(0.05, 0.1, 0.15, 0.2, 0.25, 0.30, 0.35, 0.4, 0.4, 0.5)){

nSims = 100

perp_sims = data.frame(matrix(nrow = 1001, ncol = nSims))
spot_sims = data.frame(matrix(nrow = 1001, ncol = nSims))
prem_sims = data.frame(matrix(nrow = 1001, ncol = nSims))

for(n in 1:nSims) {

tMax <- 1000

spot_price <- gbm(x0=100, mu=1, sigma=0.5, t0=0, t=1, n=tMax)

# Set number of agents
nAgents <- 200

# Fundamentalist weight
sigmaF <- 0

# Chartist weight
sigmaC <- 10

# Noise weight
sigmaN <- 10

# random component of spread
kMax <- 0.5

# horizons for momentum rules - bounds for how far back should they go to estimate trend
lMin <- 1
lMax <- 5

# Initialize traders list
traders <- data.frame(matrix(ncol = 6, nrow = 0))
names(traders) <- c("Fundamentalist", "Chartist", "Noise", "Horizon", "Spread", "Side")

#bias <- 0.5

close_position_probability <- 0.05

sigmaE <- 0.05

t <- 250

tau <- 8

cohortSize <- 4

perp_prices <- spot_price + runif(tMax+1, -1, 1)
perp_prices[250:tMax + 1] <- rep(0,tMax + 1)

premia <- perp_prices - spot_price
premia[250:tMax + 1] <- rep(0,tMax + 1)

# Create traders
for (i in 1:nAgents) {
  traders[i,] <- createAgentVector(sigmaF, sigmaC, sigmaN, kMax, lMin, lMax)
}

ob <- orderbook("orderbook.txt")

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

perp_sims[,n] <- perp_prices
spot_sims[,n] <- spot_price
prem_sims[,n] <- premia

}

p <- premia[250:tMax]

title <- paste0("Chartist:Noise = ", sigmaC, " : ", sigmaN)

plot(spot_price[250:tMax], type = "l", col = "red", main = title, xlab = "Timestep", ylab = "Price")
lines(perp_prices[250:tMax], col = "green")
legend("topleft", legend=c("Spot price", "Perp price"),
       col=c("red", "green"), lty=c(1,1), cex=1)

qcc(data = p,
    type = "xbar.one",
    title = "Shewhart Chart of Premiums", # Replacement title
    xlab = "Timestep",
    ylab = "Premium",
    digits = 2, # Limit the significant figures
    plot = TRUE)

print("Sims done")

qs <- data.frame(matrix(nrow = 100, ncol = 6))
names(qs) <- c("center", "stddev", "lcl", "ucl", "violations", "runs")

for (i in 1:100) {
  q <- qcc(prem_sims[,i], type = "xbar.one", digits = 2, plot = FALSE)
  
  center <- q$center
  stddev <- q$std.dev
  lcl <- q$limits[1]
  ucl <- q$limits[2]
  violations <- length(q$violations[[1]])
  runs <- length(q$violations[[2]])
  qs[i,] <- c(center, stddev, lcl, ucl, violations, runs)
}

stats[[count]] <- qs

count <- count + 1

}

centers <- stddevs <- lcls <- ucls <- violations <- runs <- c()

for(s in 1:(count-1)) {
  centers <- append(centers, mean(stats[[s]]$center))
  stddevs <- append(stddevs, mean(stats[[s]]$stddev))
  lcls <- append(lcls, mean(stats[[s]]$lcl))
  ucls <- append(ucls, mean(stats[[s]]$ucl))
  violations <- append(violations, mean(stats[[s]]$violations))
  runs <- append(runs, mean(stats[[s]]$runs))
}

plot(centers, type = 'l', ylim = c(-15,15), 
     main = 'Varying Bias from 0.05:0.5 - Center, LCL and UCL',
     xlab = 'Bias',
     ylab = 'value',
     xaxt = 'n')
axis(1, at = 1:10, labels = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.30, 0.35, 0.4, 0.4, 0.5))
text(1.5, mean(centers) + 2, "Center")
lines(lcls, col = 'red')
text(1.5, mean(lcls) + 2, "LCL")
lines(ucls, col = 'red')
text(1.5, mean(ucls) + 2, "UCL")

plot(violations, type = 'l', ylim = c(0, 550), 
     main = 'Varying Bias from 0.05:0.5 - Violations and Runs',
     xlab = 'Bias',
     ylab = 'value',
     xaxt = 'n')
axis(1, at = 1:10, labels = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.30, 0.35, 0.4, 0.4, 0.5))
text(2, mean(violations) + 30, "Violations")
lines(runs, col = 'red')
text(2, mean(runs), "Runs")

save(spot_sims, file = "Figures/bias_sweep/spot_sims.RDA")
save(perp_sims, file = "Figures/bias_sweep/perp_sims.RDA")
save(prem_sims, file = "Figures/bias_sweep/prem_sims.RDA")
save(qs, file = "Figures/bias_sweep/qs.RDA")
save(stats, file = "Figures/bias_sweep/stats.RDA")
save(centers, file = "Figures/bias_sweep/centers.RDA")
save(lcls, file = "Figures/bias_sweep/lcls.RDA")
save(ucls, file = "Figures/bias_sweep/ucls.RDA")
save(violations, file = "Figures/bias_sweep/violations.RDA")
save(runs, file = "Figures/bias_sweep/runs.RDA")

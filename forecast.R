# Function that generates a forecast given a trader, the spot price history and the perp price history
getForecast <- function(trader, prices, sigmaE, t) {
  
  if(is.na(prices[t])) { 
    print("Error! Price:")
    print(prices[t])
    stop()
  }
  
  # The Fundamentalist believes the price reverts to the mean. The return expectation is scaled by the number of ticks
  fundamental_forecast <- log(mean(prices)/prices[t])
  
  # The chartist averages the sum of simple returns [P(t) - P(t-1)]/P(t-1) over a time horizon randomly chosen from [lMin, lMax]
  l <- sample(lMin:lMax, 1)
  horizon <- t - l
  simple_returns <- (prices[t:horizon] - prices[(t-1):(horizon - 1)])/prices[(t-1):(horizon - 1)]
  chartist_forecast <- (sum(simple_returns)/l)
  
  # The noise trader produces a random forecast
  noise_forecast <- sigmaE * runif(1)
  
  # Composite forecast
  fundamental_weight <- getAgentParameter(trader, "Fundamentalist")
  chartist_weight <- getAgentParameter(trader, "Chartist")
  noise_weight <- getAgentParameter(trader, "Noise")
  total_weight <- fundamental_weight + chartist_weight + noise_weight
  
  # The combined weighted expectation 
  price_expectation <- (fundamental_weight*fundamental_forecast + chartist_weight*chartist_forecast + noise_weight*noise_forecast)/total_weight

  # Return the forecast
  forecast <- prices[t]*exp(price_expectation)
  # print("Fundamental Forecast")
  # print(fundamental_forecast)
  # print("Chartist Forecast")
  # print(chartist_forecast)
  # print("Noise Forecast")
  # print(noise_forecast)
  # print("Price[t]: ")
  # print(prices[t])
  # print("price_expectation: ")
  # print(price_expectation)
  # print("Forecast: ")
  # print(forecast)
  forecast
  
}
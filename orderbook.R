# Get Agent Order
getOrder <- function(trader, premia, perp_prices, t, bias, close_position_probability, sigmaE) {
  # Get agent params  
  k <- runif(1, 0, getAgentParameter(trader, "Spread"))
  
  # Randomly close positions for some traders and exit (allowing for trader distribution to be dynamic over time)
  if (runif(1) < close_position_probability) {
    setAgentParameter(trader, "Side", -1)
    print("Exiting")
    return(list(trader, 0, 0))
  }
  
  # Generate price forecast
  price_forecast <- getForecast(trader, perp_prices, sigmaE, t)
  
  # Get trader side
  side <- getAgentParameter(trader, "Side")
  
  # If new trader, randomly assign a side
  if (side == -1) {
    trader <- setAgentParameter(trader, "Side", sample(c(0,1), 1))
    print("Randomly assigning side")
  }
  
  # If long trader 
  if (getAgentParameter(trader, "Side") == 0) {
    # Assign basis or positional trade based on bias
    if (runif(1) < bias) {
      print("Long trader positional")
      # Generate and return positional order
      return(getPositionalOrder(price_forecast, perp_prices[t], trader, k))
      
    } else {
      print("Long trader funding")
      # Adjust premia by adding lowest negative value to make all data positive
      adjusted_premia <- premia + abs(min(premia[1:t])) + 1
      
      # Generate funding forecast
      forecast <- getForecast(trader, adjusted_premia, sigmaE, t)
      
      # Generate and return order
      return(getFundingOrder(forecast, price_forecast, adjusted_premia[t], trader, k))
    }
    
  # Else short trader
  } else { 
    # Assign basis or arbitrage trade based on bias
    if (runif(1) > bias) {
      print("Short trader positional")
      # Generate and return short positional order
      return(getPositionalOrder(price_forecast, perp_prices[t], trader, k))
    } else {
      print("Short trader funding")
      # Adjust premia by adding lowest negative value to make all data positive
      adjusted_premia <- premia + abs(min(premia[1:t])) + 1
      
      # Generate funding forecast
      forecast <- getForecast(trader, adjusted_premia, sigmaE, t)
      
      # Generate and return order
      return(getFundingOrder(forecast, price_forecast, adjusted_premia[t], trader, k))
    }
  }
}

# Helpers
getPositionalOrder <- function(forecast, price, trader, kMax) {
  
  # Get side of market
  side <- getAgentParameter(trader, "Side")
  
  if (forecast > price & side == 0 | forecast < price & side == 1) {
    print("BUY")
    # Return Buy order
    return(list(trader, forecast*(1-kMax), 1))
  } else {
    print("SELL")
    # Return Sell order
    return(list(trader, -1*forecast*(1+kMax), 1))
  }
}

getFundingOrder <- function(forecast, price, premium, trader, kMax) {
  
  # Get side of market
  side <- getAgentParameter(trader, "Side")
  
  if (forecast < premium & side == 0 | forecast > premium & side == 1) {
    print("BUY")
    # Return Buy order
    return(list(trader, abs(price)*(1-kMax), 1))
  } else {
    print("SELL")
    # Return Sell order
    return(list(trader, -1*abs(price)*(1+kMax), 1))
  }
}

addOrder <- function(ob, order, t) {
  price <- order[[2]]
  size <- order[[3]]
  print("Price:")
  print(price)
  
  # Buy
  if(price > 0) {
    
    # Get best ask
    best_ask <- best.ask(ob)[["price"]] 
    
    # If price less than current best ask
    if (is.na(best_ask) | price < best_ask) {
      
      # Set limit order
      ob <- add.order(ob, price, size, type = "BID", time = t, id = t)
      return(list(ob, NULL))
      
    } else {
      
      # Else add market order
      ob <- market.order(ob, size, "BUY")
      return(list(ob, best_ask))
      
    }
  # Sell
  } else if(price < 0) {
    
    # Remove negative sign for sell
    price <- abs(price)
    
    # Get best bid
    best_bid <- best.bid(ob)[["price"]] 
    
    # If price greater than current best bid
    if (is.na(best_bid) | price > best_bid) {
      
      # Set limit order
      ob <- add.order(ob, price, size, type = "ASK", time = t, id = t)
      return(list(ob, NULL))
      
    } else {
      
      # Else add market order
      ob <- market.order(ob, size, "SELL")
      return(list(ob, best_bid))
      
    }
    # Exit market
  } else {
    
    return(list(ob, NULL))
    
  }
}

removeOldOrders <- function(ob, tau, t) {
  if(t > tau) {
    for(i in 1:t - tau) {
      # If so, remove
      ob <- remove.order(ob, i)
    } 
  }
  
  ob
}
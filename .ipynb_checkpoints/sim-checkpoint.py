# -*- coding: utf-8 -*-
"""
Created on Fri May 20 12:22:22 2016

@author: LeBaron
"""
# Author: Blake LeBaron
# This code is based on Chiarella/Iori, Quant Finance, 2002, vol 2, 346-353
# Main market simulation

# import usual Python helpers
import numpy as np
import numpy.random as rnd
from scipy.stats import kurtosis
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import scipy.stats as stats
import statsmodels.tsa.stattools as stattools

# import primary objects
from agent import agent
from forecasts import forecasts
from orderBook import orderBook

# helper routine for basic autocorrelations
def autocorr(x,m):
    n = len(x)
    v = x.var()
    x2 = x-x.mean()
    r = np.correlate(x2,x2,mode="full")[(n-1):(n+m+1)]
    result = r/(n*v)
    return result
    
def normhist(xdata,nbins):
    n, bins, patches = plt.hist(xdata,nbins,normed=1,facecolor='green', alpha=0.5)
    mu = np.mean(xdata)
    sigma = np.std(xdata)
    y = mlab.normpdf(bins,mu,sigma)
    plt.plot(bins,y,'r')
    
def reversion(xdata):
    xdiff = np.diff(xdata)
    rstat = np.corrcoef(xdata[0:-1],xdiff)
    return rstat[0,1]
    
def pltPrice(prices):
    plt.clf()
    plt.plot(prices)
    plt.xlabel('Period(t)')
    plt.ylabel('Price')
    plt.grid()
    
# set default parameters
nAgents = 1000
Tinit = 1000
Tmax = 50000
Lmin = 5
Lmax = 50
pf = 1000.
deltaP = 0.1
# deltaP = 0.5
sigmae = 0.01
kMax = 0.5
tau = 50
# tau = 10
runType = 0
if runType == 0:
    sigmaF = 0.
    sigmaM = 0.
    sigmaN = 1.
if runType == 1:
    sigmaF = 1.
    sigmaM = 0.
    sigmaN = 1.
if runType == 2:
    sigmaF = 1.
    sigmaM = 10.
    sigmaN = 1.

# time length for ticks per time period
deltaT = 100
# holder list for agent objects
agentList = []
# price, return, and volume time series
price = pf*np.ones(Tmax+1)
ret   = np.zeros(Tmax+1)
totalV = np.zeros(Tmax+1)
rprice = np.zeros((Tmax+1)/100)

# create agents in list of objects
for i in range(nAgents):
    agentList.append(agent(sigmaF,sigmaM,sigmaN,kMax,Lmin,Lmax))
# create set of 
forecastSet = forecasts(Lmax,pf,sigmae)
# create order book
marketBook = orderBook(600.,1400.,deltaP)
# set up initial prices

price[0:Tinit] = pf*(1.+0.001*np.random.randn(Tinit))
ret[0:Tinit] = 0.001*np.random.randn(Tinit)

for t in range(Tinit,Tmax):
    # update all forecasts
    forecastSet.updateForecasts(t,price[t],ret)
    tradePrice = -1
    # draw random agent
    randomAgent = agentList[np.random.randint(1,nAgents)]
    # set update current forecasts for random agent
    randomAgent.updateFcast(forecastSet,price[t],tau)
    # get demands for random agent
    randomAgent.getAgentOrder(price[t])
    # potential buyer
    if randomAgent.pfcast > price[t]:
        # add bid or market order 
        tradePrice = marketBook.addBid(randomAgent.bid,1.,t)
    else:
        # seller: add ask, or market order
        tradePrice = marketBook.addAsk(randomAgent.ask,1.,t)
    # update price and volume
    # no trade
    if tradePrice == -1:
        price[t+1]=(marketBook.bestBid + marketBook.bestAsk)/2.
        totalV[t+1]=totalV[t]
    else:
        # trade
        price[t+1] = tradePrice
        totalV[t+1] = totalV[t]+1.
    # returns
    ret[t+1]=np.log(price[t+1]/price[t])
    # clear book
    if(rnd.rand()<0.2):
        marketBook.cleanBook(t,tau)

# generate long run values for time series    
rVol    = np.diff(totalV[range(Tinit+deltaT,Tmax,deltaT)])
rPrice = price[ range(Tinit+deltaT,Tmax,deltaT)]
rret   = np.diff(np.log(rPrice))

plt.rcParams['lines.linewidth']=2
plt.rcParams['font.size']=14

print(np.var(rPrice))
print(np.var(rret))
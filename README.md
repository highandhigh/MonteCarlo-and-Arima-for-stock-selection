# MonteCarlo and Arima for stock selection

See an article about the code on <a href="http://www.tulipquant.com/2016/06/18/montecarlo-and-arima-for-stock-selection/">tulipQuant</a>.

The idea of the trading algorithm will be the following:
<ul>
 	<li>Given a day, for each stock of a certain index, select the best ARIMA model for this stock.</li>
 	<li>Then, simulate different possible trajectories of the price of this stock.</li>
 	<li>Now, estimate the probability of the stock going up, dividing the number of trajectories that actually increased their price, by the total number of trajectories. This way of estimating probabilities is called <a href="https://en.wikipedia.org/wiki/Monte_Carlo_method">Monte Carlo method</a>.</li>
 	<li>Now, since this process was repeated for a big number of stocks, we may sort them appropiately. If $latex p_i$ is the probability of a price rise for stock $latex i$, this stock would be a good candidate to buy if $latex p_i$ was close to 1. However, if it is close to 0, it would also be a good candidate, but for short selling. Thus, we pick the stock such that $latex |p_i-0.5|$ is the maximum.</li>
 	<li>Now, once we have calculated the stock where the maximum is attained, we go long or short depending if the probability was greater or less than 0.5.</li>
</ul>
Once I have explained how the algorithm will work, let's see how it works in practice. The algorithm was implemented in R.

I used the following libraries:

<pre lang="PHP">
library(quantmod)
library(timeSeries)
library(forecast);
library(TTR)
</pre>


The function that selects the best ARIMA model for a time series is the following:

<pre lang="PHP">
selectArima <- function(timeseries.ts) {
  aic<-matrix(NA,4,4);
  for(p in 0:3)
  {
    for(q in 0:3)
    {
      a.p.q<-arima(timeseries.ts,order=c(p,0,q));
      aic.p.q<-a.p.q$aic;
      aic[p+1,q+1]<-aic.p.q;
    }
  }
  inds = which(aic == min(aic), arr.ind=TRUE)-1;
  colnames(inds)<-c("p", "q");
  return(inds);
}
</pre>

Now, the following function runs the Monte Carlo method, and returns the probability for the stock going up next day.

<pre lang="PHP">
monteCarlo <- function(ts.ts, N) {
  pq<-selectArima(ts.ts);
  p=pq[1];
  q=pq[2];
  fit<-arima(ts.ts, c(p, 0, q));
  ts.sim<-ts.ts;
  n=length(ts.sim)
  
  # We simulate different trajectories
  
  sim <- list();
  S <- NULL;
  h <- 1;
  for(i in 1:N){
    sim[[i]] <- simulate(fit,h,future=TRUE,bootstrap = TRUE, innov=NULL);
    S <- cbind(S,as.vector(sim[[i]]));
  }
  S=S[ , colSums(is.na(S)) == 0];
  
  p <- sum(S>0)/N;

  return(p);
  
}
</pre>

The code is quite straightforward. The whole code is available on GitHub, or in the bottom of this post.


<h1>Backtest</h1>

So, is this strategy better than the buy-and-hold strategy? Since the code is <em>really</em> slow, I wasn't able to run it for a big period of time. I only backtested it with data from the end of 2014, to nowadays. The returns are shown in the following chart:

<div style="text-align: center;"><img src="https://raw.githubusercontent.com/bolorsociedad/MonteCarlo-and-Arima-for-stock-selection/master/arima.png"/></div>

The strategy behaved really well, and in this period the cumulative return was 1399.08%, which is really impressie. I did test it with some other small periods, and the returns were equally impressive. I was not able to test it for long periods, due to the speed of the code. However, if someone wants to test it, he is welcome to share the results obtained.

<h1>The whole code</h1>

In order to improve speed a little bit, instead of considering all the stocks that are obtained with the function <code>stockSymbols()</code>, it just selects a few of them randomly. Also, <code>stockSymbols()</code> returns a list of stocks that exist <i>now</i>, but many of them didn't exist a few years ago. So the code could be improved a lot.

This is the whole code, which is also available on GitHub:

<pre lang="PHP">
library(quantmod)
library(timeSeries)
library(forecast);
library(TTR)

selectArima <- function(timeseries.ts) {
  aic<-matrix(NA,4,4);
  for(p in 0:3)
  {
    for(q in 0:3)
    {
      a.p.q<-arima(timeseries.ts,order=c(p,0,q));
      aic.p.q<-a.p.q$aic;
      aic[p+1,q+1]<-aic.p.q;
    }
  }
  inds = which(aic == min(aic), arr.ind=TRUE)-1;
  colnames(inds)<-c("p", "q");
  return(inds);
}

monteCarlo <- function(ts.ts, N) {
  pq<-selectArima(ts.ts);
  p=pq[1];
  q=pq[2];
  fit<-arima(ts.ts, c(p, 0, q));
  ts.sim<-ts.ts;
  n=length(ts.sim)
  
  # We simulate different trajectories
  
  sim <- list();
  S <- NULL;
  h <- 1;
  for(i in 1:N){
    sim[[i]] <- simulate(fit,h,future=TRUE,bootstrap = TRUE, innov=NULL);
    S <- cbind(S,as.vector(sim[[i]]));
  }
  S=S[ , colSums(is.na(S)) == 0];
  


  
  p <- sum(S>0)/N;

  return(p);
  
}


####### SETTINGS #######

startDate="2010-01-01";
T=500;
N=1000; # Number of Monte Carlo simulations
numberOfStocks=20; # Number of stocks that will be considered.

########################

startDate=as.Date(startDate)-T;

x <- stockSymbols();


getSymbols("^GSPC", from=startDate);

GSPC=Cl(GSPC);
money<-c(1000);
for (i in (T+1):(length(GSPC)-2)) {
  symbols<-x$Symbol;
  symbols<-sample(symbols, numberOfStocks);
  
  maxProbability=0.5;
  possibleMoney=money[length(money)];
  for (j in 1:length(symbols)) {
    symbols[j] = gsub("-","", symbols[j])
    error<-0;
    error0<-tryCatch ({
      s <- getSymbols(symbols[j], from=startDate)
    }, error=function(e) {
      error0<-1;
    })
    if (error0==1) {
      next();
    }
    
    field <- c(paste(s,".Close",sep=""))
    data <- get(s)[, field]
    
    dateStart<-time(GSPC[i-T]);
    dateEnd<-time(GSPC[i]);
    
    data <- window(data, start=dateStart, end=dateEnd);
    
    
    
    if (length(data)<T) {
      cat("No data of '", as.character(s), "' for day ", as.character(dateEnd), "\n");
      next();
      
    }
    data.ts<-diff(log(window(data, start=dateStart, end=dateEnd)));
    
    error0<-tryCatch ({
      p<-monteCarlo(data.ts, N);
    }, error=function(e) {
      error0<-1;
    })
    if (error0==1) {
      next();
    }
    
    if (abs(p-0.5)>abs(maxProbability-0.5)) {
      dateStart<-time(GSPC[i-T]);
      dateEnd<-time(GSPC[i+1]);
      data <- get(s)[, field]
      
      data <- window(data, start=dateStart, end=dateEnd);
      
      newPrice=as.numeric(data[length(data)]);
      oldPrice=as.numeric(data[length(data)-1]);
      if (p<0.5) {
        possibleMoney=money[length(money)]*oldPrice/newPrice;
      }
      if (p>0.5) {
        possibleMoney=money[length(money)]*newPrice/oldPrice;
      }
      maxProbability=p;

    }
    
    
  }
  
  money<-c(money, possibleMoney);
  cat("Money:" , money[length(money)], "\n"); 
}


money.zoo<-zoo(money, time(GSPC)[(length(GSPC)-length(money)+1):length(GSPC)]);
sp500.zoo<-zoo(1000*GSPC[(length(GSPC)-length(money)+1):(length(GSPC))]/as.numeric(GSPC[length(GSPC)-length(money)+1]), time(GSPC)[(length(GSPC)-length(money)+1):length(GSPC)])
z<-as.zoo(cbind(money.zoo, sp500.zoo))
plot(x = z, ylab = "Cumulative Return", xlab="Year", main = "Cumulative Returns", screens=1, col=c("red", "blue"))
legend(x = "topleft", legend = c("Strategy", "Buy & hold"), 
       lty = 1,col = c("red", "blue"))
</pre>

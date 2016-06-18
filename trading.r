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
      cat("-----", p, "\n");
      
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


# mydata = c(22.27, 22.19, 22.08, 22.17, 22.18, 22.13, 22.23, 22.43,
#          22.24, 22.29, 22.15, 22.39, 22.38, 22.61, 23.36, 24.05, 28,
#          23.75, 23.83, 23.95, 23.63, 23.82, 23.87, 23.65, 23.19,
#          23.10, 23.33, 22.68, 23.10, 22.40, 22.17, 23,24)
# mydata = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,18,22,50,1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,18,22,50,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
# original = read.csv('~/Documents/R/tractor.csv')
# original = read.csv('/Users/fenilshingala/Documents/Python/untitled folder/disk.csv')
# original = read.csv('~/Documents/R/tractor.csv')
# mydata = original[1:200,2]

mydata = c(1,1,1, 2,4,5 ,12 ,2,1,1,1,1, 2,5,5, 17 ,1,2,1, 1,4,5 ,1,22,1)

#----------------------------------------------------------------#
anomMa <- function(data){
  coord = 1:length(data)
  window = 3
  renj=20
  mulT = 2/(window+1)
  first=0
  sma=c()
  ema=c()
  anomaly=list()
  l=list()
  expDis=c()
  k=1
  
  # anomN=0
  # anomLoop=0
  plot(coord, data, type="o")
  for (i in 0:(length(data)-window)){
    # anomLoop = anomLoop+1
    # if(anomLoop>=window){
    #   anomLoop=0
    #   cat("anomN:", anomN, "\n")
    # if(anomN>=8){return(i+window)}
    #   anomN=0
    # }
    
    sma = c(sma, mean(data[(i+1):(i+window)]))
    if (first==0){
      ema = c(ema, mean(data[(i+1):(i+window)]))
      first = first+1
    }
    else{ema = c(ema, (data[i+window]-ema[i])*mulT + ema[i])}
    
    if(length(expDis) == window){
      renj = mean(expDis)+(sd(expDis))
      # print(renj)
      expDis[k] = abs(data[i+window]-ema[i+1])
      if(k>=window){k=1}else{k=k+1}
      # lines(c((i+window):(i+2*window)), ema[i:(i+window)]+renj, type="l", col="green")
    }
    if(length(expDis) < window){
      expDis = c(expDis, abs(data[i+window]-ema[i+1]))
    }
    
    if(data[i+window] > (ema[i+1])+renj){
      # anomN = anomN+1
      anomaly = c(anomaly, data[i+window])
      l=c(l, (i+window))
    }
    if(data[i+window] < (ema[i+1])-renj){
      # anomN = anomN+1
      anomaly = c(anomaly, data[i+window])
      l=c(l, (i+window))
    }
    # print(i)
    # print(i+window)
    # print(expDis)
    # lines(c(window:length(data)), ema-renj, type="l", col="green")
  }
  anomaly = list(l, anomaly)
  
  lines(c(window:length(data)), sma, type="l", col="orange")
  lines(c(window:length(data)), ema, type="l", col="blue")
  # lines(c(window:length(data)), ema+renj, type="l", col="green")
  # lines(c(window:length(data)), ema-renj, type="l", col="green")
  lines(unlist(anomaly[1]), unlist(anomaly[2]), type="p", col="red")
  return(anomaly)
}
#----------------------------------------------------------------#

anomalies = anomMa(mydata)







visited=c()
freq=c()
elem = c()
freqAr = list()
for(i in unlist(anomalies[2])){
  index=match(i, visited)
  if(is.na(index)){
    visited = c(visited, i)
    elem <- c(elem, i)
    freq <- c(freq, length(which(unlist(anomalies[2]) == i)))
  }
}
abc = data.frame(elem, freq)
abc = abc[order(abc$freq, decreasing = TRUE),]
abc = unlist(abc[,1])

window = 3
renj=2
noOfAnom = 2
# plot(x, type="o",col = "blue")#, xlim = c(1,150), ylim = c(0,200))

d <- function(p, q){
  return(abs(p-q))
}

pattern <- function(x, anom){
  k=1
  patt <<- c()
  index = match(anom, x)
  if(is.na(index)){
    print("No match")
    return(NULL)
  }else{
    for(i in (index-window):(index-1)){
      if(i<=0){}else{
        patt[k] <<- x[i]
        k=k+1
      }
    }
  }
  print(paste("Pattern: ", list(patt)))
  return(patt)
}


matchPatt <- function(x, p){
  if(is.null(p)){
    print("got no match or 1st element! can't match empty")
    return(NULL)
  }else{
    pu = 1
    w=1
    q = c()
    while((pu+window)<=length(x)+1){
      # for(i in (pu):(pu+window-1)){
      for(i in (pu):(pu+length(p)-1)){
        q[w] <- x[i]
        w = w+1
      }
      if(DTWdist(q,p) < renj){
        print(q)
        lines(c(pu:(pu+length(p)-1)), q, type="o", col="green")
        print("alert------------------------")
      }
      
      pu=pu+1
      w=1
    }
  }
}


DTWdist <- function(x,y){
  # DTW = vector(mode="integer", length = length(x)+1)
  DTW <<- array(data=0, dim = c(length(x)+1,length(y)+1))
  for (a in 1:(length(x)+1)){
    DTW[a,1] <<- Inf
  }
  for (b in 1:(length(y)+1)){
    DTW[1,b] <<- Inf
  }
  DTW[1,1] <<- 0
  for (i in 1:length(x)+1){   # i will start from 2
    for (j in 1:length(y)+1){
      cost = d(x[i-1], y[j-1])
      # print(paste("X:", i, x[i-1], sep = " "))
      # print(paste("Y:", j, y[j-1], sep = " "))
      DTW[i,j] <<- cost + min(c(DTW[i-1,j], DTW[i,j-1], DTW[i-1,j-1]))
    }
  }
  # print(DTW)
  return(DTW[length(x)+1, length(y)+1])
}

# DTWdist(x,y)
# pattern(x)
# match(x, )
if(noOfAnom>0){
  if(noOfAnom > length(abc)){
    print("no of anomalies is out of bound")
  }else{
    for(i in (1:noOfAnom)){
      anom=abc[i]
      cat("for anom:", anom)
      matchPatt(mydata, pattern(mydata, anom))
    }
  }
}
# mydata = c(22.27, 22.19, 22.08, 22.17, 22.18, 22.13, 22.23, 22.43,
#          22.24, 22.29, 22.15, 22.39, 22.38, 22.61, 23.36, 24.05, 28,
#          23.75, 23.83, 23.95, 23.63, 23.82, 23.87, 23.65, 23.19,
#          23.10, 23.33, 22.68, 23.10, 22.40, 22.17, 23,24)
# mydata = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,18,22,50,1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,18,22,50,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
original = read.csv('~/Documents/R/cpu.csv')
original = read.csv('/Users/fenilshingala/Documents/Python/untitled folder/csv123.csv')
# original = read.csv('~/Documents/R/tractor.csv')
mydata = original[1:150,1]

#----------------------------------------------------------------#
anomMa <- function(data){
  coord = 1:length(data)
  window = 15
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
}
#----------------------------------------------------------------#

anomMa(mydata)
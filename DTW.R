# original = read.csv('~/Documents/R/tractor.csv')
# original = read.csv('/Users/fenilshingala/Documents/Python/untitled folder/disk1.csv')
# x = original[1:150,2]
x = c(1,1,1, 2,4,5 ,12 ,2,1,1,1,1, 2,5,5, 17 ,1,2,1, 1,4,5 ,1,22,1)
# y = c(1,2,1,1,1,1,1)
# x = c(1,3,2)
# y = c(1,0,2)
anom=12
window = 3
renj=2
plot(x, type="o",col = "blue")#, xlim = c(1,150), ylim = c(0,200))
# lines(y, type="o",col = "green")

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
        lines(c(pu:(pu+length(p)-1)), q, type="o", col="red")
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
matchPatt(x, pattern(x, anom))
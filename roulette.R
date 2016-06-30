#roulette

set.seed(1)
drawn = sample(0:36, 100, replace = T)
hist(drawn, main = "Frequency of numbers drawn",
     xlab = "Numbers drawn", breaks=37)
buildDf = function(howmany) {
  Matrix=matrix(rep(0, howmany * 14), nrow=howmany,ncol=14)
  DF=data.frame(Matrix)
  names(DF)=c("number","position","isRed","isBlack",
              "isOdd","isEven","is1to18","is19to36","is1to12",
              "is13to24","is25to36","isCol1","isCol2","isCol3")
  return(DF)
}

attributes = function(howmany,Seed=9999) {
  if (Seed != 9999) set.seed(Seed)
  DF = buildDf(howmany)
  drawn = sample(0:36, howmany, replace = T)
  DF$number=drawn
  numbers = c(0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27,
              13, 36, 11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14,
              31, 9, 22, 18, 29, 7, 28, 12, 35, 3, 26)
  for (i in 1:nrow(DF)){
    DF$position[i]= match(DF$number[i],numbers)
    if (DF$number[i] != 0) { if (DF$position[i]%%2) {
      DF$isBlack[i] = 1} else {DF$isRed[i] = 1}
      if (DF$number[i]%%2) { DF$isOdd[i]=1}
      else {DF$isEven[i]=1}
      if (DF$number[i] <= 18){ DF$is1to18[i]=1}
      else { DF$is19to36[i]=1}
      if(DF$number[i] <= 12){ DF$is1to12[i]=1}
      else if (DF$number[i]<25) { DF$is13to24[i] = 1}
      else { DF$is25to36[i] = 1}
      if(!(DF$number[i]%%3)){ DF$isCol3[i] = 1}
      else if ((DF$number[i] %% 3 ) == 2) {
        DF$isCol2[i] = 1}
      else { DF$isCol1[i] = 1}
    }
  }
  return(DF)
}

Data=attributes(1000,2)

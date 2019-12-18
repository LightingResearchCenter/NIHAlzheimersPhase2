runKrus_times<- function(nonParData){
  nonParData$variable <- as.numeric(as.character(nonParData$variable))
  
  TimeOfDay <- min(nonParData$variable):max(nonParData$variable)
  Krus <- c()
  p <- c()
  for(i in TimeOfDay){
    kruskalRaw1 <- kruskal.test(value~condition, data = nonParData[nonParData$variable == i,])
    Krus <- c(Krus, round(kruskalRaw1$statistic[[1]], digits = 2))
    p <- c(p, pValueReturn(kruskalRaw1$p.value))
  }
  TimeOfDay <- paste(TimeOfDay, ":00", sep = "")
  krsTests <- data.frame(TimeOfDay, Krus, p)
  colnames(krsTests)[1] <- "Time of day"
  colnames(krsTests)[2] <- "Kruskal-Wallis chi-squared"
  return(krsTests)
}

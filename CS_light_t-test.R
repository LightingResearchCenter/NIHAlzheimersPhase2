
performTtests <- function(Data, interaction){
  
  
  ComparedGroups <- c()
  df <- c()
  t <- c()
  p <- c()
  
  colnames(Data)[1] <- "Subject_id"
  colnames(Data)[2] <- "condition"
  colnames(Data)[length(colnames(Data))] <- "value"
  Data$condition <- as.factor(Data$condition)
  #get comparisons
  testModel<- lme(value ~ condition, random = ~1|Subject_id,
                  data=Data)
  
  lsmeanObj <- lsmeans(testModel, pairwise~ condition, adjust='None', data = Data)
  lsmeansComparisons3 <- data.frame(summary(lsmeanObj)[2])
  
  
  
  if(interaction == FALSE){
    comparistonList <- strsplit(as.character(lsmeansComparisons3$contrasts.contrast), " - ")
    corrected <- rep(FALSE, length(comparistonList))
    
  }
  
  if(interaction == "hourly"){
    comparistonList <- strsplit(as.character(lsmeansComparisons3$contrasts.contrast), " - ")
    corrected <- rep(TRUE, length(comparistonList))
    
  }
  if(interaction == "two-way"){
    compare1List <- str_split_fixed(as.character(lsmeansComparisons3$contrasts.contrast), " - ", 2)[,1]
    lsmeansComparisons3$compare1cond1 <- str_split_fixed(compare1List, "-", 2)[,1]
    lsmeansComparisons3$compare1cond2_time <- str_split_fixed(compare1List, "-", 2)[,2]
    lsmeansComparisons3$compare1cond2 <- str_split_fixed(lsmeansComparisons3$compare1cond2_time, "_", 2)[,1]
    lsmeansComparisons3$compare1_time <- str_split_fixed(lsmeansComparisons3$compare1cond2_time, "_", 2)[,2]
    lsmeansComparisons3$compare1cond2_time <- NULL
    
    compare2List <- str_split_fixed(as.character(lsmeansComparisons3$contrasts.contrast), " - ", 2)[,2]
    lsmeansComparisons3$compare2cond1 <- str_split_fixed(compare2List, "-", 2)[,1]
    lsmeansComparisons3$compare2cond2_time <- str_split_fixed(compare2List, "-", 2)[,2]
    lsmeansComparisons3$compare2cond2 <- str_split_fixed(lsmeansComparisons3$compare2cond2_time, "_", 2)[,1]
    lsmeansComparisons3$compare2_time <- str_split_fixed(lsmeansComparisons3$compare2cond2_time, "_", 2)[,2]
    lsmeansComparisons3$compare2cond2_time <- NULL
    
    
    lsmeansComparisons3$WithInTime <- ifelse((lsmeansComparisons3$compare2cond1 == lsmeansComparisons3$compare1cond1 
                                             & lsmeansComparisons3$compare1_time == lsmeansComparisons3$compare2_time) | 
                                               (lsmeansComparisons3$compare2cond2 == lsmeansComparisons3$compare1cond2 
                                              & lsmeansComparisons3$compare1_time == lsmeansComparisons3$compare2_time), TRUE, FALSE)
    
    
    lsmeansComparisons3$WithInCondition <- ifelse((lsmeansComparisons3$compare2cond1 == lsmeansComparisons3$compare1cond1 
                                                   & lsmeansComparisons3$compare1cond2 == lsmeansComparisons3$compare2cond2), TRUE, FALSE)
    
    
    lsmeansComparisons4 <- lsmeansComparisons3[lsmeansComparisons3$WithInTime == TRUE,]
    lsmeansComparisons5 <- lsmeansComparisons4[order(as.numeric(lsmeansComparisons4$compare1_time)),] 
    comparistonList <- strsplit(as.character(lsmeansComparisons5$contrasts.contrast), " - ")
    corrected <- rep(FALSE, length(comparistonList))
    

  }
  
  ###
  
  
  for(i in 1:length(comparistonList)){
    orignialData <- Data[Data$condition == comparistonList[[i]][1] | Data$condition == comparistonList[[i]][2] ,]
    
    
    curr_Ttest <- t.test(value ~ condition, data = orignialData,  var.equal = TRUE)
    
    
    ComparedGroups <- c(ComparedGroups, paste(comparistonList[[i]][1], comparistonList[[i]][2], sep = " - "  ))
    df <- c(df, as.numeric(curr_Ttest$parameter))
    t <- c(t, as.numeric(curr_Ttest$statistic))
    if(corrected[i]){
      #p <- c(p, p.adjust(curr_Ttest$p.value, "bonferroni", 55))
      p <- c(p, sidak_p(curr_Ttest$p.value, 55))
    }else{
      p <- c(p, curr_Ttest$p.value)
      
    }
  }
  t_test_table <- data.frame(ComparedGroups, df, t, p )
  t_test_table$p <- as.numeric(as.character(t_test_table$p))
  #t_test_table$p <- p.adjust( t_test_table$p, "bonferroni", length(t_test_table$p))
  t_test_table2 <- t_test_table
  ###
  t_test_table$t <- as.numeric(as.character(t_test_table$t))
  
  t_test_table$t <-  ifelse(!is.na(t_test_table$t), as.character(round(t_test_table$t, digits = 2)), t_test_table$t)
  #t_test_table$p <-  ifelse(!is.na(t_test_table$p), as.character(round(t_test_table$p, digits = 4)), t_test_table$p)
  t_test_table$p <- pValueReturn(t_test_table$p)

  
  return(t_test_table)
}

sidak_p <- function(p_value, numCompare){
  new_p <- 1-(1-p_value)^(numCompare)
  return(new_p)
}



performTtests <- function(Data){
  library(nlme)
  library(lsmeans)
  
  #performTtests(newData, doc, "Caffeine:Light")
  
  
  ComparedGroups <- c()
  df <- c()
  t <- c()
  p <- c()
  
  colnames(Data)[1] <- "Subject_id"
  colnames(Data)[2] <- "condition"
  colnames(Data)[length(colnames(Data))] <- "value"
  #get comparisons
  testModel<- lme(value ~ condition, random = ~1|Subject_id,
                  data=Data)
  
  df5 <- lsmeans(testModel, pairwise~ condition, adjust='None', data = Data)
  lsmeansComparisons3 <- data.frame(summary(df5)[2])
  comparistonList <- strsplit(as.character(lsmeansComparisons3$contrasts.contrast), " - ")
  ###
  
  for(i in 1:length(comparistonList)){
    orignialData <- Data[Data$condition == comparistonList[[i]][1] | Data$condition == comparistonList[[i]][2] ,]
    
    subList001 <- intersect(Data[Data$condition == comparistonList[[i]][1],]$Subject_id , Data[Data$condition == comparistonList[[i]][2],]$Subject_id)
    
    if(length(orignialData[orignialData$Subject_id %in% subList001, ]$Subject_id) == 0){
      cleanedData <- orignialData
      
      curr_Ttest <- t.test(value ~ condition, data = cleanedData, var.equal = TRUE)
    }else{
      cleanedData <- orignialData[orignialData$Subject_id %in% subList001, ]
      
      curr_Ttest <- t.test(value ~ condition, data = cleanedData, paired =  TRUE, var.equal = TRUE)
    }
    
    ComparedGroups <- c(ComparedGroups, paste(comparistonList[[i]][1], comparistonList[[i]][2], sep = " - "  ))
    df <- c(df, as.numeric(curr_Ttest$parameter))
    t <- c(t, as.numeric(curr_Ttest$statistic))
    p <- c(p, curr_Ttest$p.value)
    
  }
  t_test_table <- data.frame(ComparedGroups, df, t, p )
  t_test_table$p <- as.numeric(as.character(t_test_table$p))
  t_test_table2 <- t_test_table
  ###
  t_test_table$t <- as.numeric(as.character(t_test_table$t))
  
  t_test_table$t <-  ifelse(!is.na(t_test_table$t), as.character(round(t_test_table$t, digits = 3)), t_test_table$t)
  
  return(t_test_table)
}

addTtest2Doc <- function(t_test, doc){
  t <- t_test$statistic[[1]]
  df <- t_test$parameter[[1]]
  p <- t_test$p.value[[1]]
  
  tTestTable <- data.frame(t, df, p)
  tTestTable$t <- as.numeric(as.character(tTestTable$t))
  tTestTable$p <- ifelse(round(tTestTable$p, digits = 3) == 1, "1", ifelse(round(tTestTable$p, digits = 3) < .05, paste(substr(as.character(sprintf("%.3f", round(tTestTable$p, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(tTestTable$p, digits = 3))), 2, 5) ) )
  
  tTestTable$t <-  ifelse(!is.na(tTestTable$t), as.character(round(tTestTable$t, digits = 3)), tTestTable$t)
  
  
  
  
  textProp <- textProperties()
  
  sigFTable2 = vanilla.table( data = tTestTable )
  sigFTable2[as.numeric(substr(tTestTable$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
  doc = addFlexTable(doc, sigFTable2)
}
returnComplete <- function(data01){
  subFrame <- data.frame(table(data01$Subject))
  subList <- subFrame[subFrame$Freq > 1,]$Var1
  data02 <- data01[data01$Subject %in% subList,]
  return(data02)
}

checkDirection <- function(NIH_DIFF_data, outcomeMeasureList, directionType){
  doc = docx()

  for(i in 1:length(outcomeMeasureList)){
    print(outcomeMeasureList[i])
    outcomeMeasure <- outcomeMeasureList[i]
    
    doc <- addTitle( doc, outcomeMeasure, level = 1 )

    ctrl <- lmeControl(opt='optim')
    
    ALZ_DIFFdata_temp <- subset(NIH_DIFF_data, variable == outcomeMeasure & !is.na(value))
    
    #normalize data:
    
    ALZ_data_temp2 <- ALZ_DIFFdata_temp[ -c(4:7, 9, 12:18)]
    ALZ_data_temp3 <- spread(ALZ_data_temp2, key ="intervention", value = "value")
    ALZ_data_temp3$ALZ_data_difference <- ALZ_data_temp3$Intervention - ALZ_data_temp3$Baseline
    ALZ_data_temp3$ALZ_data_percentChange <- ((ALZ_data_temp3$Intervention/ALZ_data_temp3$Baseline)-1)*100
    
    meanBaselines <- aggregate(Baseline ~ Subject, data = ALZ_data_temp3, FUN = mean)
    
    ALZ_data_temp4 <- ALZ_data_temp3[ -c(2:4, 8:9)]
    
    colnames(ALZ_data_temp4)[3] <- "outcomeMeasure"
    
    
    ALZ_data_temp5 <- melt(ALZ_data_temp4, id = c("Subject", "light","directionDFA", "outcomeMeasure"))
    colnames(ALZ_data_temp5)[5] <- "normType"
    
    ALZ_data_temp6 <- spread(ALZ_data_temp5, key ="light", value = "value")
    
    ###add directions var
    if(directionType == "dfa"){
      
      outlier_low <- mean(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",]$value) - sd(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",]$value)*2
      outlier_high <- mean(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",]$value) + sd(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",]$value)*2
      ALZ_data_temp5$ouliter <- ifelse(ALZ_data_temp5$value < outlier_low | ALZ_data_temp5$value > outlier_high, TRUE, FALSE)
      gg_0 <-  ggplot(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",], aes(x = light, y = value, group = Subject, colour = directionDFA))+
        geom_point() +
        geom_line() +
        labs(y = outcomeMeasure)+
        geom_text(data = ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference" & ALZ_data_temp5$ouliter == TRUE,], aes(label = Subject, vjust=0), colour = "black") 
      
      
      doc <- addPlot(doc = doc, fun = print, x = gg_0, vector.graphic = FALSE, width = 4, height = 3)
      
      ALZ_data_temp6$direction <- ALZ_data_temp6$directionDFA
      
      ALZ_data_temp7 <- melt(ALZ_data_temp6, id = c("Subject", "direction", "directionDFA", "normType", "outcomeMeasure"))
      ALZ_data_temp7$value <- as.numeric(ALZ_data_temp7$value)
      ALZ_data_temp7 <- ALZ_data_temp7[!is.na(ALZ_data_temp7$value),]
      doc = addParagraph( doc,"Paired t-test between difference_active & difference_placebo: direction = greater", stylename = "Normal" )
      t_test <- t.test(value ~ variable, data =returnComplete(ALZ_data_temp7[ALZ_data_temp7$direction == "greater" & ALZ_data_temp7$normType == "ALZ_data_difference" & !is.na(ALZ_data_temp7$value),]), paired = TRUE)
      addTtest2Doc(t_test, doc)
      
      doc = addParagraph( doc,"Paired t-test between difference_active & difference_placebo: direction = less", stylename = "Normal" )
      tTestData_1 <- ALZ_data_temp7[ALZ_data_temp7$direction == "less" & ALZ_data_temp7$normType == "ALZ_data_difference" & !is.na(ALZ_data_temp7$value),]
      
      t_test <- t.test(value ~ variable, data = returnComplete(tTestData_1), paired = TRUE)
      addTtest2Doc(t_test, doc)
      
      doc = addParagraph( doc,"unpaired t-test between greater & less: condition = difference_active", stylename = "Normal" )
      t_test <- t.test(value ~ direction, data = ALZ_data_temp7[ALZ_data_temp7$variable == "Active" & ALZ_data_temp7$normType == "ALZ_data_difference"& !is.na(ALZ_data_temp7$value),], var.equal = TRUE)
      addTtest2Doc(t_test, doc)
      
      doc = addParagraph( doc,"unpaired t-test between greater & less: condition = difference_placebo", stylename = "Normal" )
      t_test <- t.test(value ~ direction, data = ALZ_data_temp7[ALZ_data_temp7$variable == "Placebo" & ALZ_data_temp7$normType == "ALZ_data_difference"& !is.na(ALZ_data_temp7$value),], var.equal = TRUE)
      addTtest2Doc(t_test, doc)
      
      
    }else{
      ALZ_data_temp6$direction <- ifelse(ALZ_data_temp6$Active > ALZ_data_temp6$Placebo,
                                         "greater", "less")
      
      outlier_low <- mean(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",]$value) - sd(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",]$value)*2
      outlier_high <- mean(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",]$value) + sd(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",]$value)*2
      ALZ_data_temp5$ouliter <- ifelse(ALZ_data_temp5$value < outlier_low | ALZ_data_temp5$value > outlier_high, TRUE, FALSE)
       gg_0 <-  ggplot(ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",], aes(x = light, y = value, group = Subject, colour = directionDFA))+
        geom_point() +
        geom_line() +
        labs(y = outcomeMeasure)+
         geom_text(data = ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference" & ALZ_data_temp5$ouliter == TRUE,], aes(label = Subject, vjust=0), colour = "black") 
         
      
      doc <- addPlot(doc = doc, fun = print, x = gg_0, vector.graphic = FALSE, width = 4, height = 3)
      
      ALZ_data_temp7 <- melt(ALZ_data_temp6, id = c("Subject", "direction", "directionDFA", "normType", "outcomeMeasure"))
      
      outlier_low <- mean(ALZ_data_temp7[!is.na(ALZ_data_temp7$directionDFA) & ALZ_data_temp7$normType == "ALZ_data_difference",]$value) - sd(ALZ_data_temp7[!is.na(ALZ_data_temp7$directionDFA) & ALZ_data_temp7$normType == "ALZ_data_difference",]$value)*2
      outlier_high <- mean(ALZ_data_temp7[!is.na(ALZ_data_temp7$directionDFA) & ALZ_data_temp7$normType == "ALZ_data_difference",]$value) + sd(ALZ_data_temp7[!is.na(ALZ_data_temp7$directionDFA) & ALZ_data_temp7$normType == "ALZ_data_difference",]$value)*2
      ALZ_data_temp7$ouliter <- ifelse(ALZ_data_temp7$value < outlier_low | ALZ_data_temp7$value > outlier_high, TRUE, FALSE)
      gg_01 <-  ggplot(ALZ_data_temp7[!is.na(ALZ_data_temp7$directionDFA) & ALZ_data_temp7$normType == "ALZ_data_difference",], aes(x = variable, y = value, group = Subject, colour = direction))+
        geom_point() +
        geom_line() +
        labs(y = outcomeMeasure, x = "")+
        geom_text(data = ALZ_data_temp7[!is.na(ALZ_data_temp7$directionDFA) & ALZ_data_temp7$normType == "ALZ_data_difference" & ALZ_data_temp7$ouliter == TRUE,], aes(label = Subject, vjust=0), colour = "black") 
      
      
    
      doc <- addPlot(doc = doc, fun = print, x = gg_01, vector.graphic = FALSE, width = 4, height = 3)
      
      ALZ_data_temp5$sub_dir <- paste(ALZ_data_temp5$Subject, ALZ_data_temp5$directionDFA, sep = "_")
      group1 <- ALZ_data_temp5[!is.na(ALZ_data_temp5$directionDFA) & ALZ_data_temp5$normType == "ALZ_data_difference",]
      
      
      ALZ_data_temp7$sub_dir <- paste(ALZ_data_temp7$Subject, ALZ_data_temp7$direction, sep = "_")
      group2 <- ALZ_data_temp7[!is.na(ALZ_data_temp7$directionDFA) & ALZ_data_temp7$normType == "ALZ_data_difference",]
      
      group1List <- unique(group1$sub_dir)
      group21List <- unique(group2$sub_dir)
      
      acuracy <- length(intersect(group1List, group21List))/mean(length(group1List), length(group21List))
      doc = addParagraph( doc,as.character(paste((round(acuracy, digits = 2) * 100), "%", sep = "")), stylename = "Normal" )
      
      doc = addParagraph( doc,"Paired t-test between difference_active & difference_placebo: direction = greater", stylename = "Normal" )
      t_test <- t.test(value ~ variable, data = ALZ_data_temp7[ALZ_data_temp7$direction == "greater" & ALZ_data_temp7$normType == "ALZ_data_difference" & !is.na(ALZ_data_temp7$value),], paired = TRUE)
      addTtest2Doc(t_test, doc)
      
      doc = addParagraph( doc,"Paired t-test between difference_active & difference_placebo: direction = less", stylename = "Normal" )
      t_test <- t.test(value ~ variable, data = ALZ_data_temp7[ALZ_data_temp7$direction == "less" & ALZ_data_temp7$normType == "ALZ_data_difference" & !is.na(ALZ_data_temp7$value),], paired = TRUE)
      addTtest2Doc(t_test, doc)
      
      doc = addParagraph( doc,"unpaired t-test between greater & less: condition = difference_active", stylename = "Normal" )
      t_test <- t.test(value ~ direction, data = ALZ_data_temp7[ALZ_data_temp7$variable == "Active" & ALZ_data_temp7$normType == "ALZ_data_difference"& !is.na(ALZ_data_temp7$value),], var.equal = TRUE)
      addTtest2Doc(t_test, doc)
      
      doc = addParagraph( doc,"unpaired t-test between greater & less: condition = difference_placebo", stylename = "Normal" )
      t_test <- t.test(value ~ direction, data = ALZ_data_temp7[ALZ_data_temp7$variable == "Placebo" & ALZ_data_temp7$normType == "ALZ_data_difference"& !is.na(ALZ_data_temp7$value),], var.equal = TRUE)
      addTtest2Doc(t_test, doc)
      
       }
    
    
 
    
    ###apply mean of each baseline for each subject
    ALZ_data_temp6$baselineMean <- meanBaselines[match(ALZ_data_temp6$Subject, meanBaselines$Subject),]$Baseline
    
    
    
    ###two summary tables and graphs:: 1. Difference & Percentage
    
    
    dfaSummary_direction1 <- summarySE(ALZ_data_temp6[!is.na(ALZ_data_temp6$direction) & ALZ_data_temp6$normType == "ALZ_data_difference",], measurevar = "baselineMean", groupvars = c("direction")) 
    
    gg_1 <- ggplot(dfaSummary_direction1, aes(x = direction, y = baselineMean, fill = direction))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_errorbar(aes(ymin=baselineMean-se, ymax=baselineMean+se),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      geom_text(aes(label = round(baselineMean, digits = 3), vjust=2)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
      theme(legend.position="none")
    
    doc <- addPlot(doc = doc, fun = print, x = gg_1, vector.graphic = FALSE, width = 4, height = 3)

    if(directionType != "dfa"){
      numSide <- data.frame(table(ALZ_data_temp6[!is.na(ALZ_data_temp6$direction) & ALZ_data_temp6$normType == "ALZ_data_difference",]$direction))
      numeSide2 <- data.frame(t(numSide))
      colnames(numeSide2) = as.character(unlist( numeSide2[1, ])) # the first row will be the header
      numeSide2 = numeSide2[-1, ] 
      
      doc = addFlexTable(doc, vanilla.table(numeSide2))
      doc = addParagraph( doc," ", stylename = "Normal" )
    }

    doc = addParagraph( doc,"..", stylename = "Normal" )
    
    t_test <- t.test(baselineMean ~ direction, data = ALZ_data_temp6[!is.na(ALZ_data_temp6$direction) & ALZ_data_temp6$normType == "ALZ_data_difference",], var.equal = TRUE)
    addTtest2Doc(t_test, doc)
    

    
  }
  filename <- paste("NIH-baselineMeas_directionTests",  format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".docx", sep = "_")
  dir <- "C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/"
  writeDoc( doc, file = paste(dir, filename, sep = "") )

}
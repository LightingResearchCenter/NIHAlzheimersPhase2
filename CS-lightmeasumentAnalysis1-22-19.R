library(readxl)
library(ggplot2)
library(lme4)
library(lmerTest)
library(officer)
library(reshape2)
library(nlme)
library(Rmisc)
library(dplyr)
library(lsmeans)
library(stringr)
library(ggpubr)


pValueReturn <- function(p_value){
  out <- NA
  out <- ifelse(p_value < .00001, "< .00001 *", 
                ifelse(p_value < .0001, "< .0001 *", 
                       ifelse(p_value < .001, "< .001 *",
                              ifelse(p_value < .01,  "< .01 *", 
                                     ifelse(p_value < .05, paste("", round(p_value, digits = 4),"*"), 
                                            paste("", round(p_value, digits = 4)))))))
  
  
  return(out)
  
  
}

modtifyAnovaTable<- function(mixedModel){
  mixedModel_outputDF <- data.frame(anova(mixedModel))
  colnames(mixedModel_outputDF)[1] <- "df"
  colnames(mixedModel_outputDF)[2] <- "Error"
  colnames(mixedModel_outputDF)[3] <- "F"
  colnames(mixedModel_outputDF)[4] <- "p"
  #mixedModel_outputDF$p <-  ifelse(!is.na(mixedModel_outputDF$p), substr(as.character(sprintf("%.3f", round(mixedModel_outputDF$p, digits = 3))), 2, 5), mixedModel_outputDF$p)
  mixedModel_outputDF$p <- pValueReturn(mixedModel_outputDF$p)
  mixedModel_outputDF$F <-  ifelse(!is.na(mixedModel_outputDF$F), as.character(round(mixedModel_outputDF$F, digits = 3)), mixedModel_outputDF$F)
  
  mixedModel_outputDF$Error <- ifelse(is.na(mixedModel_outputDF$Error), "", mixedModel_outputDF$Error)
  mixedModel_outputDF$df <- ifelse(is.na(mixedModel_outputDF$df), "", mixedModel_outputDF$df)
  mixedModel_outputDF <- mixedModel_outputDF[2:length(mixedModel_outputDF$p),]
  mixedModel_outputDF <- tibble::rownames_to_column(mixedModel_outputDF, "Dependent measures")
  
  return(mixedModel_outputDF)
}

source('~/GitHub/NIHAlzheimersPhase2/CS_light_t-test.R', echo=TRUE)
##Mean ################################################################################################




    #CS##############################################################
        average_cs_2019_01_09_1555 <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/tables/average_cs_2019-01-09_1555.xlsx")
        average_cs_2019_01_09_1555$protocol <- ifelse(average_cs_2019_01_09_1555$protocol == "placbeo", "placebo", average_cs_2019_01_09_1555$protocol)
        
        NIH_short <- subset(average_cs_2019_01_09_1555, group == "short_term")
    
        NIH_short2 <- melt(NIH_short, id = c("id", "group", "session", "protocol"))
        NIH_short2$value <- as.numeric(NIH_short2$value )
        NIH_short2$variable <- as.numeric(NIH_short2$variable )
        
        meanCS <- NIH_short2[!is.na(NIH_short2$value),]
        
        rm(average_cs_2019_01_09_1555, NIH_short, NIH_short2)
        
        meanCS$condition <- as.factor(paste(meanCS$protocol, meanCS$session, sep = "-"))
        
        #meanCS$variable <- as.numeric(meanCS$variable)
        
        meanCS <- meanCS[meanCS$variable >= 8 & meanCS$variable <= 18,]
        
        meanCS$variable <- factor(meanCS$variable , order = TRUE, levels =8:18)
        
        
        workingDir <- "//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/"
        
        corr_doc <- read_docx() 
        docFilename <- paste0(workingDir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"),"-NIH-Light measures- analysis.docx")
        
        
        meanCS$id <- as.factor(meanCS$id)
        meanCS$condition <- as.factor(meanCS$condition)

        
        meanCSModel <- lme(value ~  condition*variable ,  random=~1|id/condition/variable,  data = meanCS[is.finite(meanCS$value),])

        meanCS_mainModel <- modtifyAnovaTable(meanCSModel)
        emmeans::emmeans(meanCSModel, pairwise~ condition, adjust="bonferroni")
        emmeans::emmeans(meanCSModel, pairwise~ variable, adjust="bonferroni")
        emmeans::emmeans(meanCSModel, pairwise~ condition|variable, adjust="bonferroni")
        
        
        meanCS$variable <- as.factor(meanCS$variable)
        
        
            ###ADDED GRAPH            
        
            summaryCS00 <- summarySE(measurevar = "value", groupvars = c("condition", "id", "protocol"), data = meanCS)
            summaryCS00 <- aggregate(value ~ id * condition * protocol, data = meanCS, FUN = mean)
            conditionTtests <- performTtests(summaryCS00, FALSE)
            
            summaryCS002 <- aggregate(value ~ id * condition * protocol * variable, data = meanCS, FUN = mean)
            conditionTtests2 <- performTtests(summaryCS002, FALSE)
            
            summaryCS01 <- aggregate(value ~ id * variable, data = meanCS, FUN = mean)
            hourlyTtests2 <- performTtests(summaryCS01, "hourly")
            hourlyTtests <- performTtests(summaryCS01, FALSE)
            
            summaryCS0101 <- aggregate(value ~ id * variable*condition, data = meanCS, FUN = mean)
            hourlyTtests3 <- performTtests(summaryCS0101, "hourly")
            hourlyTtests4 <- performTtests(summaryCS0101, FALSE)
            
            hourlyTtests$p_corrected<-  hourlyTtests2$p
            
            hourlyTtests$Empty <- ""
            
            hourlyTtests$df2 <- hourlyTtests3$df
            hourlyTtests$t2 <- hourlyTtests3$t
            hourlyTtests$p2 <- hourlyTtests4$p
            hourlyTtests$p2_corrected <- hourlyTtests3$p
            
             t.test(value ~ condition, data = summaryCS00[summaryCS00$protocol == "active",])
            t.test(value ~ condition, data = summaryCS00[summaryCS00$protocol == "placebo",])
            
            summaryCS0 <- summarySE(measurevar = "value", groupvars = c("condition"), data = summaryCS00)
          
            meanCS_plot <- ggplot(summaryCS0, aes(x = condition, y = value, fill =condition))+
                 geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
                geom_errorbar(aes(ymin=value-se, ymax=value+se),
                            width=.2,                    
                            position=position_dodge(.3))+
                theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="none") +
                labs(x= "", y = "Mean CS")+
                scale_fill_manual(values=c("grey42","deepskyblue2", "grey42","orangered1")) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
            

        
            summaryCS0_hour <- summarySE(measurevar = "value", groupvars = c("variable"), data = summaryCS01)
            summaryCS0_hour$dummy <- "test"
            meanHour_plot <- ggplot(summaryCS0_hour, aes(x = variable, y = value, group = dummy))+
              geom_point()+
              geom_line()+
              geom_errorbar(aes(ymin=value-se, ymax=value+se),
                            width=.2,                    
                            position=position_dodge(.3))+
              theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
              labs(x= "Time of day", y = "CS")+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
            
            

        ###P
        
        
        ##
        summaryCS <- summarySE(measurevar = "value", groupvars = c("variable", "condition"), data = meanCS)
        
        #ggplot(summaryCS, aes(x = variable, y = value))+
        #  geom_point()
        
        #active
        active <- ggplot(summaryCS[summaryCS$condition == "active-baseline" | summaryCS$condition == "active-intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
          #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "CS")+
          scale_colour_manual(values=c("grey42","deepskyblue2")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        

                ###Perform t-tests
                 tTestData1 <- meanCS[meanCS$condition == "active-baseline" | meanCS$condition == "active-intervention",]

                 for(i in 8:18){
                   print("----------------------------------------")
                   print(i)
                   print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))

                   

                 }
                 meanCS$condition_time <- paste(meanCS$condition, meanCS$variable, sep = "_")
                 summaryCS03 <- aggregate(value ~ id * condition_time * condition * variable * protocol, data = meanCS, FUN = mean)
                 intereactionTtests <- performTtests(summaryCS03, "two-way")
        
        #placebo
        placebo <- ggplot(summaryCS[summaryCS$condition == "placebo-baseline" | summaryCS$condition == "placebo-intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "CS")+
          scale_colour_manual(values=c("grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        ###Perform t-tests
        tTestData1 <- meanCS[meanCS$condition == "placebo-baseline" | meanCS$condition == "placebo-intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        

        
        #baseline
        baselines <- ggplot(summaryCS[summaryCS$condition == "placebo-baseline" | summaryCS$condition == "active-baseline",], aes(x = variable, y = value, colour =condition, group = condition ))+
          #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "CS")+
          scale_colour_manual(values=c("grey42","grey22")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        hourlyPlots <- ggarrange(active, placebo, baselines, labels = c("A.", "B.", "C."), ncol = 1, nrow = 3)
        
        
        ###Perform t-tests
        tTestData1 <- meanCS[meanCS$condition == "placebo-baseline" | meanCS$condition == "active-baseline",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        

        
        
        
        corr_doc <- corr_doc %>% 
          body_add_par("Average hourly CS", style = "heading 1") %>% 
          body_add_par("", style = "Normal") %>% # blank paragraph
          
        
          body_add_par("Table 1.", style = "table title")  %>% 
          body_add_table(value = meanCS_mainModel, style = "table_template" ) %>% 
          
          body_add_gg(value = meanCS_plot, width = 5, height = 3, style = "Normal" ) %>% 
          body_add_par("Figure 1.", style = "graphic title")  %>% 
          
          body_add_par("Table 2.T-test results between conditions. Averaged accorss time of day.", style = "table title")  %>% 
          body_add_table(value = conditionTtests, style = "table_template" ) %>% 
          
          body_add_par("Table 2.T-test results between conditions.", style = "table title")  %>% 
          body_add_table(value = conditionTtests2, style = "table_template" ) %>% 
          
          
          body_add_gg(value = hourlyPlots, width = 6, height = 7.5, style = "Normal" ) %>% 
          body_add_par("Figure 2.", style = "graphic title")  %>% 
          
         
          body_add_par("Table 3.T-test results between conditions for each hour.", style = "table title")  %>% 
          body_add_table(value = intereactionTtests, style = "table_template" ) %>%
       
          body_add_gg(value = meanHour_plot, width = 6, height = 3, style = "Normal" ) %>% 
          body_add_par("Figure 3.", style = "graphic title")  %>% 
         
          body_add_par("Table 2. T-test results between hours.", style = "table title")  %>% 
           body_add_table(value = hourlyTtests, style = "table_template" ) %>% 
           
       # body_add_par("Table 4.", style = "table title")  %>% 
          #body_add_table(value = Red_table, style = "table_template" ) 
        
        
        print(corr_doc, target = docFilename)
    
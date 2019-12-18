library(readxl)
library(reshape2)
library(nlme)
library(ggplot2)




pValueReturn <- function(p_value){
  out <- NA
  out <- ifelse(p_value < .00001, "p < .00001 *", 
                ifelse(p_value < .0001, "p < .0001 *", 
                       ifelse(p_value < .001, "p < .001 *",
                              ifelse(p_value < .01,  "p < .01 *", 
                                     ifelse(p_value < .05, paste("p =", round(p_value, digits = 4),"*"), 
                                            paste("p =", round(p_value, digits = 4)))))))
  
  
  return(out)
  
  
}



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
        
        meanCS$condition <- as.factor(paste(meanCS$protocol, meanCS$session, sep = "_"))
        
        #meanCS$variable <- as.numeric(meanCS$variable)
        
        meanCS <- meanCS[meanCS$variable >= 8 & meanCS$variable <= 18,]
        
        meanCS$variable <- factor(meanCS$variable , order = TRUE, levels =8:18)
        
        
        meanCSModel <- lme(value ~  condition*variable ,  random=~1|id/condition/variable,  data = meanCS[is.finite(meanCS$value),])
        anova(meanCSModel)
        write.csv(data.frame(anova(meanCSModel))[-1,],"//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/meanCSModel.csv")
        #summary(meanCSModel)
        
        
        meanCS$variable <- as.factor(meanCS$variable)
        
        
            ###ADDED GRAPH            
        
            summaryCS00 <- summarySE(measurevar = "value", groupvars = c("condition", "id", "protocol"), data = meanCS)
            t.test(value ~ condition, data = summaryCS00[summaryCS00$protocol == "active",])
            t.test(value ~ condition, data = summaryCS00[summaryCS00$protocol == "placebo",])
            
            summaryCS0 <- summarySE(measurevar = "value", groupvars = c("condition"), data = summaryCS00)
          
              ggplot(summaryCS0, aes(x = condition, y = value, fill =condition))+
                 geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
                geom_errorbar(aes(ymin=value-se, ymax=value+se),
                            width=.2,                    
                            position=position_dodge(.3))+
                theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="none") +
                labs(x= "", y = "Mean CS")+
                scale_fill_manual(values=c("grey42","deepskyblue2", "grey42","orangered1")) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
            
            ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/meanCS_active.png", width = 5, height = 3, units = "in", dpi = 400)
            
        
        
        
        #ggplot(summaryCS, aes(x = variable, y = value))+
        #  geom_point()
        
        #active
        ggplot(summaryCS[summaryCS$condition == "active_baseline" | summaryCS$condition == "active_intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
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
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanCS_active.png", width = 8, height = 6, units = "in", dpi = 400)
        
        ###P
        
        
        ##
        summaryCS <- summarySE(measurevar = "value", groupvars = c("variable", "condition"), data = meanCS)
        
        #ggplot(summaryCS, aes(x = variable, y = value))+
        #  geom_point()
        
        #active
        ggplot(summaryCS[summaryCS$condition == "active_baseline" | summaryCS$condition == "active_intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
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
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanCS_active.png", width = 8, height = 6, units = "in", dpi = 400)
        
                ###Perform t-tests
                 tTestData1 <- meanCS[meanCS$condition == "active_baseline" | meanCS$condition == "active_intervention",]
                 
                 for(i in 8:18){
                   print("----------------------------------------")
                   print(i)
                   print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
                   
                 }
        
        
        
        #placebo
        ggplot(summaryCS[summaryCS$condition == "placebo_baseline" | summaryCS$condition == "placebo_intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
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
        tTestData1 <- meanCS[meanCS$condition == "placebo_baseline" | meanCS$condition == "placebo_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanCS_placebo.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
        #baseline
        ggplot(summaryCS[summaryCS$condition == "placebo_baseline" | summaryCS$condition == "active_baseline",], aes(x = variable, y = value, colour =condition, group = condition ))+
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
        
        
        
        ###Perform t-tests
        tTestData1 <- meanCS[meanCS$condition == "placebo_baseline" | meanCS$condition == "active_baseline",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanCS_baseline.png", width = 8, height = 6, units = "in", dpi = 400)
        
    
    
    #meanLog10CLA##############################################################
        
        mean_log10CircadianLight_2019_01_09_1557 <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/tables/mean_log10CircadianLight_2019-01-09_1557.xlsx")
        mean_log10CircadianLight_2019_01_09_1557$protocol <- ifelse(mean_log10CircadianLight_2019_01_09_1557$protocol == "placbeo", "placebo", mean_log10CircadianLight_2019_01_09_1557$protocol)
        
        NIH_short <- subset(mean_log10CircadianLight_2019_01_09_1557, group == "short_term")
        
        
        
        
        NIH_short2 <- melt(NIH_short, id = c("id", "group", "session", "protocol"))
        NIH_short2$value <- as.numeric(NIH_short2$value )
        NIH_short2$variable <- as.numeric(NIH_short2$variable )
        
        meanLogCLA <- NIH_short2[!is.na(NIH_short2$value),]
        
        rm(mean_log10CircadianLight_2019_01_09_1557, NIH_short, NIH_short2)
        
        meanLogCLA$condition <- as.factor(paste(meanLogCLA$protocol, meanLogCLA$session, sep = "_"))
        
        #meanLogCLA$variable <- as.numeric(meanLogCLA$variable)
        
        meanLogCLA <- meanLogCLA[meanLogCLA$variable >= 8 & meanLogCLA$variable <= 18,]
        
        meanLogCLA$variable <- factor(meanLogCLA$variable , order = TRUE, levels =8:18)
        
        meanLogCLAModel <- lme(value ~  condition*variable ,  random=~1|id/condition/variable,  data = meanLogCLA[is.finite(meanLogCLA$value),])
        anova(meanLogCLAModel)
        write.csv(data.frame(anova(meanLogCLAModel))[-1,],"//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/meanLogCLAModel.csv")
        
        ###add graphh
        summaryCS00 <- summarySE(measurevar = "value", groupvars = c("condition", "id"), data = meanLogCLA)
        
        summaryCS0 <- summarySE(measurevar = "value", groupvars = c("condition"), data = summaryCS00)
        
        ggplot(summaryCS0, aes(x = condition, y = value, fill =condition))+
          geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="none") +
          labs(x= "", y = "Mean log10(CLA)")+
          scale_fill_manual(values=c("grey42","deepskyblue2", "grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/meanLOGCLA_active.png", width = 5, height = 3, units = "in", dpi = 400)
        
        ##
        summaryCS <- summarySE(measurevar = "value", groupvars = c("variable", "condition"), data = meanLogCLA)
        
        #ggplot(summaryCS, aes(x = variable, y = value))+
        #  geom_point()
        
        #active
        ggplot(summaryCS[summaryCS$condition == "active_baseline" | summaryCS$condition == "active_intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
          #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Mean log10(CLA)")+
          scale_colour_manual(values=c("grey42","deepskyblue2")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanLogCLA_active.png", width = 8, height = 6, units = "in", dpi = 400)
        
        ###Perform t-tests
        tTestData1 <- meanLogCLA[meanLogCLA$condition == "active_baseline" | meanLogCLA$condition == "active_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        #placebo
        ggplot(summaryCS[summaryCS$condition == "placebo_baseline" | summaryCS$condition == "placebo_intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y ="Mean log10(CLA)")+
          scale_colour_manual(values=c("grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        ###Perform t-tests
        tTestData1 <- meanLogCLA[meanLogCLA$condition == "placebo_baseline" | meanLogCLA$condition == "placebo_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanLogCLA_placebo.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
        #baseline
        ggplot(summaryCS[summaryCS$condition == "placebo_baseline" | summaryCS$condition == "active_baseline",], aes(x = variable, y = value, colour =condition, group = condition ))+
          #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Mean log10(CLA)")+
          scale_colour_manual(values=c("grey42","grey22")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        
        ###Perform t-tests
        tTestData1 <- meanLogCLA[meanLogCLA$condition == "placebo_baseline" | meanLogCLA$condition == "active_baseline",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanLogCLA_baseline.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
    #meanLog10ILL##############################################################
        
        mean_log10Illuminance_2019_01_09_1558 <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/tables/mean_log10Illuminance_2019-01-09_1558.xlsx")
        mean_log10Illuminance_2019_01_09_1558$protocol <- ifelse(mean_log10Illuminance_2019_01_09_1558$protocol == "placbeo", "placebo", mean_log10Illuminance_2019_01_09_1558$protocol)
        
        NIH_short <- subset(mean_log10Illuminance_2019_01_09_1558, group == "short_term")
        
        
        
        
        NIH_short2 <- melt(NIH_short, id = c("id", "group", "session", "protocol"))
        NIH_short2$value <- as.numeric(NIH_short2$value )
        NIH_short2$variable <- as.numeric(NIH_short2$variable )
        
        meanLogILL <- NIH_short2[!is.na(NIH_short2$value),]
        
        rm(mean_log10Illuminance_2019_01_09_1558, NIH_short, NIH_short2)
        
        meanLogILL$condition <- as.factor(paste(meanLogILL$protocol, meanLogILL$session, sep = "_"))
        
        #meanLogILL$variable <- as.numeric(meanLogILL$variable)
        
        meanLogILL <- meanLogILL[meanLogILL$variable >= 8 & meanLogILL$variable <= 18,]
        meanLogILL$variable <- factor(meanLogILL$variable , order = TRUE, levels =8:18)
        
        meanLogILLModel <- lme(value ~  condition*variable ,  random=~1|id/condition/variable,  data = meanLogILL[is.finite(meanLogILL$value),])
        anova(meanLogILLModel)
        write.csv(data.frame(anova(meanLogILLModel))[-1,],"//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/meanLogILLModel.csv")
    
        #summary(CS_directionModel)
        
        
        ###add graphh
        summaryCS00 <- summarySE(measurevar = "value", groupvars = c("condition", "id"), data = meanLogILL)
        
        summaryCS0 <- summarySE(measurevar = "value", groupvars = c("condition"), data = summaryCS00)
        
        ggplot(summaryCS0, aes(x = condition, y = value, fill =condition))+
          geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="none") +
          labs(x= "", y = "Mean log10(Illuminance)")+
          scale_fill_manual(values=c("grey42","deepskyblue2", "grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/meanLOGLUX_active.png", width = 5, height = 3, units = "in", dpi = 400)
        
        ##
        summaryCS <- summarySE(measurevar = "value", groupvars = c("variable", "condition"), data = meanLogILL)
        
        #ggplot(summaryCS, aes(x = variable, y = value))+
        #  geom_point()
        
        #active
        ggplot(summaryCS[summaryCS$condition == "active_baseline" | summaryCS$condition == "active_intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
          #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Mean log10(Illuminance)")+
          scale_colour_manual(values=c("grey42","deepskyblue2")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanLogILL_active.png", width = 8, height = 6, units = "in", dpi = 400)
        
        ###Perform t-tests
        tTestData1 <- meanLogILL[meanLogILL$condition == "active_baseline" | meanLogILL$condition == "active_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        #placebo
        ggplot(summaryCS[summaryCS$condition == "placebo_baseline" | summaryCS$condition == "placebo_intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Mean log10(Illuminance)")+
          scale_colour_manual(values=c("grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        ###Perform t-tests
        tTestData1 <- meanLogILL[meanLogILL$condition == "placebo_baseline" | meanLogILL$condition == "placebo_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanLogILL_placebo.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
        #baseline
        ggplot(summaryCS[summaryCS$condition == "placebo_baseline" | summaryCS$condition == "active_baseline",], aes(x = variable, y = value, colour =condition, group = condition ))+
          #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin=value-se, ymax=value+se),
                        width=.2,                    
                        position=position_dodge(.3))+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Mean log10(Illuminance)")+
          scale_colour_manual(values=c("grey42","grey22")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        
        ###Perform t-tests
        tTestData1 <- meanLogILL[meanLogILL$condition == "placebo_baseline" | meanLogILL$condition == "active_baseline",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(t.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_meanLogILL_baseline.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
    
    
##Median ################################################################################################
    
    # Function: "Kruskal-Wallis chi-squared"
    
    source('~/GitHub/NIHAlzheimersPhase2/Krsucal-hour-tests.R', echo=TRUE)
    
    
    
    #CS##############################################################
        median_cs_2019_01_09_1558 <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/tables/median_cs_2019-01-09_1558.xlsx")
        median_cs_2019_01_09_1558$protocol <- ifelse(median_cs_2019_01_09_1558$protocol == "placbeo", "placebo", median_cs_2019_01_09_1558$protocol)
        
        NIH_short <- subset(median_cs_2019_01_09_1558, group == "short_term")
        
        
        NIH_short2 <- melt(NIH_short, id = c("id", "group", "session", "protocol"))
        NIH_short2$value <- as.numeric(NIH_short2$value )
        
        medianCS <- NIH_short2[!is.na(NIH_short2$value),]
        
        rm(median_cs_2019_01_09_1558, NIH_short, NIH_short2)
        
        medianCS$condition <- as.factor(paste(medianCS$protocol, medianCS$session, sep = "_"))
        
        medianCS$variable <- as.numeric(medianCS$variable)
        
        medianCS <- medianCS[medianCS$variable >= 8 & medianCS$variable <= 18,]
        medianCS$variable <- factor(medianCS$variable , order = TRUE, levels =8:18)
        
    
        runKrus_times(medianCS)
    
        
        
        write.csv(runKrus_times(medianCS),"//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/CS_medianModels.csv", row.names = FALSE)
    
        
        ###add graphh
     
        
        ggplot(medianCS, aes(x = condition, y = value, fill =condition))+
         geom_boxplot()+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="none") +
          labs(x= "", y = "Median CS")+
          scale_fill_manual(values=c("grey42","deepskyblue2", "grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/medianCS.png", width = 5, height = 3, units = "in", dpi = 400)
        
     
        #active
        ggplot(medianCS[medianCS$condition == "active_baseline" | medianCS$condition == "active_intervention",], aes(x = variable, y = value, colour =condition))+
          geom_boxplot()+
    
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Median CS")+
          scale_colour_manual(values=c("grey42","deepskyblue2")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_medianCS_active.png", width = 8, height = 6, units = "in", dpi = 400)
        
        ###Perform t-tests
        tTestData1 <- medianCS[medianCS$condition == "active_baseline" | medianCS$condition == "active_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(wilcox.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        #placebo
        ggplot(medianCS[medianCS$condition == "placebo_baseline" | medianCS$condition == "placebo_intervention",], aes(x = variable, y = value, colour =condition))+
          geom_boxplot()+
    
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Median CS")+
          scale_colour_manual(values=c("grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        ###Perform t-tests
        tTestData1 <- medianCS[medianCS$condition == "placebo_baseline" | medianCS$condition == "placebo_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(wilcox.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_medianCS_placebo.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
        #baseline
        ggplot(medianCS[medianCS$condition == "placebo_baseline" | medianCS$condition == "active_baseline",], aes(x = variable, y = value, colour =condition))+
          geom_boxplot()+
    
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Median CS")+
          scale_colour_manual(values=c("grey42","grey22")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        
        ###Perform t-tests
        tTestData1 <- medianCS[medianCS$condition == "placebo_baseline" | medianCS$condition == "active_baseline",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(wilcox.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_medianCS_baseline.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
    
    #median Log(CLA)##############################################################
        median_log10CircadianLight_2019_01_09_1559 <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/tables/median_log10CircadianLight_2019-01-09_1559.xlsx")
        median_log10CircadianLight_2019_01_09_1559$protocol <- ifelse(median_log10CircadianLight_2019_01_09_1559$protocol == "placbeo", "placebo", median_log10CircadianLight_2019_01_09_1559$protocol)
        
        NIH_short <- subset(median_log10CircadianLight_2019_01_09_1559, group == "short_term")
        
        
        NIH_short2 <- melt(NIH_short, id = c("id", "group", "session", "protocol"))
        NIH_short2$value <- as.numeric(NIH_short2$value )
        
        meanLogCLA <- NIH_short2[!is.na(NIH_short2$value),]
        
        rm(median_log10CircadianLight_2019_01_09_1559, NIH_short, NIH_short2)
        
        meanLogCLA$condition <- as.factor(paste(meanLogCLA$protocol, meanLogCLA$session, sep = "_"))
        
        meanLogCLA$variable <- as.numeric(meanLogCLA$variable)
        
        meanLogCLA <- meanLogCLA[meanLogCLA$variable >= 8 & meanLogCLA$variable <= 18,]
        meanLogCLA$variable <- factor(meanLogCLA$variable , order = TRUE, levels =8:18)
        
        
        runKrus_times(meanLogCLA)
        
        
        
        write.csv(runKrus_times(meanLogCLA),"//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/medianLogCLAModels.csv", row.names = FALSE)
        
        ggplot(meanLogCLA, aes(x = condition, y = value, fill =condition))+
          geom_boxplot()+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="none") +
          labs(x= "", y = "Median log10(CLA)")+
          scale_fill_manual(values=c("grey42","deepskyblue2", "grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/medianlogCLA.png", width = 5, height = 3, units = "in", dpi = 400)
        
        
        #active
        ggplot(meanLogCLA[meanLogCLA$condition == "active_baseline" | meanLogCLA$condition == "active_intervention",], aes(x = variable, y = value, colour =condition))+
          geom_boxplot()+
          
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Median log10(CLA)")+
          scale_colour_manual(values=c("grey42","deepskyblue2")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_medianLogCLA_active.png", width = 8, height = 6, units = "in", dpi = 400)
        
        ###Perform t-tests
        tTestData1 <- meanLogCLA[meanLogCLA$condition == "active_baseline" | meanLogCLA$condition == "active_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(wilcox.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        #placebo
        ggplot(meanLogCLA[meanLogCLA$condition == "placebo_baseline" | meanLogCLA$condition == "placebo_intervention",], aes(x = variable, y = value, colour =condition))+
          geom_boxplot()+
          
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Median log10(CLA)")+
          scale_colour_manual(values=c("grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        ###Perform t-tests
        tTestData1 <- meanLogCLA[meanLogCLA$condition == "placebo_baseline" | meanLogCLA$condition == "placebo_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(wilcox.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_medianLogCLA_placebo.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
        #baseline
        ggplot(meanLogCLA[meanLogCLA$condition == "placebo_baseline" | meanLogCLA$condition == "active_baseline",], aes(x = variable, y = value, colour =condition))+
          geom_boxplot()+
          
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Median log10(CLA)")+
          scale_colour_manual(values=c("grey42","grey22")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        
        ###Perform t-tests
        tTestData1 <- meanLogCLA[meanLogCLA$condition == "placebo_baseline" | meanLogCLA$condition == "active_baseline",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(wilcox.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_medianLogCLA_baseline.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
    

    #median Log(LUX)##############################################################
       
        median_log10Illuminance_2019_01_09_1559 <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/tables/median_log10Illuminance_2019-01-09_1559.xlsx")
        median_log10Illuminance_2019_01_09_1559$protocol <- ifelse(median_log10Illuminance_2019_01_09_1559$protocol == "placbeo", "placebo", median_log10Illuminance_2019_01_09_1559$protocol)
        
        NIH_short <- subset(median_log10Illuminance_2019_01_09_1559, group == "short_term")
        
        
        NIH_short2 <- melt(NIH_short, id = c("id", "group", "session", "protocol"))
        NIH_short2$value <- as.numeric(NIH_short2$value )
        
        meanLogLUX <- NIH_short2[!is.na(NIH_short2$value),]
        
        rm(median_log10Illuminance_2019_01_09_1559, NIH_short, NIH_short2)
        
        meanLogLUX$condition <- as.factor(paste(meanLogLUX$protocol, meanLogLUX$session, sep = "_"))
        
        meanLogLUX$variable <- as.numeric(meanLogLUX$variable)
        
        meanLogLUX <- meanLogLUX[meanLogLUX$variable >= 8 & meanLogLUX$variable <= 18,]
        meanLogLUX$variable <- factor(meanLogLUX$variable , order = TRUE, levels =8:18)
        
        source('~/GitHub/NIHAlzheimersPhase2/Krsucal-hour-tests.R', echo=TRUE)
        runKrus_times(meanLogLUX)
        
        
        
        write.csv(runKrus_times(meanLogLUX),"//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/medianLogLUXModels.csv", row.names = FALSE)
        
        ggplot(meanLogLUX, aes(x = condition, y = value, fill =condition))+
          geom_boxplot()+
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="none") +
          labs(x= "", y = "Median log10(LUX)")+
          scale_fill_manual(values=c("grey42","deepskyblue2", "grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/medianlogLUX.png", width = 5, height = 3, units = "in", dpi = 400)
        
        
        #active
        ggplot(meanLogLUX[meanLogLUX$condition == "active_baseline" | meanLogLUX$condition == "active_intervention",], aes(x = variable, y = value, colour =condition))+
          geom_boxplot()+
          
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Median log10(LUX)")+
          scale_colour_manual(values=c("grey42","deepskyblue2")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_medianLogLUX_active.png", width = 8, height = 6, units = "in", dpi = 400)
        
        ###Perform t-tests
        tTestData1 <- meanLogLUX[meanLogLUX$condition == "active_baseline" | meanLogLUX$condition == "active_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(wilcox.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        #placebo
        ggplot(meanLogLUX[meanLogLUX$condition == "placebo_baseline" | meanLogLUX$condition == "placebo_intervention",], aes(x = variable, y = value, colour =condition))+
          geom_boxplot()+
          
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Median log10(LUX)")+
          scale_colour_manual(values=c("grey42","orangered1")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        ###Perform t-tests
        tTestData1 <- meanLogLUX[meanLogLUX$condition == "placebo_baseline" | meanLogLUX$condition == "placebo_intervention",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(wilcox.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_medianLogLUX_placebo.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
        #baseline
        ggplot(meanLogLUX[meanLogLUX$condition == "placebo_baseline" | meanLogLUX$condition == "active_baseline",], aes(x = variable, y = value, colour =condition))+
          geom_boxplot()+
          
          theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
          labs(x= "Time of day", y = "Median log10(LUX)")+
          scale_colour_manual(values=c("grey42","grey22")) +
          
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
        
        ###Perform t-tests
        tTestData1 <- meanLogLUX[meanLogLUX$condition == "placebo_baseline" | meanLogLUX$condition == "active_baseline",]
        
        for(i in 8:18){
          print("----------------------------------------")
          print(i)
          print(wilcox.test(value ~ condition, data = tTestData1[tTestData1$variable == i,], var.equal = TRUE))
          
        }
        
        
        ggsave("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/graphs/hour_medianLogLUX_baseline.png", width = 8, height = 6, units = "in", dpi = 400)
        
        
        
        
        
        
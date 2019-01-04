library(readxl)
library(reshape2)

Subject_List_1 <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/Subject List_1.xlsx", 
                             col_types = c("numeric", "numeric", "text", 
                                           "numeric", "numeric", "text", "text", 
                                           "numeric", "numeric", "date", "numeric", 
                                           "numeric", "date", "text", "date", 
                                           "numeric", "numeric", "date", "numeric", 
                                           "text", "date", "text", "date", "numeric", 
                                           "text"))
NIH_Alz_questionnaires <- read_excel("C:/Users/roohac/Google Drive/LRC/NIH_alzheimer_project/data_directory/NIH_Alz_questionnaires_stats_2017 with demographics.xlsx", 
                                     col_types = c("numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "text", "text", 
                                                   "numeric", "numeric", "numeric", 
                                                   "text"))
nih_alz <- read_excel("C:/Users/roohac/Google Drive/LRC/NIH_alzheimer_project/data_directory/nih_alz_dfa_2018)_10_18.xlsx")

average_cs_2018_02_23_1018 <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/tables/average_cs_2018-02-23_1018.xlsx")


NIH_short <- subset(average_cs_2018_02_23_1018, group == "short_term")
dfaData <- melt(data.frame(nih_alz), id = c("Subject", "meanDFA", "meanDFA2"))

NIH_short2 <- melt(NIH_short, id = c("id", "group", "session", "protocol"))

NIH_short2$value <- as.numeric(NIH_short2$value )
NIH_short2$variable <- as.numeric(NIH_short2$variable )

NIH_short3 <- NIH_short2[!is.na(NIH_short2$value),]



NIH_short3$age <-  NIH_Alz_questionnaires[match(NIH_short3$id, NIH_Alz_questionnaires$Subject),]$Age

NIH_short3$directionDFA <-  dfaDiff2[match(NIH_short3$id, dfaDiff2$Subject),]$direction
NIH_short3$condition <- as.factor(paste(NIH_short3$protocol, NIH_short3$session, sep = "_"))

NIH_short3$variable <- as.numeric(NIH_short3$variable)
CS_directionModel <- lme(value ~  directionDFA*condition*variable ,  random=~1|id/condition/variable,  data = NIH_short3[is.finite(NIH_short3$value) & !is.na(NIH_short3$directionDFA),])
anova(CS_directionModel)
summary(CS_directionModel)

ggplot(NIH_short3[!is.na(NIH_short3$directionDFA),], aes(x = variable, y = value, group = directionDFA, colour =directionDFA ))+
  geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(.~condition)+
  labs(x= "Time of Day", y = "CS")



SubsMeanCS0 <- aggregate(value ~ id * directionDFA, FUN = mean, data = NIH_short3[!is.na(NIH_short3$directionDFA) & NIH_short3$variable > 0 & NIH_short3$variable < 25,])

CondMeanCS0 <- summarySE(data = SubsMeanCS0, measurevar = "value", groupvars = c( "directionDFA"))

t.test(value ~ directionDFA, data = SubsMeanCS0)
ggplot(CondMeanCS0, aes(x = directionDFA, y = value, fill =directionDFA ))+
  geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.9))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  theme(legend.title=element_blank()) +
  labs(x= "", y = "CS")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggsignif::geom_signif( xmin = 1, xmax = 2, annotations = ".0153", y_position = .18)
  


SubsMeanCS <- aggregate(value ~ id * condition*directionDFA, FUN = mean, data = NIH_short3[!is.na(NIH_short3$directionDFA) & NIH_short3$variable > 17 & NIH_short3$variable < 25,])

CondMeanCS <- summarySE(data = SubsMeanCS, measurevar = "value", groupvars = c("condition", "directionDFA"))

ggplot(CondMeanCS, aes(x = condition, y = value, fill =directionDFA ))+
  geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.9))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  theme(legend.title=element_blank()) +
  labs(x= "", y = "CS")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))




###Looking at differents in time
NIH_short3$condition <- gsub("_", "-", NIH_short3$condition )

NIH_short3$condition_time <- paste(NIH_short3$condition, NIH_short3$variable, "", sep = "_")
SubsMeanCS0 <- aggregate(value ~ id * condition_time*condition*variable, FUN = mean, data = NIH_short3[!is.na(NIH_short3$directionDFA) & NIH_short3$variable > 6 & NIH_short3$variable < 23,])

tTestTable <- performTtests(SubsMeanCS0)

tTestTable$keep <- NA
library(stringr)

for(i in 7:25){
  numCount <- str_count(tTestTable$ComparedGroups, paste("_", i, "_", sep = ""))
  tTestTable$numCount <- numCount
  tTestTable$keep <- ifelse(is.na(tTestTable$keep) & tTestTable$numCount ==2, TRUE, tTestTable$keep)
  }
 
tTestTable_specif <- subset(tTestTable, keep == TRUE)
tTestTable_specif$hour <- as.numeric(gsub("[^\\d]+", "", tTestTable_specif$ComparedGroup1, perl=TRUE))
SubsMeanCS0$variable <- as.factor(SubsMeanCS0$variable )
CondMeanCS0 <- summarySE(data = SubsMeanCS0, measurevar = "value", groupvars = c("condition", "variable"))

ggplot(CondMeanCS0, aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","deepskyblue2" ,"grey32", "orangered1")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


#active
ggplot(CondMeanCS0[CondMeanCS0$condition == "active-baseline" | CondMeanCS0$condition == "active-intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
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

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/hour_CS_active.png", width = 8, height = 6, units = "in", dpi = 400)


#placebo
ggplot(CondMeanCS0[CondMeanCS0$condition == "placebo-baseline" | CondMeanCS0$condition == "placebo-intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","orangered1")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/hour_CS_placebo.png", width = 8, height = 6, units = "in", dpi = 400)


#baseline
ggplot(CondMeanCS0[CondMeanCS0$condition == "placebo-baseline" | CondMeanCS0$condition == "active-baseline",], aes(x = variable, y = value, colour =condition, group = condition ))+
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

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/hour_CS_baseline.png", width = 8, height = 6, units = "in", dpi = 400)


###Looking at differents in time for each group (greater or less)

##GREATER
NIH_short3$condition_time <- paste(NIH_short3$condition, NIH_short3$variable, "", sep = "_")
SubsMeanCS3 <- aggregate(value ~ id * condition_time*condition*variable*directionDFA, FUN = mean, data = NIH_short3[!is.na(NIH_short3$directionDFA) & NIH_short3$variable > 8 & NIH_short3$variable < 23 & NIH_short3$directionDFA == "greater",,])

tTestTable2 <- performTtests(SubsMeanCS3)

tTestTable2$keep <- NA
library(stringr)

for(i in 7:25){
  numCount <- str_count(tTestTable2$ComparedGroups, paste("_", i, "_", sep = ""))
  tTestTable2$numCount <- numCount
  tTestTable2$keep <- ifelse(is.na(tTestTable2$keep) & tTestTable2$numCount ==2, TRUE, tTestTable2$keep)
}

tTestTable2_specif <- subset(tTestTable2, keep == TRUE)
tTestTable2_specif$hour <- as.numeric(gsub("[^\\d]+", "", tTestTable_specif$ComparedGroup1, perl=TRUE))
SubsMeanCS3$variable <- as.factor(SubsMeanCS3$variable )
condMeanCS3 <- summarySE(data = SubsMeanCS3, measurevar = "value", groupvars = c("condition", "variable"))

ggplot(condMeanCS3, aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","deepskyblue2" ,"grey32", "orangered1")) +
  ggtitle("Greater group")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


#active
ggplot(condMeanCS3[condMeanCS3$condition == "active-baseline" | condMeanCS3$condition == "active-intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  ggtitle("Greater group")+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","deepskyblue2")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/hour_CS_active_greater.png", width = 8, height = 6, units = "in", dpi = 400)


#placebo
ggplot(condMeanCS3[condMeanCS3$condition == "placebo-baseline" | condMeanCS3$condition == "placebo-intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  ggtitle("Greater group")+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","orangered1")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/hour_CS_placebo_greater.png", width = 8, height = 6, units = "in", dpi = 400)


#baseline
ggplot(condMeanCS3[condMeanCS3$condition == "placebo-baseline" | condMeanCS3$condition == "active-baseline",], aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","grey22")) +
  ggtitle("Greater group")+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/hour_CS_baseline_greater.png", width = 8, height = 6, units = "in", dpi = 400)



##less
NIH_short3$condition_time <- paste(NIH_short3$condition, NIH_short3$variable, "", sep = "_")
SubsMeanCS3 <- aggregate(value ~ id * condition_time*condition*variable*directionDFA, FUN = mean, data = NIH_short3[!is.na(NIH_short3$directionDFA) & NIH_short3$variable > 8 & NIH_short3$variable < 23 & NIH_short3$directionDFA == "less",])

tTestTable2 <- performTtests(SubsMeanCS3)

tTestTable2$keep <- NA
library(stringr)

for(i in 7:25){
  numCount <- str_count(tTestTable2$ComparedGroups, paste("_", i, "_", sep = ""))
  tTestTable2$numCount <- numCount
  tTestTable2$keep <- ifelse(is.na(tTestTable2$keep) & tTestTable2$numCount ==2, TRUE, tTestTable2$keep)
}

tTestTable2_specif <- subset(tTestTable2, keep == TRUE)
tTestTable2_specif$hour <- as.numeric(gsub("[^\\d]+", "", tTestTable_specif$ComparedGroup1, perl=TRUE))
SubsMeanCS3$variable <- as.factor(SubsMeanCS3$variable )
condMeanCS3 <- summarySE(data = SubsMeanCS3, measurevar = "value", groupvars = c("condition", "variable"))

ggplot(condMeanCS3, aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","deepskyblue2" ,"grey32", "orangered1")) +
  ggtitle("Greater group")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


#active
ggplot(condMeanCS3[condMeanCS3$condition == "active-baseline" | condMeanCS3$condition == "active-intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  ggtitle("Less group")+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","deepskyblue2")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/hour_CS_active_less.png", width = 8, height = 6, units = "in", dpi = 400)


#placebo
ggplot(condMeanCS3[condMeanCS3$condition == "placebo-baseline" | condMeanCS3$condition == "placebo-intervention",], aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  ggtitle("Less group")+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","orangered1")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/hour_CS_placebo_less.png", width = 8, height = 6, units = "in", dpi = 400)


#baseline
ggplot(condMeanCS3[condMeanCS3$condition == "placebo-baseline" | condMeanCS3$condition == "active-baseline",], aes(x = variable, y = value, colour =condition, group = condition ))+
  #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.3))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  labs(x= "Time of day", y = "CS")+
  scale_colour_manual(values=c("grey42","grey22")) +
  ggtitle("Less group")+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/hour_CS_baseline_less.png", width = 8, height = 6, units = "in", dpi = 400)



###Location

NIH_short3$location <-  Subject_List_1[match( NIH_short3$id, Subject_List_1$`Subject #`),]$`Facility location`



ggplot(NIH_short3[!is.na(NIH_short3$directionDFA),], aes(x = variable, y = value, group = directionDFA, colour =directionDFA ))+
  geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(location ~condition)+
  labs(x= "Time of Day", y = "CS")

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/location_condition_CS.png", width = 8, height = 15, units = "in", dpi = 400)


SubsMeanCS <- aggregate(value ~ id * condition*directionDFA*location, FUN = mean, data = NIH_short3[!is.na(NIH_short3$directionDFA) & NIH_short3$variable > 18 & NIH_short3$variable < 25,])

CondMeanCS <- summarySE(data = SubsMeanCS, measurevar = "value", groupvars = c("condition", "directionDFA", "location"))

ggplot(CondMeanCS, aes(x = condition, y = value, fill = directionDFA ))+
  geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.9))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  theme(legend.title=element_blank()) +
  labs(x= "", y = "CS")+
  facet_grid(directionDFA~location)+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/location_condition_aveCS-9-midnight.png", width = 15, height = 6, units = "in", dpi = 400)


###Season
NIH_Alzheimers_Actigraph_Stats_shortTerm <- read_excel("C:/Users/roohac/Google Drive/LRC/NIH_alzheimer_project/data_directory/NIH_Alzheimers_Actigraph Stats_sheet November 2017.xlsx", 
                                                       col_types = c("text", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "text", "numeric", "text",  "text", "text", "date"))
                                                                     
NIH_Alzheimers_Actigraph_Stats_shortTerm$intervention0 <- as.factor(substr(NIH_Alzheimers_Actigraph_Stats_shortTerm$period, 1 , 1))
NIH_Alzheimers_Actigraph_Stats_shortTerm$condition <- as.factor(paste(NIH_Alzheimers_Actigraph_Stats_shortTerm$intervention0, NIH_Alzheimers_Actigraph_Stats_shortTerm$intervention, sep = "_"))
NIH_Alzheimers_Actigraph_Stats_shortTerm$condition <-ifelse(NIH_Alzheimers_Actigraph_Stats_shortTerm$condition == "B_A", "active_baseline", 
                                                            ifelse(NIH_Alzheimers_Actigraph_Stats_shortTerm$condition == "I_A", "active_intervention", 
                                                                   ifelse(NIH_Alzheimers_Actigraph_Stats_shortTerm$condition == "B_P", "placebo_baseline", "placebo_intervention" 
                                                                   )))
NIH_Alzheimers_Actigraph_Stats_shortTerm$sub_cond <- paste(NIH_Alzheimers_Actigraph_Stats_shortTerm$Subject, NIH_Alzheimers_Actigraph_Stats_shortTerm$condition, sep = "_")                                                                                                                                      

NIH_short3$sub_cond <- paste(NIH_short3$id, NIH_short3$condition, sep = "_")                                                                                                                                      
NIH_short3$season <-  NIH_Alzheimers_Actigraph_Stats_shortTerm[match( NIH_short3$sub_cond, NIH_Alzheimers_Actigraph_Stats_shortTerm$sub_cond),]$season



ggplot(NIH_short3[!is.na(NIH_short3$directionDFA),], aes(x = variable, y = value, group = directionDFA, colour =directionDFA ))+
  geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(condition ~season)+
  labs(x= "Time of Day", y = "CS")
ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/season_condition_CS.png", width = 8, height = 15, units = "in", dpi = 400)


ggplot(NIH_short3[!is.na(NIH_short3$directionDFA),], aes(x = variable, y = value, group = directionDFA, colour =directionDFA ))+
  geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(season~condition)+
  labs(x= "Time of Day", y = "CS")
ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/season_condition_CS2.png", width = 15, height = 8, units = "in", dpi = 400)


ggplot(NIH_short3[!is.na(NIH_short3$directionDFA),], aes(x = variable, y = value, group = sub_cond, shape = directionDFA, colour =  season, linetype =  directionDFA))+
  geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(id~condition)+
  labs(x= "Time of Day", y = "CS")

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/subject_season_condition_CS.png", width = 8, height = 49.5, units = "in", dpi = 400)
##look at each subject
idList <- unique(NIH_short3$id)

sub <- idList[1]
ggplot(NIH_short3[!is.na(NIH_short3$directionDFA) & NIH_short3$id == sub,], aes(x = variable, y = value, group = sub_cond, colour = season ))+
  geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(.~condition)+
  labs(x= "Time of Day", y = "CS")+
  ggtitle(NIH_short3[!is.na(NIH_short3$directionDFA) & NIH_short3$id == sub,]$directionDFA[1])

SubsMeanCS <- aggregate(value ~ id * condition*directionDFA*location, FUN = mean, data = NIH_short3[!is.na(NIH_short3$directionDFA) & NIH_short3$variable > 8 & NIH_short3$variable < 11,])

CondMeanCS <- summarySE(data = SubsMeanCS, measurevar = "value", groupvars = c("condition", "directionDFA", "location"))

ggplot(CondMeanCS, aes(x = condition, y = value, fill = directionDFA ))+
  geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.9))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  theme(legend.title=element_blank()) +
  labs(x= "", y = "CS")+
  facet_grid(directionDFA~location)+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


### fixture type
NIH_short3$light0 <-  Subject_List_1[match( NIH_short3$id, Subject_List_1$`Subject #`),]$`Active or Placebo`

NIH_short3$light0 <- toupper(NIH_short3$light0)
NIH_short3$light <- NIH_short3$light0
NIH_short3$light <-  gsub("ACTIVE", "", NIH_short3$light, NIH_short3$light0 )
NIH_short3$light <-  gsub("PLACEBO", "", NIH_short3$light, NIH_short3$light0 )
NIH_short3$light <-  gsub(" - ", "", NIH_short3$light, NIH_short3$light0 )

unique(NIH_short3$light )

ggplot(NIH_short3[!is.na(NIH_short3$directionDFA),], aes(x = variable, y = value, group = directionDFA, colour =directionDFA ))+
  geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(light ~condition)+
  labs(x= "Time of Day", y = "CS")

ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/LIGHT_FIXTURE_condition_CS.png", width = 8, height = 20, units = "in", dpi = 400)


###Model testing all factors
NIH_short3$location <- as.factor(NIH_short3$location)
CS_directionModel2 <- lme(value ~  directionDFA*season*condition*variable ,  random=~1|location/id/season/condition/variable,  data = NIH_short3[is.finite(NIH_short3$value) & !is.na(NIH_short3$directionDFA) & !is.na(NIH_short3$location),])
anova(CS_directionModel2)

CS_directionModel3 <- lme(value ~  directionDFA*season*condition*variable ,  random=~1|location/id/light/season/condition/variable,  data = NIH_short3[is.finite(NIH_short3$value) & !is.na(NIH_short3$directionDFA) & !is.na(NIH_short3$location),])
anova(CS_directionModel3)


#CS_directionModel3.1 <- lme(value ~  directionDFA*season*condition*variable , random=list(~1|location, ~1|light, ~1|season,~1|id/condition/variable),  data = NIH_short3[is.finite(NIH_short3$value) & !is.na(NIH_short3$directionDFA) & !is.na(NIH_short3$location),])

CS_directionModel3.1 <- lme(value ~  directionDFA*season*condition*variable , random=list(~1|location, ~1|id, ~1|season, ~1|light, ~1|condition ),  data = NIH_short3[is.finite(NIH_short3$value) & !is.na(NIH_short3$directionDFA) & !is.na(NIH_short3$location),])
anova(CS_directionModel3.1)
summary(CS_directionModel3.1)

CS_directionModel4 <- lme(value ~  location*variable ,  random=~1|id/condition/variable,  data = NIH_short3[is.finite(NIH_short3$value) & !is.na(NIH_short3$directionDFA) & !is.na(NIH_short3$location),])
anova(CS_directionModel4)

CS_directionModel5 <- lme(value ~  light*variable ,  random=~1|id/condition/variable,  data = NIH_short3[is.finite(NIH_short3$value) & !is.na(NIH_short3$directionDFA) & !is.na(NIH_short3$location),])
anova(CS_directionModel5)



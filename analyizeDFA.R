library(readxl)
library(readr)
library(ggplot2)
library(reshape2)
library(Rmisc)
source('~/GitHub/NIHAlzheimersPhase2/checkDirection.R', echo=TRUE)
NIH_alzShort_ALL <- read_csv("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/NIH-data.csv")
nih_alz <- read_excel("C:/Users/roohac/Google Drive/LRC/NIH_alzheimer_project/data_directory/nih_alz_dfa_2018)_10_18.xlsx")
NIH_Alz_questionnaires <- read_excel("C:/Users/roohac/Google Drive/LRC/NIH_alzheimer_project/data_directory/NIH_Alz_questionnaires_stats_2017 with demographics.xlsx", 
                                     col_types = c("numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "text", "text", 
                                                   "numeric", "numeric", "numeric", 
                                                   "text"))


NIH_Alzheimers_Actigraph_Stats_shortTerm <- read_excel("C:/Users/roohac/Google Drive/LRC/NIH_alzheimer_project/data_directory/NIH_Alzheimers_Actigraph Stats_sheet November 2017.xlsx", 
                                                       col_types = c("text", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "text", "numeric", "text", 
                                                                     
                                                                     "text", "text", "date"))


####
###
nih_alz$meanDFA <- (nih_alz$active_baseline + nih_alz$placebo_baseline)/2
nih_alz$meanDFA2 <- (nih_alz$active_baseline + nih_alz$active_intervention + nih_alz$placebo_baseline + nih_alz$placebo_intervention)/4


dfaData <- melt(data.frame(nih_alz), id = c("Subject", "meanDFA", "meanDFA2"))

  
dfaData$group <- NIH_alzShort_ALL[match(dfaData$Subject, NIH_alzShort_ALL$Subject),]$group
dfaData$season <- NIH_alzShort_ALL[match(dfaData$Subject, NIH_alzShort_ALL$Subject),]$season

dfaData$period <- ifelse(dfaData$variable == "active_baseline" | dfaData$variable == "active_intervention", "active", "placebo")
dfaData$treatment <- ifelse(dfaData$variable == "placebo_baseline" | dfaData$variable == "active_baseline", "baseline", "intervention")

dfaData$outlier <- ifelse(dfaData$value > 1.147488 | dfaData$value < 0.3962867, TRUE, FALSE)

#OUlters: 3 19, 33, 52
#dfaData <- subset(dfaData, Subject != 3 &  Subject != 19 & Subject != 33 &  Subject != 52)

dfaData <- subset(dfaData, Subject != 19 & Subject != 52)

doc = docx()
doc <- addTitle( doc, "NIH DFA analysis", level = 1 )

dfaData$sub_cond <- paste(dfaData$Subject, dfaData$period, sep = "_")

dfaModel1 <- lme(value ~  variable ,  random=~1|Subject/variable,  data = dfaData[is.finite(dfaData$value) ,])
anova(dfaModel1)

dfaDataTest <- aggregate(value~Subject*variable, data = dfaData, FUN = mean)
performTtests(dfaDataTest)
gg_01 <- ggplot(dfaData, aes(x = variable, y = value, group = sub_cond, colour = period))+
  geom_point() +
  geom_line(colour = "black")+
  labs(y = "DFA", x = "") +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values=c( "deepskyblue2",  "orangered1")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
doc <- addPlot(doc = doc, fun = print, x = gg_01, vector.graphic = FALSE, width = 6, height = 3)


t.test(value ~ variable, data = subset(dfaData, treatment == "intervention"), paired = TRUE)


dfaSummary <- summarySE(dfaData, measurevar = "value", groupvars = c("variable", "period", "treatment")) 

gg_02 <- ggplot(dfaSummary, aes(x = variable, y = value, fill = variable))+
  geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.9))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  theme(legend.title=element_blank()) +
  geom_text(aes(label = round(value, digits = 3), vjust=2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("grey42","deepskyblue2" ,"grey32", "orangered1")) +
  coord_cartesian(ylim = c(.6, .9)) + 
  labs(y = "DFA", x = "")

doc <- addPlot(doc = doc, fun = print, x = gg_02, vector.graphic = FALSE, width = 5, height = 3)



###Broken by groups

dfaModel2 <- lme(value ~  variable ,  random=~1|Subject/variable,  data = dfaData[is.finite(dfaData$value) & dfaData$group == "NE" ,])
anova(dfaModel2)

dfaDataTest <- aggregate(value~Subject*variable, data = dfaData[is.finite(dfaData$value) & dfaData$group == "NE" ,], FUN = mean)
performTtests(dfaDataTest)



dfaSummary2 <- summarySE(dfaData, measurevar = "value", groupvars = c("variable", "period", "treatment", "group")) 

gg_02.1 <- ggplot(dfaSummary2[dfaSummary2$group == "EN",], aes(x = variable, y = value, fill = variable))+
  geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.9))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  theme(legend.title=element_blank()) +
  geom_text(aes(label = round(value, digits = 3), vjust=2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("grey42","deepskyblue2" ,"grey32", "orangered1")) +
  coord_cartesian(ylim = c(.6, .9)) + 
  labs(y = "DFA", x = "")+
  ggtitle("Order 1")

dfaSummary2$variable <- factor(dfaSummary2$variable, levels = c("placebo_baseline", "placebo_intervention", "active_baseline", "active_intervention"))

gg_02.2 <- ggplot(dfaSummary2[dfaSummary2$group == "NE",], aes(x = variable, y = value, fill = variable))+
  geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.9))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  theme(legend.title=element_blank()) +
  geom_text(aes(label = round(value, digits = 3), vjust=2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("grey42","orangered1","grey32","deepskyblue2"  )) +
  coord_cartesian(ylim = c(.6, .9)) + 
  labs(y = "DFA", x = "")+
  ggtitle("Order 2")

doc <- addPlot(doc = doc, fun = print, x = gg_02.2, vector.graphic = FALSE, width = 5, height = 3)

nih_alz$difference_active <- nih_alz$active_intervention - nih_alz$active_baseline
nih_alz$difference_placebo <- nih_alz$placebo_intervention - nih_alz$placebo_baseline

dfaDiff <- nih_alz
dfaDiff$active_baseline <- NULL
dfaDiff$active_intervention <- NULL
dfaDiff$placebo_baseline <- NULL
dfaDiff$placebo_intervention <- NULL


dfaDiff2 <- melt(data.frame(dfaDiff), id = c("Subject", "meanDFA", "meanDFA2"))
dfaDiff2$period <- ifelse(dfaDiff2$variable == "difference_active" , "active", "placebo")

dfaDiff2$direction <- ifelse(dfaDiff2[dfaDiff2$variable == "difference_active",]$value > dfaDiff2[dfaDiff2$variable== "difference_placebo",]$value,
                             "greater", "less")

gg_03.0 <- ggplot(dfaDiff2, aes(x = variable, y = value, group = Subject, colour = variable ))+
  geom_point() +
  geom_line(color = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  labs(x = "", y = "DFA")+
  scale_fill_manual(values=c("deepskyblue2" ,"orangered1")) +
  ylim(-.8, .8)
  
ggsave("C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/normalizedDFA.png", width = 6, height = 5, units = "in", dpi = 400)


gg_03 <- ggplot(dfaDiff2, aes(x = variable, y = value, group = Subject, colour = direction))+
  geom_point() +
  geom_line() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  labs(x = "", y = "DFA")

doc <- addPlot(doc = doc, fun = print, x = gg_03, vector.graphic = FALSE, width = 5, height = 3)


nums <- aggregate(meanDFA ~ Subject + direction, FUN = mean, data =  dfaDiff2)

numSide <- data.frame(table(nums$direction))

numeSide2 <- data.frame(t(numSide))
colnames(numeSide2) = as.character(unlist( numeSide2[1, ])) # the first row will be the header
numeSide2 = numeSide2[-1, ] 

doc = addFlexTable(doc, vanilla.table(numeSide2))

doc = addParagraph( doc,"Paired t-test between difference_active & difference_placebo ", stylename = "Normal" )
t_test <- t.test(value ~ variable, data = dfaDiff2, paired = TRUE)
addTtest2Doc(t_test, doc)

dfaDiffSummary <- summarySE(dfaDiff2, measurevar = "value", groupvars = c("variable")) 

gg_03.1 <- ggplot(dfaDiffSummary, aes(x = variable, y = value, fill = variable))+
  geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.9))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  theme(legend.title=element_blank()) +
  geom_text(aes(label = round(value, digits = 3), vjust=2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(legend.position="none")+
  scale_fill_manual(values=c( "deepskyblue2",  "orangered1")) +
  ggsignif::geom_signif(comparisons = list(c("difference_active", "difference_placebo")), annotations = ".72", y_position = .049)+
  labs(x = "", y = "Difference normalization DFA")

doc <- addPlot(doc = doc, fun = print, x = gg_03.1, vector.graphic = FALSE, width = 5, height = 4)

doc = addParagraph( doc,"Paired t-test between difference_active & difference_placebo: direction = greater", stylename = "Normal" )
t_test <- t.test(value ~ variable, data = dfaDiff2[dfaDiff2$direction == "greater",], paired = TRUE)
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"Paired t-test between difference_active & difference_placebo: direction = less", stylename = "Normal" )
t_test <- t.test(value ~ variable, data = dfaDiff2[dfaDiff2$direction == "less",], paired = TRUE)
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"unpaired t-test between greater & less: condition = difference_active", stylename = "Normal" )
t_test <- t.test(value ~ direction, data = dfaDiff2[dfaDiff2$variable == "difference_active",], var.equal = TRUE)
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"unpaired t-test between greater & less: condition = difference_placebo", stylename = "Normal" )
t_test <- t.test(value ~ direction, data = dfaDiff2[dfaDiff2$variable == "difference_placebo",], var.equal = TRUE)
addTtest2Doc(t_test, doc)


#####
doc = addParagraph( doc,"unpaired t-test of subject mean baseline DFA between greater & less: condition = difference_placebo", stylename = "Normal" )

dfaData$directionDFA <- dfaDiff2[match( dfaData$Subject, dfaDiff2$Subject),]$direction

directionModel1 <- lme(value ~  directionDFA*variable ,  random=~1|Subject/variable,  data = dfaData[is.finite(dfaData$value),])
anova(directionModel1)
####
###
###Between t-tests
doc = addParagraph( doc,"unpaired t-test between greater & less: condition = active_baseline", stylename = "Normal" )
t_test <- t.test(value ~ directionDFA, data = subset(dfaData, variable == "active_baseline"))
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"unpaired t-test between greater & less: condition = active_intervention", stylename = "Normal" )
t_test <- t.test(value ~ directionDFA, data = subset(dfaData, variable == "active_intervention"))
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"unpaired t-test between greater & less: condition = placebo_baseline", stylename = "Normal" )
t_test <- t.test(value ~ directionDFA, data = subset(dfaData, variable == "placebo_baseline"))
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"unpaired t-test between greater & less: condition = placebo_intervention", stylename = "Normal" )
t_test <- t.test(value ~ directionDFA, data = subset(dfaData, variable == "placebo_intervention"))
addTtest2Doc(t_test, doc)

##within t-tests
doc = addParagraph( doc,"Paired t-test between active_intervention & active_baseline: direction = greater", stylename = "Normal" )
t_test <- t.test(value ~ variable, data = subset(dfaData, directionDFA == "greater" & period == "active"), paired = TRUE)
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"Paired t-test between placebo_intervention & placebo_baseline: direction = greater", stylename = "Normal" )
t_test <- t.test(value ~ variable, data = subset(dfaData, directionDFA == "greater" & period == "placebo"), paired = TRUE)
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"Paired t-test between active_intervention & active_baseline: direction = less", stylename = "Normal" )
t_test <- t.test(value ~ variable, data = subset(dfaData, directionDFA == "less" & period == "active"), paired = TRUE)
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"Paired t-test between placebo_intervention & placebo_baseline: direction = less", stylename = "Normal" )
t_test <- t.test(value ~ variable, data = subset(dfaData, directionDFA == "less" & period == "placebo"), paired = TRUE)
addTtest2Doc(t_test, doc)

###Baselin t-tests
doc = addParagraph( doc,"Paired t-test between active_baseline & placebo_baseline: direction = greater", stylename = "Normal" )
t_test <- t.test(value ~ variable, data = subset(dfaData, directionDFA == "greater" & treatment == "baseline"), paired = TRUE)
addTtest2Doc(t_test, doc)

doc = addParagraph( doc,"Paired t-test between active_baseline & placebo_baseline: direction = less", stylename = "Normal" )
t_test <- t.test(value ~ variable, data = subset(dfaData, directionDFA == "less" & treatment == "baseline"), paired = TRUE)
addTtest2Doc(t_test, doc)

dfaSummary_direction <- summarySE(dfaData, measurevar = "value", groupvars = c("directionDFA", "variable")) 

gg_04 <- ggplot(dfaSummary_direction, aes(x = variable, y = value, fill = directionDFA))+
  geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    
                position=position_dodge(.9))+
  theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
  theme(legend.title=element_blank()) +
  #geom_text(aes(label = round(value, digits = 3), vjust=2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ggsignif::geom_signif( xmin = .75, xmax = 1.25, annotations = ".006", y_position = .95)+
  ggsignif::geom_signif( xmin = 3.75, xmax = 4.25, annotations = ".037", y_position = .95)+
  ggsignif::geom_signif( xmin = .75, xmax = 1.75, annotations = ".0037", y_position = 1.05)+
  ggsignif::geom_signif( xmin = 1.25, xmax = 2.15, annotations = ".0017", y_position = 1.15)+
  ggsignif::geom_signif( xmin = 1.25, xmax = 3.25, annotations = ".0012", y_position = 1.2)

  

doc <- addPlot(doc = doc, fun = print, x = gg_04, vector.graphic = FALSE, width = 5, height = 4)



checkDirection <- FALSE
if(checkDirection){
  
  lessGroup <- unique(dfaDiff2[dfaDiff2$direction == "less",]$Subject)
  greaterGroup <- unique(dfaDiff2[dfaDiff2$direction == "greater",]$Subject)
  
  NIH_alzShort_ALL$directionDFA <- dfaDiff2[match( NIH_alzShort_ALL$Subject, dfaDiff2$Subject),]$direction
  outcomeList <- unique(NIH_alzShort_ALL$variable)
  
  checkDirection(NIH_alzShort_ALL, outcomeList, "none")
  
  
  checkDirection(NIH_alzShort_ALL, outcomeList, "dfa")
  
}


demographCheck <- TRUE
if(demographCheck){
  doc <- addTitle( doc, "NIH DFA analysis: Demographics", level = 1 )
  
  
  
  if(TRUE){
    severity1_dfaData <- aggregate(BIM ~ Subject + directionDFA, FUN = mean, data =  NIH_alzShort_ALL)
    
    doc = addParagraph( doc,"unpaired t-test comparing BIM between greater & less", stylename = "Normal" )
    t_test <- t.test(BIM~ directionDFA, data = severity1_dfaData, var.equal = TRUE)
    addTtest2Doc(t_test, doc)
    
    severitySummary1 <- summarySE(data =  severity1_dfaData[!is.na(severity1_dfaData$BIM) & !is.na(severity1_dfaData$directionDFA),], measurevar = "BIM", groupvars = c("directionDFA"))
    gg_06 <- ggplot(severitySummary1, aes(x = directionDFA, y = BIM, fill = directionDFA))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_errorbar(aes(ymin=BIM-se, ymax=BIM+se),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      geom_text(aes(label = round(BIM, digits = 3), vjust=2)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
      theme(legend.position="none")
    
    doc <- addPlot(doc = doc, fun = print, x = gg_06, vector.graphic = FALSE, width = 3, height = 3)
    
    
    severity2_dfaData <- aggregate(MMSE ~ Subject + directionDFA, FUN = mean, data =  NIH_alzShort_ALL)
    
    doc = addParagraph( doc,"unpaired t-test comparing MMSE between greater & less", stylename = "Normal" )
    t_test <- t.test(MMSE~ directionDFA, data = severity2_dfaData)
    addTtest2Doc(t_test, doc)
    
    severitySummary2 <- summarySE(data =  severity2_dfaData, measurevar = "MMSE", groupvars = c("directionDFA"))
    
    gg_06 <- ggplot(severitySummary2, aes(x = directionDFA, y = MMSE, fill = directionDFA))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_errorbar(aes(ymin=MMSE-se, ymax=MMSE+se),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      geom_text(aes(label = round(MMSE, digits = 3), vjust=2)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
      theme(legend.position="none")
    
    doc <- addPlot(doc = doc, fun = print, x = gg_06, vector.graphic = FALSE, width = 3, height = 3)
    NIH_Alz_questionnaires$directionDFA <- dfaDiff2[match( NIH_Alz_questionnaires$Subject, dfaDiff2$Subject),]$direction
    
    gg_07 <- ggplot(NIH_Alz_questionnaires[!is.na(NIH_Alz_questionnaires$directionDFA),], aes(x = Age, y = MMSE, group = directionDFA, colour = directionDFA))+
      geom_point() +
      geom_smooth(method = "lm")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 
    
    doc <- addPlot(doc = doc, fun = print, x = gg_07, vector.graphic = FALSE, width = 5, height = 3)
    
    NIH_Alz_questionnaires$MMSE <- as.numeric(NIH_Alz_questionnaires$MMSE )
    NIH_Alz_questionnaires$Age <- as.numeric(NIH_Alz_questionnaires$Age )
    
    linearModel1 <- lme(MMSE ~ Age * directionDFA, data = NIH_Alz_questionnaires[!is.na(NIH_Alz_questionnaires$directionDFA) & !is.na(NIH_Alz_questionnaires$MMSE) & !is.na(NIH_Alz_questionnaires$Age),])
    summary(linearModel1)
    anova(linearModel1)
    
    
    
    gg_08 <- ggplot(NIH_Alz_questionnaires[!is.na(NIH_Alz_questionnaires$directionDFA),], aes(x = Age, y = BIM, group = directionDFA, colour = directionDFA))+
      geom_point() +
      geom_smooth(method = "lm")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 
    
    doc <- addPlot(doc = doc, fun = print, x = gg_08, vector.graphic = FALSE, width = 5, height = 3)
    
  }

  

  age_dfaData <- aggregate(Age ~ Subject + directionDFA, FUN = mean, data =  NIH_Alz_questionnaires)
  
  doc = addParagraph( doc,"unpaired t-test comparing Age between greater & less", stylename = "Normal" )
  t_test <-  t.test(Age~ directionDFA, data = age_dfaData)
  addTtest2Doc(t_test, doc)
 
  
  
  dfaAgeSummary <- summarySE(age_dfaData, measurevar = "Age", groupvars = c("directionDFA")) 
  
  gg_09 <- ggplot(dfaAgeSummary, aes(x = directionDFA, y = Age, fill = directionDFA))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=Age-se, ymax=Age+se),
                  width=.2,                    
                  position=position_dodge(.9))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    geom_text(aes(label = round(Age, digits = 3), vjust=2)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
    theme(legend.position="none")+
    ggsignif::geom_signif(comparisons = list(c("greater", "less")), annotations = ".039", y_position = 92)+
    coord_cartesian(ylim = c(60, 95))
  
  doc <- addPlot(doc = doc, fun = print, x = gg_09, vector.graphic = FALSE, width = 5, height = 3)
  
  age_dfaData <- aggregate(Age ~ Subject + directionDFA + Gender, FUN = mean, data =  NIH_Alz_questionnaires)
  
  data.frame(table(age_dfaData$directionDFA, age_dfaData$Gender))
  
  
  dfaDiff$age <- NIH_Alz_questionnaires[match(dfaDiff$Subject, NIH_Alz_questionnaires$Subject),]$Age
  dfaDiff$directionDFA <- NIH_Alz_questionnaires[match(dfaDiff$Subject, NIH_Alz_questionnaires$Subject),]$directionDFA
  dfaDiff$diffDiff_ap <- dfaDiff$difference_active - dfaDiff$difference_placebo
  dfaDiff$meanDFA2 <- NULL
  colnames(dfaDiff)[2] <- "Baseline_meanDFA"
  dfaDiff_age <- melt(data.frame(dfaDiff), id = c("Subject", "age", "directionDFA", "diffDiff_ap"))
  
  
  age_dfaModel <- lm(value ~ variable * age, data =dfaDiff_age[dfaDiff_age$variable != "meanDFA",])
  gg_11 <- ggplot(dfaDiff_age[dfaDiff_age$variable != "Baseline_meanDFA" & !is.na(dfaDiff_age$age),], aes(x = age, y = value, group = Subject))+
    geom_point(aes(colour = variable),  position=position_dodge(width=0.4)) +
    geom_line(aes(linetype = directionDFA),  position=position_dodge(width=0.4))+
    scale_colour_manual(values=c( "deepskyblue2",  "orangered1")) +
    labs(y ="Difference DFA") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank()) 
  
  doc <- addPlot(doc = doc, fun = print, x = gg_11, vector.graphic = FALSE, width = 5, height = 3)
  
  gg_12 <- ggplot(dfaDiff_age[dfaDiff_age$variable != "Baseline_meanDFA" & !is.na(dfaDiff_age$age),], aes(x = age, y = value, group = variable, colour = variable))+
    geom_point() +
    geom_smooth(method = "lm")+
    scale_colour_manual(values=c( "deepskyblue2",  "orangered1")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank()) +
    labs(y ="Difference DFA")
  
  doc <- addPlot(doc = doc, fun = print, x = gg_12, vector.graphic = FALSE, width = 5, height = 3)
  
  gg_13 <- ggplot(dfaDiff_age[dfaDiff_age$variable == "Baseline_meanDFA" & !is.na(dfaDiff_age$age),], aes(x = age, y = value, group = directionDFA, colour = directionDFA))+
    geom_point() +
    geom_smooth(method = "lm")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())+
    labs(y="Baseline meanDFA")
  
  doc <- addPlot(doc = doc, fun = print, x = gg_13, vector.graphic = FALSE, width = 5, height = 3)
 
  #####
  directionModel2 <- lm(age ~  directionDFA *value * variable, data = dfaData[is.finite(dfaData$value),])
  anova(directionModel2)
  dfaData$age <- NIH_Alz_questionnaires[match(dfaData$Subject, NIH_Alz_questionnaires$Subject),]$Age

  dfa_age <- aggregate(value ~ Subject * age*directionDFA*variable, data = dfaData, FUN = mean)
  
  directionModel3 <- lm(age ~  value*directionDFA*variable , data = dfa_age[is.finite(dfa_age$value),])
  anova(directionModel3)
  gg_13.0 <- ggplot(dfaData[!is.na(dfaData$age),], aes(x = age, y = value, group = variable))+
    geom_point() +
    geom_smooth(method = "lm")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())+
    labs(y="DFA")+
    facet_grid(.~variable)
  
  doc <- addPlot(doc = doc, fun = print, x = gg_13.0, vector.graphic = FALSE, width = 7, height = 5)
  
  
  gg_13.2 <- ggplot(dfaData[!is.na(dfaData$age),], aes(x = value, y = age, group = directionDFA, colour = directionDFA))+
    geom_point() +
    geom_smooth(method = "lm")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())+
    labs(y="age", x="dfa")+
    facet_grid(.~variable)
  
  doc <- addPlot(doc = doc, fun = print, x = gg_14, vector.graphic = FALSE, width = 7, height = 5)
  
  
  gg_13.3 <- ggplot(dfaData[!is.na(dfaData$age),], aes(x = age, y = value, group = directionDFA, colour = directionDFA))+
    geom_point() +
    geom_smooth(method = "lm")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())+
    labs(y="DFA")+
    facet_grid(.~variable)
  
  doc <- addPlot(doc = doc, fun = print, x = gg_13.3, vector.graphic = FALSE, width = 7, height = 5)
  
  ######
  age2_dfaData <- aggregate(diffDiff_ap ~ Subject + age +directionDFA, FUN = mean, data =  dfaDiff_age)
  
  age2_dfaModel <- lm(age ~ diffDiff_ap, data =age2_dfaData[!is.na(age2_dfaData$age),])
  anova(age2_dfaModel)
  summary(age2_dfaModel)
  
  gg_14 <- ggplot(age2_dfaData[!is.na(age2_dfaData$age),], aes(x = age, y = diffDiff_ap))+
    geom_point(aes(colour = directionDFA)) +
    geom_smooth(method = "lm")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank()) 
  
  doc <- addPlot(doc = doc, fun = print, x = gg_14, vector.graphic = FALSE, width = 5, height = 3)
  
  gg_15 <- ggplot(age2_dfaData[!is.na(age2_dfaData$age),], aes(x = age, y = diffDiff_ap, group = directionDFA))+
    geom_point(aes(colour = directionDFA)) +
    geom_smooth(method = "lm")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank()) 
  
  doc <- addPlot(doc = doc, fun = print, x = gg_15, vector.graphic = FALSE, width = 5, height = 3)
  
  gg_16 <- ggplot(age2_dfaData[!is.na(age2_dfaData$age) & age2_dfaData$directionDFA == "less",], aes(x = age, y = diffDiff_ap))+
    geom_point(aes(colour = directionDFA)) +
    geom_smooth(method = "lm")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank()) 
  
  doc <- addPlot(doc = doc, fun = print, x = gg_16, vector.graphic = FALSE, width = 5, height = 3)
  
}
writeDoc( doc, file = "C:/Users/roohac/Desktop/ALZ_temp/summaryDocs/NIH-dfa-demographics-analysis.docx" )



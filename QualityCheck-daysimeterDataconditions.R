
sleepData <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/DATA/short-term/sleep/NIH ALZ Actiwatch Data_All subjects_June_2018_long.xlsx")

sleepData$condition <- tolower(sleepData$condition)
daysReff <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/daysimeterData/crossReference/subject_inventory_2018-02-22.xlsx")

daysReff <- daysReff[!is.na(daysReff$ids) & daysReff$group == "short_term",]
daysReff$condition <- paste(daysReff$session,daysReff$protocol)



##New var
sleepData$matchVar <- paste(sleepData$Subject, sleepData$condition)
daysReff$matchVar <- paste(daysReff$ids, daysReff$condition)


## add sleep data to daysreff:

daysReff$sleepDate <- sleepData[match(daysReff$matchVar, sleepData$matchVar),]$Date


daysReff$diffTime <- difftime(daysReff$startTime, daysReff$sleepDate, units = "days")
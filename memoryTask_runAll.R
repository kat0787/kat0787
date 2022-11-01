#NOTE: in order for this to work, you must first turn eprime files into tabDeliminated text files. 
# Then erase the first line in the text file so that it starts with headers.
# Next open the file in excel and save it as a csv file. Place these files within a folder called Memory_csvFiles within your R project
# it *should* work, but check that your paths and folders all make sense if you get errors.
# NOTE: if you try to simply change the .txt into .csv, R will reject you and turn all your friends against you (aka it won't work)


# Libraries 
#install.packages("tidyverse")
library(tidyverse)

#set working directory and path name - change if running from different folder/computer

dir_path <- "/Users/katelynoliver/Desktop/E2/R_thingz/"
setwd(dir_path)

# Make blank CSV file with column names
col_names = c("Subject", "Session", 
              "Hit_A", "Miss_A", "falseAlarm_A", "correctReject_A", 
              "Hit_T", "Miss_T", "falseAlarm_T", "correctReject_T", 
              "Total_A", "Total_T", "Total_all")
write.table(data.frame(matrix(nrow = 0, ncol = length(col_names))), 
            "MemoryPerformance_wholeGroup.csv", 
            sep = ",", 
            col.names = col_names,
            row.names = FALSE)

# Find all the csv files for the loop

fileNames <- Sys.glob("Memory_csvFiles/Memory-*.csv")

# scoring performance: hit = Old/Old, Miss = actual Old/pt "New", FA = actual New/ pt "Old", CR = New/New

for (i in 1:length(fileNames)) {
  # read & score data:
  memFile <- read.csv(paste0(dir_path,
                             fileNames[i]), header = T) %>%
    select(Subject, Session, Type, StimType, CS.RESP, Correct) %>%
    mutate(Score = case_when(
      Type == "Old" & StimType == "Animal" & CS.RESP < 3 ~ "Hit_A",
      Type == "Old" & StimType == "Animal" & CS.RESP > 2 ~ "Miss_A",
      Type == "New" & StimType == "Animal" & CS.RESP < 3 ~ "falseAlarm_A",
      Type == "New" & StimType == "Animal" & CS.RESP > 2 ~ "correctReject_A",
      Type == "Old" & StimType == "Tool" & CS.RESP < 3 ~ "Hit_T",
      Type == "Old" & StimType == "Tool" & CS.RESP > 2 ~ "Miss_T",
      Type == "New" & StimType == "Tool" & CS.RESP < 3 ~ "falseAlarm_T",
      Type == "New" & StimType == "Tool" & CS.RESP > 2 ~ "correctReject_T"))
  # Create New Dataframe
  memPerform <- data.frame(Subject = memFile[1,1], 
                           Session = memFile[1,2], 
                           Hit_A = sum(memFile$Score == "Hit_A"),
                           Miss_A = sum(memFile$Score == "Miss_A"),
                           falseAlarm_A =sum(memFile$Score == "falseAlarm_A"),
                           correctReject_A = sum(memFile$Score == "correctReject_A"),
                           Hit_T = sum(memFile$Score == "Hit_T"),
                           Miss_T = sum(memFile$Score == "Miss_T"),
                           falseAlarm_T =sum(memFile$Score == "falseAlarm_T"),
                           correctReject_T = sum(memFile$Score == "correctReject_T"),
                           Total_A = sum(memFile$Score == "Hit_A" | 
                                           memFile$Score == "Miss_A" | 
                                           memFile$Score == "falseAlarm_A" | 
                                           memFile$Score == "correctReject_A"),
                           Total_T = sum(memFile$Score == "Hit_T" | 
                                           memFile$Score == "Miss_T" | 
                                           memFile$Score == "falseAlarm_T" | 
                                           memFile$Score == "correctReject_T"),
                           Total_all = sum(memFile$Score == "Hit_A" | 
                                             memFile$Score == "Miss_A" | 
                                             memFile$Score == "falseAlarm_A" | 
                                             memFile$Score == "correctReject_A" |
                                             memFile$Score == "Hit_T" | 
                                             memFile$Score == "Miss_T" | 
                                             memFile$Score == "falseAlarm_T" | 
                                             memFile$Score == "correctReject_T"))
  # write new data to separate file:
  write.table(memPerform, "MemoryPerformance_wholeGroup.csv", 
              append = TRUE, 
              sep = ",", 
              row.names = FALSE, 
              col.names = FALSE)
}

# Re-order so that all session 1 runs come first and all session 2 runs come last
df_forOrdering <- read.csv("MemoryPerformance_wholeGroup.csv")
df_forOrdering<- df_forOrdering[order(df_forOrdering$Session),]
write.table(df_forOrdering, "MemoryPerformance_wholeGroup.csv", append=F, sep=",", row.names=F, col.names=T)





# this script was designed to pull button press data from individual subject csv files and output
# some summary data into 4 master csv files
# For this to work for others, you must change your directory path ('dir_path') 
# AND you must make a folder containing all of your csv files (I named mine: 'CStask_participantResponses')
# Note: the output master csv files and histograms will be stored in this folder

# Libraries 
#install.packages("tidyverse")
library(tidyverse)

#set working directory and path name - change if running from different folder/computer
dir_path <- "/Users/katelynoliver/Desktop/E2/R_thingz/CStask_participantResponses/"
setwd(dir_path)

#### Making Response Files for all Subjects ####
# Make Blank CSV Files
col_names2 = c("Subject", "Visit", "Run", "R-Yes", "R-No", "R-None", "percent_AnyResponse")
write.table(data.frame(matrix(nrow = 0, ncol = length(col_names2))), 
            "taskResponses_V1-run1.csv", sep = ",", col.names = col_names2, row.names = F)
write.table(data.frame(matrix(nrow = 0, ncol = length(col_names2))), 
            "taskResponses_V1-run2.csv", sep = ",", col.names = col_names2, row.names = F)
write.table(data.frame(matrix(nrow = 0, ncol = length(col_names2))), 
            "taskResponses_V2-run1.csv", sep = ",", col.names = col_names2, row.names = F)
write.table(data.frame(matrix(nrow = 0, ncol = length(col_names2))), 
            "taskResponses_V2-run2.csv", sep = ",", col.names = col_names2, row.names = F)

# Find response files
fileNames_V1_r1 <- Sys.glob("*V1_task-CS_run-1_events.csv")
fileNames_V1_r2 <- Sys.glob("*V1_task-CS_run-2_events.csv")
fileNames_V2_r1 <- Sys.glob("*V2_task-CS_run-1_events.csv")
fileNames_V2_r2 <- Sys.glob("*V2_task-CS_run-2_events.csv")

# Pull response data into 1 csv file

# Visit 1, Run 1

for (i in 1:length(fileNames_V1_r1)) {
  # read & score data:
  df <- read.csv(paste0(dir_path, fileNames_V1_r1[i]), header = T) %>%
    select('response.y.n.')
  # Create New Dataframe
  responses_df <- data.frame(Subject = unlist(strsplit(fileNames_V1_r1[i], split='_ses', fixed=T))[1],
                             Visit = 1,
                             Run = 1,
                             response_yes = sum(df$'response.y.n.' == "y"),
                             response_no = sum(df$'response.y.n.' == "n"),
                             response_none = sum(df$'response.y.n.' == "none"),
                             percent_response = round(sum((df$'response.y.n.' == 'y' | 
                                                      df$'response.y.n.' == 'n')/30*100), 2))
  
  # write new data to separate file:
  write.table(responses_df, "taskResponses_V1-run1.csv", 
              append = TRUE, 
              sep = ",", 
              row.names = FALSE, 
              col.names = FALSE)
}

# Visit 1, Run 2

for (i in 1:length(fileNames_V1_r2)) {
  # read & score data:
  df <- read.csv(paste0(dir_path, fileNames_V1_r2[i]), header = T) %>%
    select('response.y.n.')
  # Create New Dataframe
  responses_df <- data.frame(Subject = unlist(strsplit(fileNames_V1_r2[i], split='_ses', fixed=T))[1],
                             Visit = 1,
                             Run = 2,
                             response_yes = sum(df$'response.y.n.' == "y"),
                             response_no = sum(df$'response.y.n.' == "n"),
                             response_none = sum(df$'response.y.n.' == "none"),
                             percent_response = round(sum((df$'response.y.n.' == 'y' | 
                                                             df$'response.y.n.' == 'n')/30*100), 2)) 
  
  # write new data to separate file:
  write.table(responses_df, "taskResponses_V1-run2.csv", 
              append = TRUE, 
              sep = ",", 
              row.names = FALSE, 
              col.names = FALSE)
}

# Visit 2, Run 1 

for (i in 1:length(fileNames_V2_r1)) {
  # read & score data:
  df <- read.csv(paste0(dir_path, fileNames_V2_r1[i]), header = T) %>%
    select('response.y.n.')
  # Create New Dataframe
  responses_df <- data.frame(Subject = unlist(strsplit(fileNames_V2_r1[i], split='_ses', fixed=T))[1],
                             Visit = 2,
                             Run = 1,
                             response_yes = sum(df$'response.y.n.' == "y"),
                             response_no = sum(df$'response.y.n.' == "n"),
                             response_none = sum(df$'response.y.n.' == "none"),
                             percent_response = round(sum((df$'response.y.n.' == 'y' | 
                                                             df$'response.y.n.' == 'n')/30*100), 2)) 
  
  # write new data to separate file:
  write.table(responses_df, "taskResponses_V2-run1.csv", 
              append = TRUE, 
              sep = ",", 
              row.names = FALSE, 
              col.names = FALSE)
}

# Visit 2, Run 2 (Total: 28, missing 22093 & 22245)

for (i in 1:length(fileNames_V2_r2)) {
  # read & score data:
  df <- read.csv(paste0(dir_path, fileNames_V2_r2[i]), header = T) %>%
    select('response.y.n.')
  # Create New Dataframe
  responses_df <- data.frame(Subject = unlist(strsplit(fileNames_V2_r2[i], split='_ses', fixed=T))[1],
                             Visit = 2,
                             Run = 2,
                             response_yes = sum(df$'response.y.n.' == "y"),
                             response_no = sum(df$'response.y.n.' == "n"),
                             response_none = sum(df$'response.y.n.' == "none"),
                             percent_response = round(sum((df$'response.y.n.' == 'y' | 
                                                             df$'response.y.n.' == 'n')/30*100), 2)) 
  
  # write new data to separate file:
  write.table(responses_df, "taskResponses_V2-run2.csv", 
              append = TRUE, 
              sep = ",", 
              row.names = FALSE, 
              col.names = FALSE)
}

#### Playing with Distributions ####

allSub_V1_r1 <- read.csv(paste0(dir_path, "taskResponses_V1-run1.csv"), header = T)
allSub_V1_r2 <- read.csv(paste0(dir_path, "taskResponses_V1-run2.csv"), header = T)
allSub_V2_r1 <- read.csv(paste0(dir_path, "taskResponses_V2-run1.csv"), header = T)
allSub_V2_r2 <- read.csv(paste0(dir_path, "taskResponses_V2-run2.csv"), header = T)

h1_1 <- ggplot(data = allSub_V1_r1, aes(x = percent_AnyResponse)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.6, col = "blue", fill = "#B6B0FF")+
  labs(title="Responsiveness during fMRI CS task",
       subtitle="Visit 1, run 1: 'early' (N=41)",
       x="% of any button responses") 

h1_2 <- ggplot(data = allSub_V1_r2, aes(x = percent_AnyResponse)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.6, col = "blue", fill = "#B6B0FF")+
  labs(title="Responsiveness during fMRI CS task",
       subtitle="Visit 1, run 2: 'late' (N=40)",
       x="% of any button responses") 

h2_1 <- ggplot(data = allSub_V2_r1, aes(x = percent_AnyResponse)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.6, col = "blue", fill = "#B6B0FF")+
  labs(title="Responsiveness during fMRI CS task",
       subtitle="Visit 2, run 1: 'early' (N=30)",
       x="% of any button responses") 

h2_2 <- ggplot(data = allSub_V2_r2, aes(x = percent_AnyResponse)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.6, col = "blue", fill = "#B6B0FF")+
  labs(title="Responsiveness during fMRI CS task",
       subtitle="Visit 2, run 2: 'late' (N=28)",
       x="% of any button responses") 


#### end ####
#
#
#
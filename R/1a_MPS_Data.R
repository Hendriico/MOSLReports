MPS_Data<-function(dir=choose.dir()){

  # Setup --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  library(tidyverse)
  library(lubridate)



  # Importing raw data ----------------------------------------------------------------------------------

  mps_data <- read.csv(paste(dir, "\\data\\inputs\\MPS_data.csv", sep = ""))
  mps_thresholds <- read.csv(paste(dir, "\\data\\inputs\\mps_thresholds.csv", sep = ""))


  # Importing and cleaning MPS data --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  mps_data_clean <- mps_data %>%
    mutate(
      Date = as.Date(mps_data$Date, format = "%d/%m/%Y"),
      Charges = Total.Performance.Charge.Value,
      TaskCompletion = (Number.of.tasks.completed.on.time / Total.number.of.tasks.compeleted.within.Period),
      TaskVolume = Total.number.of.tasks.compeleted.within.Period,
      MPS = Market.Performance.Standard.No.
    ) %>%
    select(
      Date,
      Trading.Party.ID,
      MPS,
      TaskCompletion,
      TaskVolume,
      Charges
    ) %>%
    mutate(
      TaskCompletion = if_else(is.na(TaskCompletion), 0, TaskCompletion)
    )

  mps_data_clean <- left_join(mps_data_clean, mps_thresholds, by = c("MPS")) %>%
    mutate(
      MPS = factor(
        MPS,
        levels = c(
          "MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 5", "MPS 6", "MPS 7",
          "MPS 8", "MPS 9", "MPS 10", "MPS 12", "MPS 13", "MPS 14",
          "MPS 15", "MPS 16", "MPS 17", "MPS 18", "MPS 19"
        )))


  # Creating summary grouped by MPS with market metrics by month --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  mps_summary <- mps_data_clean %>%
    group_by(Date, MPS) %>%
    summarize(
      MPS_Mean = mean(TaskCompletion, na.rm = TRUE),
      MPS_Median = median(TaskCompletion, na.rm = TRUE),
      TotalTaskVolume = sum(TaskVolume)
    ) %>%
    arrange(MPS, Date) %>%
    ungroup()

  write.csv(mps_summary, paste(dir, "\\data\\outputs\\mps_summary.csv", sep = ""))
  saveRDS(mps_summary, file = paste(dir, "\\data\\rdata\\mps_summary.Rda", sep = ""))


  # Joining MPS summary with MPS data and checking if performance is below peer level or threshold --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  mps_data_clean <- left_join(mps_summary, mps_data_clean, by = c("Date", "MPS")) %>%
    mutate(
      TaskShare = TaskVolume / TotalTaskVolume,
      check1 = if_else (
        ((TaskCompletion < MPS_Mean) & (MPS_Mean <= MPS_Median)) |
          ((TaskCompletion < MPS_Median) & (MPS_Mean > MPS_Median)) |
          (TaskCompletion < mps_threshold),
        1, 0),
      key = as.factor(paste(Trading.Party.ID, MPS))
    )

  write.csv(mps_data_clean, paste(dir,"\\data\\outputs\\MPS_data_clean.csv",sep=""))
  saveRDS(mps_data_clean, file = paste(dir,"\\data\\rdata\\mps_data_clean.Rda",sep=""))

}

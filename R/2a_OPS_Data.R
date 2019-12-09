OPS_Data <- function(dir=choose.dir()){

  # Setup --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  library(tidyverse)



  # Importing raw data ----------------------------------------------------------------------------------

  ops_data <- read.csv(paste0(dir, "\\data\\inputs\\OPS_data.csv"))

  ops_thresholds <- read.csv(paste0(dir, "\\data\\inputs\\ops_thresholds.csv"))

  tp_details <- read.csv(paste0(dir, "\\data\\inputs\\tp_details.csv"))

  outstanding.tasks.threshold <- 0.75


  # Tidying and preparing data, joining thresholds to ops data ----------------------------------------------------------------------------------

  ops_data_clean <- ops_data %>%
    rename(
      "Trading.Party.ID" = Trading.Party.Name,
      "Date" = Period,
      "OPS" = Standard,
      "TaskVolume" = Tasks.Completed.Within.Period,
      "TotalOutstanding" = Tasks.Outstanding.End.Period
    ) %>%
    mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      TaskCompletion = Tasks.Completed.Within.Time / TaskVolume,
      OutstandingOntime = Tasks.Outstanding.Within.Time / TotalOutstanding
    ) %>%
    select(
      Date,
      Trading.Party.ID,
      OPS,
      TaskCompletion,
      TaskVolume,
      OutstandingOntime,
      TotalOutstanding,
      OutstandingOntime
    ) %>%
    filter(
      Date >= "2019-04-01"
    )

  ops_data_clean <- left_join(ops_data_clean, tp_details, by = c("Trading.Party.ID"))

  ops_data_clean <- left_join(ops_data_clean, ops_thresholds, by = c("OPS")) %>%
    mutate(
      OPS = factor(
        OPS,
        levels = c(
          "OPS B1a", "OPS B3a", "OPS B3b", "OPS B5a", "OPS C1a",
          "OPS C1b", "OPS C2a", "OPS C3a", "OPS C4a", "OPS C4b",
          "OPS C5a", "OPS C6a", "OPS F5a", "OPS F5b", "OPS G2a",
          "OPS G4a", "OPS G4b", "OPS H1a", "OPS I1a", "OPS I1b",
          "OPS I8a", "OPS I8b")))


  # Creating summary grouped by OPS with market metrics by month ----------------------------------------------------------------------------------

  ops_summary <- ops_data_clean %>%
    group_by(Date, OPS) %>%
    summarize(
      ops.mean.taskcompletion = mean(TaskCompletion, na.rm = TRUE),
      ops.mean.outstanding=mean(OutstandingOntime,na.rm = TRUE),
      OPS_Median = median(TaskCompletion, na.rm = TRUE),
      TotalTaskVolume = sum(TaskVolume)
    ) %>%
    ungroup() %>%
    arrange(OPS, Date)

  write.csv(ops_summary, paste0(dir, "\\data\\outputs\\ops_summary.csv"))
  saveRDS(ops_summary, file = paste0(dir,"\\data\\rdata\\ops_summary.Rda"))


  # Joining original data with summary to include market metrics and check if performance is below market mean ----------------------------------------------------------------------------------

  ops_data_clean <- left_join(ops_data_clean, ops_summary, by = c("Date", "OPS"))

  ops_data_clean <- ops_data_clean %>%
    mutate(
      TaskShare = TaskVolume / TotalTaskVolume,
      check1 = if_else(TaskCompletion < threshold, 1, 0),
      check2 = if_else(OutstandingOntime < outstanding.tasks.threshold, 1, 0),
      key = as.factor(paste(Trading.Party.ID, OPS))
    )

  write.csv(ops_data_clean, paste(dir, "\\data\\outputs\\OPS_data_clean.csv", sep = ""))
  saveRDS(ops_data_clean, file = paste(dir, "\\data\\rdata\\ops_data_clean.Rda", sep = ""))


}

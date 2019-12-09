MPS_Performance <- function(dir=choose.dir(), period=Sys.Date() %m-% months(1)){

  # Setup --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  library(tidyverse)
  library(lubridate)



  # Setting some parameters --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  day(period) <- 1

  period1 <- period
  period2 <- period1 %m-% months(1)
  period3 <- period2 %m-% months(1)

  period6 <- period %m-% months(5)

  # For IPRP comparison

  ds <- as.Date(c(period1, period2, period3))

  mps_list <- c("MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 7", "MPS 12", "MPS 16", "MPS 17", "MPS 18")


  # Importing raw data ----------------------------------------------------------------------------------

  mps_data_clean <- readRDS(paste(dir, "\\data\\rdata\\mps_data_clean.Rda", sep = ""))

  IPRP_plans <- read.csv(paste(dir, "\\data\\inputs\\IPRP_plans_mps.csv", sep = "")) %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

  watch_list <- read.csv(paste(dir, "\\data\\inputs\\tracking_mps.csv", sep = ""))


  # Produces a comparison of planned IPRP milestone versus actual performance and giving indication of which IPRPs are "On-Track", "Close-to-On-Track" (within 5% of milestone) and "Off-Track" (more than 5% off milestone) for each month --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  IPRP_plan_comparison <-
    left_join(
      IPRP_plans,
      mps_data_clean,
      by = c("Date", "MPS", "Trading.Party.ID")
    ) %>%
    mutate(
      MPS = as.factor(MPS),
      Trading.Party.ID = as.factor(Trading.Party.ID),
      Delta = TaskCompletion - Planned_Perf,
      DeltaQuant = Delta / Planned_Perf,
      check2 = if_else ((TaskCompletion < Planned_Perf), 1, 0)
    ) %>%
    select(
      Trading.Party.ID, MPS, Date, TaskVolume,
      TaskCompletion, Planned_Perf, Delta, DeltaQuant,
      MPS_Mean, MPS_Median, mps_threshold, Batch, check1,
      check2
    ) %>%
    mutate(
      OnTrack = if_else (Delta >= 0,1,0),
      Close = if_else (Delta < 0 & DeltaQuant > -0.05,1,0),
      OffTrack = if_else (Delta < 0 & DeltaQuant <= -0.05,1,0)
    )

  write.csv(IPRP_plan_comparison, paste(dir,"\\data\\outputs\\IPRP_plan_comparison.csv",sep=""))
  saveRDS(IPRP_plan_comparison, paste(dir,"\\data\\rdata\\IPRP_plan_comparison_mps.Rda",sep=""))


  # Creates IPRP table and IPRP list --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  IPRP_table <- IPRP_plan_comparison %>%
    filter(Date == period, OffTrack == 1) %>%
    mutate(Action = "tbd", Rationale = "tbd") %>%
    select(Trading.Party.ID, MPS, Batch, Action, Rationale)

  write.csv(IPRP_table, paste(dir,"\\data\\outputs\\flagged_IPRP_table_mps.csv", sep = ""))

  IPRP_list <- IPRP_plan_comparison %>%
    mutate(key = paste(Trading.Party.ID, MPS)) %>%
    select(key)


  # Flagging poor performance and saving as csv and Rdata files...Watch-list evaluated separately... Only certain MPS included. For retailers: MPS 1, 2, 15, 16, 17, 18; and for wholesalers: MPS 3, 4, 7, 12 --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  flagged_mps_table_3m <- mps_data_clean %>%
    filter(
      Date %in% ds,
      MPS %in% mps_list,
      !(key %in% IPRP_list$key)
    ) %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(
      sum_check1 = sum(check1)
    ) %>%
    ungroup() %>%
    filter(
      sum_check1 == 3
    ) %>%
    mutate(
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, MPS)),
      Date = period
    ) %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, MPS, key)

  write.csv(flagged_mps_table_3m, paste(dir, "\\data\\outputs\\flagged_mps_table_3m.csv", sep = ""))
  saveRDS(flagged_mps_table_3m, file = paste(dir, "\\data\\rdata\\flagged_mps_table_3m.Rda", sep = ""))


  flagged_mps_table_6m <- mps_data_clean %>%
    filter(
      Date >= period6,
      MPS %in% mps_list,
      !(key %in% IPRP_list$key),
      !(key %in% flagged_mps_table_3m$key)
    ) %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(
      sum_check1 = sum(check1)
    ) %>%
    ungroup() %>%
    filter(
      sum_check1 >= 3
    ) %>%
    mutate(
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, MPS)),
      Date = period
    ) %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, MPS, key)

  write.csv(flagged_mps_table_6m, paste(dir, "\\data\\outputs\\flagged_mps_table_6m.csv", sep = ""))
  saveRDS(flagged_mps_table_6m, file = paste(dir, "\\data\\rdata\\flagged_mps_table_6m.Rda", sep = ""))


  # Creating Watch-list --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  watch_list <- watch_list %>%
    rename("Trading.Party.ID" = ORG_ID) %>%
    mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, MPS))
    ) %>%
    filter(Date == period2 & (Action == "Watch" | Action == "De-escalate")) %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, MPS, Batch, Action, Rationale, key)

  write.csv(watch_list, paste(dir, "\\data\\outputs\\watch_list_mps.csv", sep = ""))
  saveRDS(watch_list, file = paste(dir, "\\data\\rdata\\watch_list_mps.Rda", sep = ""))


  # Combining flagged MPS, IPRP milestone flags and Watch-list into one tracking sheet --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  flagged_mps_table_3m <- flagged_mps_table_3m %>%
    mutate(
      Category = "Performance_Trigger_3m",
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      MPS = as.character(MPS),
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    droplevels() %>%
    select(Category, Date, Trading.Party.ID, MPS, Batch, key)

  flagged_mps_table_6m <- flagged_mps_table_6m %>%
    mutate(
      Category = "Performance_Trigger_6m",
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID, key),
      MPS = as.character(MPS),
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    droplevels() %>%
    select(Category, Date, Trading.Party.ID, MPS, Batch, key)

  watch_list <- watch_list %>%
    mutate(
      Date = period,
      Category = "Watch_list",
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      MPS = as.character(MPS),
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    select(Category, Date, Trading.Party.ID, MPS, Batch, key)

  IPRP_plan_comparison_period <- IPRP_plan_comparison %>%
    filter(Date == period, OffTrack == 1) %>%
    mutate(
      Category = "Milestone_Trigger",
      Trading.Party.ID = as.character(Trading.Party.ID),
      MPS = as.character(MPS),
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    select(Category, Date, Trading.Party.ID, MPS, Batch, key)

  iprp_end <- IPRP_plans %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(end.date = max(Date)) %>%
    ungroup() %>%
    filter(end.date == period) %>%
    mutate(
      Category = "IPRP_end",
      Batch = NA,
      Date = period,
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    select(Category, Date, Trading.Party.ID, MPS, Batch, key)


  monthly_tracking <-
    rbind(
      watch_list%>%filter(!key %in% c(flagged_mps_table_3m$key, flagged_mps_table_6m$key, IPRP_plan_comparison_period$key, iprp_end$key)),
      flagged_mps_table_3m,
      flagged_mps_table_6m,
      IPRP_plan_comparison_period,
      iprp_end
    ) %>%
    mutate_at(
      c("Category", "Trading.Party.ID", "MPS", "Batch"),
      factor
    )%>%
    select(Category, Date, Trading.Party.ID, MPS, Batch)

  write.csv(monthly_tracking, paste(dir,"\\data\\tracking\\mps\\", format(period, "%Y-%m"), "_monthly-tracking-mps.csv", sep = ""))
  saveRDS(monthly_tracking, file = paste(dir,"\\data\\rdata\\monthly-tracking-mps.Rda", sep = ""))


}

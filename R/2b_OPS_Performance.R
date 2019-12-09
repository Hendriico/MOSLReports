OPS_Performance <- function(dir=choose.dir(), period = Sys.Date() %m-% months(1)){

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
  ops_list <- c("OPS B5a", "OPS C1a")


  # Importing OPS data and OPS IPRP data --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  ops_data_clean <- readRDS(file = paste0(dir, "\\data\\rdata\\ops_data_clean.Rda"))

  IPRP_plans <- read.csv(paste0(dir, "\\data\\inputs\\IPRP_plans_ops.csv")) %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"))


  # Produces a comparison of planned IPRP milestone versus actual performance and giving indication of which IPRPs are "On-Track", "Close-to-On-Track" (within 5% of milestone) and "Off-Track" (more than 5% off milestone) for each month --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  IPRP_plan_comparison <- left_join(IPRP_plans, ops_data_clean, by = c("Date", "OPS", "Trading.Party.ID")) %>%
    mutate(
      OPS = as.factor(OPS),
      Trading.Party.ID = as.factor(Trading.Party.ID),
      Delta = TaskCompletion - Planned_Perf,
      DeltaQuant = Delta / Planned_Perf
    ) %>%
    select(
      Trading.Party.ID, OPS, Date, TaskVolume,
      TaskCompletion, Planned_Perf, Delta, DeltaQuant,
      ops.mean.taskcompletion, OPS_Median, threshold, Batch, check1,
      check2
    ) %>%
    mutate(
      OnTrack = if_else (Delta >= 0, 1, 0),
      Close = if_else (Delta < 0 & DeltaQuant > -0.05, 1, 0),
      OffTrack = if_else (Delta < 0 & DeltaQuant <= -0.05, 1, 0)
    )

  write.csv(IPRP_plan_comparison, paste(dir,"\\data\\outputs\\IPRP_plan_comparison_ops.csv", sep = ""))
  saveRDS(IPRP_plan_comparison, paste(dir, "\\data\\rdata\\IPRP_plan_comparison_ops.Rda", sep = ""))


  # Creates IPRP table and IPRP list --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  IPRP_table <- IPRP_plan_comparison %>%
    filter(Date == period, OffTrack == 1) %>%
    mutate(Action = "tbd", Rationale = "tbd") %>%
    select(Trading.Party.ID, OPS, Batch, Action, Rationale)

  write.csv(IPRP_table, paste(dir,"\\data\\outputs\\flagged_IPRP_table_ops.csv",sep=""))

  IPRP_list <- IPRP_plan_comparison %>%
    mutate(
      key = paste(Trading.Party.ID, OPS)
    ) %>%
    select(key)


  # Flagging poor performance and saving as csv and Rdata files...Watch-list evaluated separately... --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  flagged_ops_table_3m_KPI <- ops_data_clean %>%
    filter(
      Date %in% ds,
      OPS %in% ops_list,
      !(key %in% IPRP_list$key)
    ) %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(
      sum_check1 = sum(check1),
      sum_check2 = sum(check2)
    ) %>%
    ungroup() %>%
    filter(
      (sum_check1 == 3)
    ) %>%
    mutate(
      indicator = "KPI",
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, OPS, indicator)),
      Date = period
    ) %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, OPS, key, sum_check1, sum_check2, indicator)


  flagged_ops_table_3m_API <- ops_data_clean %>%
    filter(
      Date %in% ds,
      OPS %in% ops_list,
      !(key %in% IPRP_list$key)
    ) %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(
      sum_check1 = sum(check1),
      sum_check2 = sum(check2)
    ) %>%
    ungroup() %>%
    filter(
      (sum_check2 == 3)
    ) %>%
    mutate(
      indicator = "API",
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, OPS, indicator)),
      Date = period
    ) %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, OPS, key, sum_check1, sum_check2, indicator)

  flagged_ops_table_3m<-rbind(flagged_ops_table_3m_API, flagged_ops_table_3m_KPI)

  write.csv(flagged_ops_table_3m, paste(dir, "\\data\\outputs\\flagged_ops_table_3m.csv", sep = ""))
  saveRDS(flagged_ops_table_3m, file = paste(dir, "\\data\\rdata\\flagged_ops_table_3m.Rda", sep = ""))
  rm(flagged_ops_table_3m_API, flagged_ops_table_3m_KPI)

  flagged_ops_table_6m_KPI <- ops_data_clean %>%
    filter(
      Date >= period6,
      OPS %in% ops_list,
      !(key %in% IPRP_list$key),

    ) %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(
      sum_check1 = sum(check1),
      sum_check2 = sum(check2)
    ) %>%
    ungroup() %>%
    filter(
      sum_check1 >= 3
    ) %>%
    mutate(
      indicator = "KPI",
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, OPS, indicator)),
      Date = period
    ) %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, OPS, key, sum_check1, sum_check2, indicator)

  flagged_ops_table_6m_API <- ops_data_clean %>%
    filter(
      Date >= period6,
      OPS %in% ops_list,
      !(key %in% IPRP_list$key),
    ) %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(
      sum_check1 = sum(check1),
      sum_check2 = sum(check2)
    ) %>%
    ungroup() %>%
    filter(
      sum_check2 >= 3
    ) %>%
    mutate(
      indicator = "API",
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, OPS, indicator)),
      Date = period
    ) %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, OPS, key, sum_check1, sum_check2, indicator)

  flagged_ops_table_6m<-rbind(flagged_ops_table_6m_API, flagged_ops_table_6m_KPI)

  flagged_ops_table_6m<-flagged_ops_table_6m%>%
    filter(
      !(key %in% flagged_ops_table_3m$key)
    )

  write.csv(flagged_ops_table_6m, paste(dir, "\\data\\outputs\\flagged_ops_table_6m.csv", sep = ""))
  saveRDS(flagged_ops_table_6m, file = paste(dir, "\\data\\rdata\\flagged_ops_table_6m.Rda", sep = ""))
  rm(flagged_ops_table_6m_API, flagged_ops_table_6m_KPI)

  # Creating Watch-list --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  watch_list <- read.csv(paste(dir, "\\data\\inputs\\tracking_ops.csv", sep = "")) %>%
    rename("Trading.Party.ID" = ORG_ID) %>%
    mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, OPS))
    ) %>%
    filter(Date == period2, Action == "Watch") %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, OPS, Batch, Action, Rationale, key)

  write.csv(watch_list, paste(dir, "\\data\\outputs\\watch_list_ops.csv", sep = ""))
  saveRDS(watch_list, file = paste(dir, "\\data\\rdata\\watch_list_ops.Rda", sep = ""))


  # Combining flagged OPS, IPRP milestone flags and Watch-list into one tracking sheet --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  flagged_ops_table_3m <- flagged_ops_table_3m %>%
    mutate(
      Category =
        case_when(
          indicator == "KPI" ~
            "Performance_Trigger_3m_KPI",
          indicator == "API" ~
            "Performance_Trigger_3m_API",
          TRUE ~ "Other"
        ),
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      OPS = as.character(OPS)
    ) %>%
    droplevels() %>%
    select(Category, Date, Trading.Party.ID, OPS, Batch)

  flagged_ops_table_6m <- flagged_ops_table_6m %>%
    mutate(
      Category =
        case_when(
          indicator == "KPI" ~
            "Performance_Trigger_6m_KPI",
          indicator == "API" ~
            "Performance_Trigger_6m_API",
          TRUE ~ "Other"
        ),
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      OPS = as.character(OPS)
    ) %>%
    droplevels() %>%
    select(Category, Date, Trading.Party.ID, OPS, Batch)

  watch_list <- watch_list %>%
    mutate(
      Date = period,
      Category = "Watch_list",
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      OPS = as.character(OPS)
    ) %>%
    select(Category, Date, Trading.Party.ID, OPS, Batch)

  IPRP_plan_comparison <- IPRP_plan_comparison %>%
    filter(Date == period, OffTrack == 1) %>%
    mutate(
      Category = "Milestone_Trigger",
      Trading.Party.ID = as.character(Trading.Party.ID),
      OPS = as.character(OPS)
    ) %>%
    select(Category, Date, Trading.Party.ID, OPS, Batch)

  iprp_end <- IPRP_plans %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(end.date = max(Date)) %>%
    ungroup() %>%
    filter(end.date == period) %>%
    mutate(Category = "IPRP_end")

  monthly_tracking <-
    rbind(
      watch_list,
      flagged_ops_table_3m,
      flagged_ops_table_6m,
      IPRP_plan_comparison,
      iprp_end
    ) %>%
    mutate_at(
      c("Category", "Trading.Party.ID", "OPS", "Batch"),
      factor
    )

  write.csv(monthly_tracking, paste(dir,"\\data\\tracking\\ops\\", format(period, "%Y-%m"), "_monthly-tracking-ops.csv", sep = ""))
  saveRDS(monthly_tracking, file = paste(dir,"\\data\\rdata\\monthly-tracking-ops.Rda", sep = ""))


}

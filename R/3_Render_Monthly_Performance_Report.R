monthly_performance_report <- function(dir = choose.dir(), output_dir = paste0(dir,"\\Monthly\\"), period = Sys.Date() %m-% months(1)){


  # Loading library, clearing workspace and setting directory--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  library(tidyverse)
  library(lubridate)
  library(knitr)
  library(kableExtra)
  library(ggthemes)
  library(gridExtra)



  day(period) <- 1



  # Importing clean MPS Data and MPS Summary--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  mps_data <- readRDS(file = paste0(dir,"\\data\\rdata\\mps_data_clean.Rda"))

  mps_summary <- readRDS(file = paste0(dir,"\\data\\rdata\\mps_summary.Rda"))

  iprp_plan_comparison_mps <- readRDS(paste0(dir, "\\data\\rdata\\IPRP_plan_comparison_mps.Rda"))

  tracking_mps <- read.csv(paste0(dir, "\\data\\inputs\\tracking_mps.csv")) %>%
    rename("Trading.Party.ID" = ORG_ID) %>%
    mutate(
      Date = as.Date(Date, "%d/%m/%Y"),
      Rationale = as.character(Rationale),
      PFM_Commentary = as.character(PFM_Commentary),
      key = paste(Trading.Party.ID, MPS)
    )

  IPRP_plans_mps <- read.csv(paste0(dir,"\\data\\inputs\\IPRP_plans_mps.csv")) %>%
    mutate(
      Date = as.Date(Date,format = "%d/%m/%Y"),
      key  = as.factor(paste(Trading.Party.ID, MPS))
    )


  # Importing monthly tracking sheet for performance flags and milestones flags for tables--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  flagged_mps_table <- tracking_mps %>%
    filter(
      Category == "Performance_Trigger_3m" | Category == "Performance_Trigger_6m" | (Category == "Watch_list" & is.na(Batch)),
      Date == period
    ) %>%
    mutate(
      Watch_list =
        case_when(
          Category == "Watch_list" ~ "Yes",
          TRUE ~ "No"
        )
    ) %>%
    select(Trading.Party.ID, MPS, Action, Rationale, Watch_list)

  flagged_milestones_mps <- tracking_mps %>%
    filter(
      Category == "Milestone_Trigger" | (Category == "Watch_list" & !is.na(Batch)),
      Date == period
    ) %>%
    mutate(
      Watch_list =
        case_when(
          Category == "Watch_list" ~ "Yes",
          TRUE ~ "No"
        )
    ) %>%
    select(Trading.Party.ID, MPS, Batch, Action, Rationale, Watch_list)

  flagged_iprp_end_mps <- tracking_mps %>%
    filter(
      Category == "IPRP_end",
      Date == period
    ) %>%
    mutate(
      Watch_list =
        case_when(
          Category == "Watch_list" ~ "Yes",
          TRUE ~ "No"
        )
    ) %>%
    select(Trading.Party.ID, MPS, Batch, Action, Rationale)

  watch_mps <- tracking_mps %>%
    filter(
      Category == "Watch_list",
      Date == period,
      is.na(Batch)
    ) %>%
    select(Trading.Party.ID, MPS)

  watch_iprp_mps <- tracking_mps %>%
    filter(
      Category == "Watch_list",
      Date == period,
      !is.na(Batch)
    ) %>%
    select(Trading.Party.ID, MPS, Batch)


  # Combining flagged with mps_data and creating a key  (i.e. combine TP and MPS into unique string identifer) and then melting for graphs--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  flagged_mps_data <-
    left_join(
      flagged_mps_table,
      mps_data,
      by = c("Trading.Party.ID", "MPS")
    ) %>%
    mutate(key = paste(Trading.Party.ID, MPS)) %>%
    filter(
      Watch_list == "No",
      key %in% tracking_mps$key,
      Date >="2018-04-01"
    ) %>%
    select(
      Date,
      Trading.Party.ID,
      MPS,
      key,
      TaskCompletion,
      TaskVolume,
      TotalTaskVolume,
      MPS_Mean,
      MPS_Median,
      TaskShare
    )

  flagged_mps_data_melt <-
    gather(
      flagged_mps_data,
      key = "variable",
      value = "value",
      TaskCompletion, MPS_Mean, MPS_Median, TaskShare,
      factor_key = TRUE
    ) %>%
    mutate(
      category = str_sub(Trading.Party.ID,-1,-1),
      TaskVolume =
        if_else(
          variable %in% c("MPS_Mean", "MPS_Median", "TaskShare"), 0,
          as.double(TaskVolume)
        ),
      key = factor(key),
      MPS =
        factor(
          MPS, levels = c(
            "MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 5", "MPS 6", "MPS 7",
            "MPS 8", "MPS 9", "MPS 10", "MPS 12", "MPS 13", "MPS 14",
            "MPS 15", "MPS 16", "MPS 17", "MPS 18", "MPS 19")
        )
    )


  # Importing and preparing IPRP data for tables and graphs using melt--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  IPRP_tps_mps <- iprp_plan_comparison_mps %>%
    filter(Date == period) %>%
    mutate(key = as.factor(paste(Trading.Party.ID, MPS))) %>%
    select(Trading.Party.ID, MPS, key) %>%
    droplevels()

  IPRP_plans_data_mps <-
    left_join(
      mps_data,
      IPRP_plans_mps,
      by = c("Date", "Trading.Party.ID", "MPS", "key")
    ) %>%
    filter(
      Date >= "2018-04-01",
      key %in% IPRP_tps_mps$key
    ) %>%
    group_by(key) %>%
    mutate(Batch = max(Batch, na.rm = TRUE)) %>%
    ungroup() %>%
    select(
      Date, Trading.Party.ID, MPS, key, Batch, TaskCompletion,
      TaskVolume, TotalTaskVolume, MPS_Mean, MPS_Median, TaskShare, Planned_Perf)

  cols <- c("Trading.Party.ID", "MPS", "key")

  IPRP_plans_melt_mps <-
    gather(
      IPRP_plans_data_mps,
      key = "variable",
      value = "value",
      TaskCompletion, MPS_Mean, MPS_Median, TaskShare, Planned_Perf,
      factor_key = TRUE
    ) %>%
    mutate(
      category = str_sub(as.character(Trading.Party.ID),-1,-1),
      TaskVolume =
        if_else(
          variable %in% c("MPS_Mean", "MPS_Median", "Planned_Perf", "TaskShare"), 0,
          as.double(TaskVolume)
        )
    ) %>%
    mutate_at(cols, factor)


  # Preparing data for watch-list graphs using melt--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  watch_mps_data <-
    left_join(
      watch_mps,
      mps_data,
      by = c("Trading.Party.ID", "MPS")
    ) %>%
    filter(Date >="2018-04-01") %>%
    mutate(key = paste(Trading.Party.ID, MPS)) %>%
    select(
      Date,
      Trading.Party.ID,
      MPS,
      key,
      TaskCompletion,
      TaskVolume,
      TotalTaskVolume,
      MPS_Mean,
      MPS_Median,
      TaskShare
    )

  watch_mps_melt <-
    gather(
      watch_mps_data,
      key = "variable",
      value = "value",
      TaskCompletion, MPS_Mean, MPS_Median, TaskShare,
      factor_key = TRUE
    ) %>%
    mutate(
      category = str_sub(as.character(Trading.Party.ID),-1,-1),
      TaskVolume = if_else(
        variable %in% c("MPS_Mean", "MPS_Median", "TaskShare"), 0,
        as.double(TaskVolume)
      ),
      key = as.factor(as.character(key))
    )


  # Using melt to prepare data for aggregate MPS graphs--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  mps_summary_melt <-
    gather(
      mps_summary,
      key = "variable",
      value = "value",
      MPS_Mean, MPS_Median,
      factor_key = TRUE
    ) %>%
    filter(Date >= "2018-04-01") %>%
    mutate(
      TotalTaskVolume =
        if_else(
          variable == "MPS_Mean", 0,
          as.double(TotalTaskVolume)
        )
    ) %>%
    droplevels()


  # Importing IPRP plan comparison file, preparing IPRP analytics --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  iprp_status_mps <- iprp_plan_comparison_mps %>%
    filter(Date == period) %>%
    mutate(
      Status = case_when(
        OnTrack == 1 ~ "On Track",
        Close == 1 ~ "Close",
        OffTrack==1 ~ "Off Track"),
      TaskCompletion = as.numeric(format(TaskCompletion, digits = 1)),
      Planned_Perf = as.numeric(format(Planned_Perf, digits = 1))
    ) %>%
    select(Trading.Party.ID, MPS, Batch, TaskCompletion, Planned_Perf, Status) %>%
    arrange(Batch, Trading.Party.ID, MPS)


  # Splitting IPRP plan comparison between Wholesaler and Retailer --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  iprp_plan_comparison_w <- iprp_plan_comparison_mps %>%
    mutate(
      category = str_sub(iprp_plan_comparison_mps$Trading.Party.ID, -1, -1)
    ) %>%
    filter(category == "W", Date == period) %>%
    droplevels()

  w_iprps <- iprp_plan_comparison_w %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(
      count = n()
    ) %>%
    ungroup() %>%
    spread(
      key = MPS,
      value = count
    ) %>%
    mutate(
      IPRP_count = rowSums(select(., contains("MPS")), na.rm = TRUE)
    )

  # old version using addmargins: w_iprps <- addmargins(table(unique(iprp_plan_comparison_w[c("Trading.Party.ID", "MPS")])), 2)

  write.csv(w_iprps, paste(dir, "\\data\\outputs\\w_iprps.csv", sep = ""))


  iprp_plan_comparison_r <- iprp_plan_comparison_mps %>%
    mutate(
      category = str_sub(iprp_plan_comparison_mps$Trading.Party.ID,-1,-1)
    ) %>%
    filter(category == "R", Date == period) %>%
    droplevels()

  r_iprps <- iprp_plan_comparison_r %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(
      count = n()
    ) %>%
    ungroup() %>%
    spread(
      key = MPS,
      value = count
    ) %>%
    mutate(
      Total = rowSums(select(., -(Trading.Party.ID)), na.rm = TRUE),
    )
  r_iprps <- r_iprps %>%
    bind_rows(
      r_iprps %>%
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        mutate(Trading.Party.ID = "Total")
    )


  # old version using addmargins: r_iprps <- addmargins(table(unique(iprp_plan_comparison_r[c("Trading.Party.ID", "MPS")])), 2)

  write.csv(r_iprps,paste(dir,"\\data\\outputs\\r_iprps.csv", sep = ""))


  # Running render to produce Monthly Performance Report --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  rmarkdown::render(
    paste0(system.file("extdata", "MonthlyPerformanceReport.Rmd", package = "MOSLReports")),
    output_file = paste0(
      output_dir,
      "MonthlyPerformanceReport-",
      as.character(format(Sys.Date(), "%Y-%m")),
      ".pdf")
  )

}

Pfm_reports <- function(dir=choose.dir(),output=dir, render_list = c()){


  # Loading library, clearing workspace and setting directory--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  # Clear plots
  #if(!is.null(dev.list())) dev.off()

  # Clear console
  #cat("\014")

  # Clean workspace
  #rm(list = ls())




  library(tidyverse)
  library(lubridate)
  library(reshape2)
  library(scales)
  library(knitr)
  library(kableExtra)
  #library(pivottabler)
  library(ggthemes)


  # Importing clean MPS Data, MPS Summary and MPS details--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  mps_data <- readRDS(file = paste(dir,"\\data\\rdata\\mps_data_clean.Rda",sep="")) %>%
    filter(Date >= "2018-04-01")

  mps_summary <- readRDS(file = paste(dir,"\\data\\rdata\\mps_summary.Rda",sep="")) %>%
    filter(Date >= "2018-04-01") %>%
    mutate(
      Date = format(Date, "%Y-%m"),
      MPS_Mean = MPS_Mean * 100,
      MPS_Median = MPS_Median * 100)

  mps_details <- read.csv(file = paste(dir, "\\data\\inputs\\MPS_details.csv",sep=""))


  # Creating Charges tables and data for graphs --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  charges_graph_mps <- mps_data %>%
    select(Trading.Party.ID, Date, MPS, Charges)

  charges_table_mps <- charges_graph_mps %>%
    mutate(Date = format(Date, "%Y-%m"))


  # Melting MPS data for graphs --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  mps_data_melt <- mps_data %>%
    select(
      Date,
      Trading.Party.ID,
      MPS,
      key,
      TaskCompletion,
      TaskVolume,
      MPS_Mean,
      MPS_Median,
      TaskShare
    ) %>%
    melt(
      id.vars =
        c(
          "Date",
          "Trading.Party.ID",
          "MPS",
          "TaskVolume",
          "key"
        )
    ) %>%
    mutate(
      category = str_sub(as.character(Trading.Party.ID), -1, -1),
      TaskVolume =
        if_else (
          variable %in% c("MPS_Mean", "MPS_Median", "TaskShare"), 0, as.double(TaskVolume)
        ),
      key = as.factor(key))


  # Importing and preparing MPS IPRP data --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  IPRP_plans_mps <- read.csv(paste(dir,"\\data\\inputs\\IPRP_plans_mps.csv",sep="")) %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"), key = as.factor(paste(Trading.Party.ID, MPS)))

  cols <- c("Trading.Party.ID", "MPS", "Batch", "key")

  IPRP_plans_mps_data <- left_join(
    mps_data,
    IPRP_plans_mps,
    by = c(
      "Date",
      "Trading.Party.ID",
      "MPS",
      "key")
  ) %>%
    group_by(key) %>%
    mutate(Batch = max(Batch, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate_at(cols, factor) %>%
    select(Date, Trading.Party.ID, MPS, key, Batch, TaskCompletion, TaskVolume, MPS_Mean, MPS_Median, TaskShare, Planned_Perf)

  IPRP_plans_mps_melt <- melt(
    IPRP_plans_mps_data,
    id.vars =
      c("Date", "Trading.Party.ID", "MPS", "TaskVolume", "Batch", "key"
      )
  ) %>%
    mutate(
      category = str_sub(as.character(Trading.Party.ID), -1, -1),
      TaskVolume = if_else(
        variable %in% c("MPS_Mean", "MPS_Median", "Planned_Perf", "TaskShare"), 0,
        as.double(TaskVolume)))


  # Importing IPRP status sheet --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  iprp_status_mps <- readRDS(paste(dir,"\\data\\rdata\\IPRP_plan_comparison_mps.Rda", sep = "")) %>%
    mutate(
      Status = case_when(
        OnTrack == 1 ~ "On Track",
        Close == 1 ~ "Close",
        OffTrack==1 ~ "Off Track"),
      TaskCompletion = as.numeric(format(TaskCompletion, digits = 1)),
      Planned_Perf = as.numeric(format(Planned_Perf, digits = 1))
    ) %>%
    select(Trading.Party.ID, Date, MPS, Batch, TaskCompletion, Planned_Perf, Status)

  latest_period <- iprp_status_mps %>% filter(!is.na(Status)) %>% select(Date)
  latest_period <- max(latest_period$Date)


  # Importing performance tracking sheet --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  tracking_mps <- read.csv(paste(dir,"\\data\\inputs\\tracking_mps.csv",sep="")) %>%
    rename("Trading.Party.ID" = ORG_ID) %>%
    mutate(
      Date = format(as.Date(Date, format = "%d/%m/%Y"), "%Y-%m"),
      Rationale = as.character(Rationale),
      PFM_Commentary = as.character(PFM_Commentary)
    )

  tracking_mps_pfm_mpsperf <- tracking_mps %>%
    filter(
      Category == "Performance_Trigger_3m" |
        #Category == "Performance_Trigger_6m" |
        Category == "Watch_list",
      PFM_Commentary != "none",
      PFM_Commentary != "") %>%
    select(Trading.Party.ID, Date, MPS, PFM_Commentary)

  tracking_mps_pfm_mile <- tracking_mps %>%
    filter(Category == "Milestone_Trigger", PFM_Commentary != "none", PFM_Commentary != "") %>%
    select(Trading.Party.ID, Date, MPS, PFM_Commentary)

  tracking_mps_performance <- tracking_mps %>%
    filter(Category == "Performance_Trigger_3m" | Category == "Watch_list") %>%
    select(Trading.Party.ID, Date, MPS, Action, Rationale)

  tracking_mps_milestone <- tracking_mps %>%
    filter(Category == "Milestone_Trigger") %>%
    select(Trading.Party.ID, Date, MPS, Batch, Action, Rationale)

  tracking_mps_watch <- readRDS(paste(dir, "\\data\\rdata\\monthly-tracking-mps.Rda", sep = "")) %>%
    filter(Category == "Watch_list") %>%
    select(Trading.Party.ID, MPS)

  tracking_mps_requested <- tracking_mps %>%
    filter((Action == "Extend" | Action == "IPRP" | Action == "Resubmit") & Response_Received == "")

  # Importing OPS data --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  start <- Sys.Date() %m-% months(12)
  day(start) <- 1

  ops_data <- readRDS(file = paste(dir, "\\data\\rdata\\ops_data_clean.Rda", sep = "")) %>%
    filter(
      Date >= "2019-04-01",
      TaskVolume > 0
    ) %>%
    mutate(threshold.taskcompletion=ifelse(OPS %in% c("OPS B5a", "OPS C1a"),0.85, NA),
           threshold.outstanding=ifelse(OPS %in% c("OPS B5a", "OPS C1a"),0.75, NA))

  ops_summary <- readRDS(file = paste(dir, "\\data\\rdata\\ops_summary.Rda", sep = "")) %>%
    filter(Date >= start) %>%
    mutate(
      Date = format(Date, "%Y-%m"),
      ops.mean.taskcompletion = ops.mean.taskcompletion * 100,
      OPS_Median = OPS_Median * 100)


  # Melting OPS data for graphs --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  ops_data_melt <- ops_data %>%
    select(
      Date,
      Trading.Party.ID,
      OPS,
      key,
      TaskCompletion,
      ops.mean.taskcompletion,
      TaskVolume,
      OutstandingOntime,
      TotalOutstanding,
      ops.mean.outstanding,
      threshold.taskcompletion,
      threshold.outstanding
    ) %>%
    melt(
      id.vars =
        c(
          "Date",
          "Trading.Party.ID",
          "OPS",
          "TaskVolume",
          "TotalOutstanding",
          "key"
        )
    ) %>%
    mutate(
      TaskVolume =
        if_else (
          variable %in%
            c("OPS_Mean", "OPS_Median", "TaskShare"), 0, as.double(TaskVolume)
        ),
      key = as.factor(key))


  # Importing and preparing OPS IPRP data --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  IPRP_plans_ops <- read.csv(paste(dir,"\\data\\inputs\\IPRP_plans_ops.csv",sep="")) %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"), key = as.factor(paste(Trading.Party.ID, OPS)))

  cols <- c("Trading.Party.ID", "OPS", "Batch", "key")

  IPRP_plans_ops_data <- left_join(
    ops_data,
    IPRP_plans_ops,
    by = c(
      "Date",
      "Trading.Party.ID",
      "OPS",
      "key")
  ) %>%
    group_by(key) %>%
    mutate(Batch = max(Batch, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate_at(cols, factor) %>%
    select(Date, Trading.Party.ID, OPS, key, Batch, TaskCompletion, TaskVolume, OutstandingOntime, TotalOutstanding, ops.mean.taskcompletion, ops.mean.outstanding, threshold.taskcompletion, threshold.outstanding, Planned_Perf)

  IPRP_plans_ops_melt <- melt(
    IPRP_plans_ops_data,
    id.vars =
      c("Date", "Trading.Party.ID", "OPS", "TaskVolume", "Batch", "key"
      )
  ) %>%
    mutate(
      category = str_sub(as.character(Trading.Party.ID), -1, -1),
      TaskVolume = if_else(
        variable %in% c("OPS_Mean", "OPS_Median", "Planned_Perf", "TaskShare"), 0,
        as.double(TaskVolume)))


  # Importing OPS IPRP status sheet --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  iprp_status_ops <- readRDS(paste(dir,"\\data\\rdata\\IPRP_plan_comparison_ops.Rda", sep = "")) %>%
    mutate(
      Status = case_when(
        OnTrack == 1 ~ "On Track",
        Close == 1 ~ "Close",
        OffTrack==1 ~ "Off Track"),
      TaskCompletion = as.numeric(format(TaskCompletion, digits = 1)),
      Planned_Perf = as.numeric(format(Planned_Perf, digits = 1))
    ) %>%
    select(Trading.Party.ID, Date, OPS, Batch, TaskCompletion, Planned_Perf, Status)


  # Importing performance tracking sheet --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  tracking_ops <- read.csv(paste(dir,"\\data\\inputs\\tracking_ops.csv",sep="")) %>%
    rename("Trading.Party.ID" = ORG_ID) %>%
    mutate(
      Date = format(as.Date(Date, format = "%d/%m/%Y"), "%Y-%m"),
      Rationale = as.character(Rationale),
      PFM_Commentary = as.character(PFM_Commentary)
    )

  tracking_ops_pfm_opsperf <- tracking_ops %>%
    filter(
      Category == "Performance_Trigger_3m_KPI" |
        Category == "Performance_Trigger_3m_API" |
        Category == "Performance_Trigger_6m_KPI" |
        Category == "Performance_Trigger_6m_API" |
        Category == "Watch_list",
      PFM_Commentary != "none",
      PFM_Commentary != "") %>%
    select(Trading.Party.ID, Date, OPS, PFM_Commentary)

  tracking_ops_pfm_mile <- tracking_ops %>%
    filter(Category == "Milestone_Trigger",
           PFM_Commentary != "none",
           PFM_Commentary != "") %>%
    select(Trading.Party.ID, Date, OPS, PFM_Commentary)

  tracking_ops_performance <- tracking_ops %>%
    filter(Category == "Performance_Trigger_3m_KPI" |
             Category == "Performance_Trigger_3m_API" |
             Category == "Performance_Trigger_6m_KPI" |
             Category == "Performance_Trigger_6m_API" |
             Category == "Watch_list") %>%
    mutate(
      OPS = paste(OPS, substr(Category, nchar(as.character(Category))-2, nchar(as.character(Category))))
    )%>%
    select(Trading.Party.ID, Date, OPS, Action, Rationale)

  tracking_ops_milestone <- tracking_ops %>%
    filter(Category == "Milestone_Trigger") %>%
    select(Trading.Party.ID, Date, OPS, Batch, Action, Rationale)

  tracking_ops_watch <- readRDS(paste(dir, "\\data\\rdata\\monthly-tracking-ops.Rda", sep = ""))%>%
    filter(Category == "Watch_list") %>%
    select(Trading.Party.ID, OPS)

  tracking_ops_requested <- tracking_ops %>%
    filter((Action == "Extend" | Action == "IPRP" | Action == "Resubmit") & Response_Received == "")


  # Creating list of TPs for reports and running render to produce Monthly Performance Report --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  tp_details <- read.csv(paste(dir, "\\data\\inputs\\tp_details.csv", sep = ""))

  excluded <- c(
    "ICOSA2-R", "ICOSA2-W", "CHOLDERTON-R", "THAMESCOM-R",
    "SEVERNCON-W", "CCEP-R", "VEOLIA-W", "PEELWL-R",
    "ALBION-W", "AQUAFLOW-R", "SOUTHEAST-R"
  )

if(length(render_list)==0){
    render_list <- mps_data %>%
    filter(!Trading.Party.ID %in% excluded) %>%
    select(Trading.Party.ID) %>%
    droplevels() %>%
    mutate(Trading.Party.ID = as.character(Trading.Party.ID))
  render_list <- unique(render_list$Trading.Party.ID)
}
  else{
    render_list <- render_list
  }

print(render_list)
  #render_list <- c("THAMES-W")


  for (TRADING.PARTY in render_list) {
    TRADING.PARTY.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = T]
    SHORT.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = T]
    rmarkdown::render(
      paste0(system.file("extdata", "MonthlyPfmReport_word.Rmd", package = "MOSLReports")),
      output_file =
        paste(
          TRADING.PARTY,
          "_pfm-report_",
          format(Sys.Date(), "%Y-%m"),
          ".docx", sep = ""
        ),
      output_dir =
        paste(
          output,
          "\\",
          format(Sys.Date(), "%Y-%m"),
          sep=""
        )
    )
  }

}

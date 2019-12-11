graphs_OPS <- function(dir=choose.dir(),output_dir=dir){

  # Setup --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  library(reshape2)
  library(tidyverse)




  ops_data <- readRDS(paste(dir,"\\data\\rdata\\ops_data_clean.Rda",sep=""))

  #ops_data <- select(ops_data, Date, Trading.Party.ID, OPS,TaskCompletion,TaskVolume)


  # Using melt to prepare data for graphs --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  ops_data2 <- ops_data %>%
    filter(
      Date >="2018-04-01",
      TaskVolume > 0
    ) %>%
    mutate(
      TaskShare = TaskVolume/TotalTaskVolume,
      threshold.taskcompletion=ifelse(OPS %in% c("OPS B5a", "OPS C1a"),0.85, NA),
      threshold.outstanding=ifelse(OPS %in% c("OPS B5a", "OPS C1a"),0.75, NA)
    ) %>%
    select(Date, Trading.Party.ID, OPS, TaskVolume, TaskCompletion, TotalOutstanding, OutstandingOntime, ops.mean.taskcompletion, ops.mean.outstanding, threshold.taskcompletion, threshold.outstanding, OPS_Median, TaskShare)
  ops_data2 <- melt(ops_data2, id.vars = c("Date", "Trading.Party.ID", "OPS", "TaskVolume", "TotalOutstanding"))
  ops_data2 <- ops_data2 %>%
    mutate(
      TaskVolume = if_else(variable %in% c("OPS_Mean", "OPS_Median", "TaskShare"), 0, as.double(TaskVolume)),
      key = paste(Trading.Party.ID, "_", OPS, sep = "")
    )

  TP_list <- unique(ops_data2$key)


  labels.taskcompletion <- c("On-Time Tasks (RHS)", "Market Mean (RHS)","Threshold")
  labels.outstanding <- c("Outstanding On-Time Tasks (RHS)", "Market Mean (RHS)","Threshold")

  for (x in TP_list) {

    ops_data_graph <- ops_data2 %>%
      filter(key == x)
    if(unique(ops_data_graph$OPS) %in% c("OPS B5a", "OPS C1a")){

      data.taskcompletion <- ops_data_graph %>%
        filter((variable=="TaskCompletion"|variable=="ops.mean.taskcompletion"|variable=="threshold.taskcompletion"))

      data.outstanding<- ops_data_graph %>%
        filter((variable=="OutstandingOntime"|variable=="ops.mean.outstanding"|variable=="threshold.outstanding"))

      plot.taskcompletion <- ggplot(data.taskcompletion) +
        geom_bar(
          aes(
            x = data.taskcompletion$Date,
            y = data.taskcompletion$TaskVolume,
            fill="Task Volume (LHS)"
          ),
          stat = "identity",
          position = "dodge",
          inherit.aes = FALSE
        ) +
        geom_line(
          aes(x = data.taskcompletion$Date,
              y = data.taskcompletion$value * max(data.taskcompletion$TaskVolume),
              colour = data.taskcompletion$variable,
              linetype = data.taskcompletion$variable,
              size = data.taskcompletion$variable
          )
        ) +
        scale_y_continuous(
          breaks = pretty_breaks(4),
          sec.axis =
            sec_axis(~. / max(data.taskcompletion$TaskVolume), name = "Proportion")
        ) +
        scale_fill_manual(NULL, values = "azure3", na.value = "red") +
        scale_size_manual(values = c(1, 0.5, 0.6), na.value = "1", labels = labels.taskcompletion) +
        scale_linetype_manual(values = c(1, 2, 1), na.value = "1", labels = labels.taskcompletion) +
        scale_colour_manual(values = c("darkorange",  "azure4","red"),
                            na.value = "red", labels = labels.taskcompletion) +
        ylab("Volume of tasks") + xlab("Date") +
        theme(legend.title = element_blank(), legend.position = "right") +
        ggtitle(paste(data.taskcompletion$Trading.Party.ID, " (", data.taskcompletion$OPS, ") KPI", sep=""))

      g <- gridExtra::arrangeGrob(plot.taskcompletion)

      ggsave(file = paste(output_dir, "\\graphs\\OPS\\", x, "_KPI.png", sep = ""),g, width = 7, height = 5)

      plot.outstanding <- ggplot(data.outstanding) +
        geom_bar(
          aes(
            x = data.outstanding$Date,
            y = data.outstanding$TotalOutstanding,
            fill="Number of Outstanding Tasks (LHS)"
          ),
          stat = "identity",
          position = "dodge",
          inherit.aes = FALSE
        ) +
        geom_line(aes(
          x = data.outstanding$Date,
          y = data.outstanding$value * max(data.outstanding$TotalOutstanding),
          colour = data.outstanding$variable,
          linetype = data.outstanding$variable,
          size = data.outstanding$variable)
        ) +
        scale_y_continuous(
          breaks = pretty_breaks(4),
          sec.axis =
            sec_axis(~. / max(data.outstanding$TotalOutstanding), name = "Proportion")
        ) +
        scale_fill_manual(NULL, values = "azure3", na.value = "red") +
        scale_size_manual(values = c(1, 0.5, 0.6), na.value = "1", labels = labels.outstanding) +
        scale_linetype_manual(values = c(1, 2, 1), na.value = "1", labels = labels.outstanding) +
        scale_colour_manual(values = c("violet", "azure4", "red"),
                            na.value = "red", labels = labels.outstanding) +
        ylab("Volume of tasks") + xlab("Date") +
        theme(legend.title = element_blank(), legend.position = "right") +
        ggtitle(paste(data.outstanding$Trading.Party.ID, " (", data.outstanding$OPS, ") API", sep=""))
      g <- gridExtra::arrangeGrob(plot.outstanding)

      ggsave(file = paste(output_dir, "\\graphs\\OPS\\", x, "_API.png", sep = ""),g, width = 7, height = 5)


    }

    else{
      data.taskcompletion <- ops_data_graph %>%
        filter((variable=="TaskCompletion"|variable=="ops.mean.taskcompletion"))

      plot.taskcompletion <- ggplot(data.taskcompletion) +
        geom_bar(
          aes(
            x = data.taskcompletion$Date,
            y = data.taskcompletion$TaskVolume,
            fill="Task Volume (LHS)"
          ),
          stat = "identity",
          position = "dodge",
          inherit.aes = FALSE
        ) +
        geom_line(
          aes(x = data.taskcompletion$Date,
              y = data.taskcompletion$value * max(data.taskcompletion$TaskVolume),
              colour = data.taskcompletion$variable,
              linetype = data.taskcompletion$variable,
              size = data.taskcompletion$variable
          )
        ) +
        scale_y_continuous(
          breaks = pretty_breaks(4),
          sec.axis =
            sec_axis(~. / max(data.taskcompletion$TaskVolume), name = "Proportion")
        ) +
        scale_fill_manual(NULL, values = "azure3", na.value = "red") +
        scale_size_manual(values = c(1, 0.5, 0.2), na.value = "1", labels = labels.taskcompletion) +
        scale_linetype_manual(values = c(1, 2, 1), na.value = "1", labels = labels.taskcompletion) +
        scale_colour_manual(values = c("darkorange",  "azure4","red"),
                            na.value = "red", labels = labels.taskcompletion) +
        ylab("Volume of tasks") + xlab("Date") +
        theme(legend.title = element_blank(), legend.position = "right") +
        ggtitle(paste(data.taskcompletion$Trading.Party.ID, " (", data.taskcompletion$OPS, ") KPI", sep=""))

      g <- gridExtra::arrangeGrob(plot.taskcompletion)

      ggsave(file = paste(output_dir, "\\graphs\\OPS\\", x, "_KPI.png", sep = ""),g, width = 7, height = 5)
    }





  }
}

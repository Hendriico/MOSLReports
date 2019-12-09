graphs_OPS <- function(dir=choose.dir(),output_dir=dir){

  # Setup --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  library(reshape2)
  library(tidyverse)




  # Set directory



  #dir2 <- "C:\\Users\\AlexandraPiper\\OneDrive - Market Operator Services Limited\\Market Performance & Operations - Performance Monitoring\\Analytics\\graphs"



  # Importing OPS data --------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
    ) %>%
    select(Date, Trading.Party.ID, OPS, TaskVolume, TotalTaskVolume, TaskCompletion, OPS_Mean, OPS_Median, TaskShare)
  ops_data2 <- melt(ops_data2, id.vars = c("Date", "Trading.Party.ID", "OPS", "TaskVolume", "TotalTaskVolume"))
  ops_data2 <- ops_data2 %>%
    mutate(
      TaskVolume = if_else(variable %in% c("OPS_Mean", "OPS_Median", "TaskShare"), 0, as.double(TaskVolume)),
      key = paste(Trading.Party.ID, "_", OPS, sep = "")
    )

  TP_list <- unique(ops_data2$key)

  labels <- c("Task Completion (RHS)", "Mean (RHS)", "Median (RHS)", "Task Share (RHS)")

  for (x in TP_list) {

    ops_data_graph <- ops_data2 %>%
      filter(key == x)

    plot1 <- ggplot(ops_data_graph) +
      geom_bar(aes(x = Date, y= TaskVolume, fill = "Task Volume (LHS)"), stat = "identity", position = "dodge") +
      geom_line(aes(x = Date, y = value * max(ops_data_graph$TaskVolume), colour = variable, linetype = variable, size = variable)) +
      scale_y_continuous(sec.axis = sec_axis(~ . / max(ops_data_graph$TaskVolume), name = "Proportion")) +
      scale_fill_manual(values = "azure3") +
      scale_size_manual(values = c(1, 0.5, 0.5, 0.5), labels = labels) +
      scale_linetype_manual(values = c(1, 2, 1, 3), labels = labels) +
      scale_colour_manual(values = c("darkorange", "azure4", "dodgerblue4", "grey3"), labels = labels) +
      ylab("Volume of tasks") + xlab("Date") +
      theme(legend.title = element_blank(), legend.position = "right") +
      ggtitle(x)

    g <- gridExtra::arrangeGrob(plot1)

    ggsave(file = paste(output_dir, "\\graphs\\OPS\\", x, ".png", sep = ""),g, width = 7, height = 5)

  }

}

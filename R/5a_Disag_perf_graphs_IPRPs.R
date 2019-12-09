graphs_IPRPs <- function(dir=choose.dir()){

  library(reshape2)
  library(tidyverse)
  library(dplyr)
  library(ggplot2)


  #dir <- "C:\\Users\\AlexandraPiper\\OneDrive - Market Operator Services Limited\\graphs"

  ##Importing data with some basic data cleaning

  mps_data <- read.csv(paste(dir,"\\data\\inputs\\MPS_data.csv",sep=""))

  mps_data$Date <- as.Date(mps_data$Date, format = "%d/%m/%Y")
  mps_data$X.Tasks.Completed.On.Time <- as.numeric(as.character(mps_data$X.Tasks.Completed.On.Time))
  mps_data$TaskCompletion <- (mps_data$Number.of.tasks.completed.on.time / mps_data$Total.number.of.tasks.compeleted.within.Period)
  mps_data <- select(mps_data, Date, Trading.Party.ID, Market.Performance.Standard.No.,TaskCompletion,Total.number.of.tasks.compeleted.within.Period)
  mps_data$key <- as.factor(paste(mps_data$Trading.Party.ID,mps_data$Market.Performance.Standard.No.))


  ##Calculating MPS means and medians

  mps_summary <- mps_data %>%
    group_by(Date,Market.Performance.Standard.No.) %>%
    summarise(MPS_Mean = mean(TaskCompletion, na.rm=TRUE), MPS_Median = median(TaskCompletion, na.rm=TRUE),TaskVolume=sum(Total.number.of.tasks.compeleted.within.Period)) %>%
    arrange(Market.Performance.Standard.No.,Date)

  mps_data <- left_join(mps_data,mps_summary,by = c("Date","Market.Performance.Standard.No."))


  ##importing IPRP and preparing data: only TPs, and regarding relevant MPS, on IPRPs included in dataset for graphs

  IPRP_plans <- read.csv(paste(dir,"\\data\\inputs\\IPRP_plans_mps.csv",sep="")) %>%
    rename("Market.Performance.Standard.No." = MPS)
  IPRP_plans$Date <- as.Date(IPRP_plans$Date, format = "%d/%m/%Y")
  IPRP_plans$key <- as.factor(paste(IPRP_plans$Trading.Party.ID,IPRP_plans$Market.Performance.Standard.No.))

  IPRP_tps <- unique(IPRP_plans[c("Trading.Party.ID","Market.Performance.Standard.No.","key")])

  IPRP_plans2 <- left_join(mps_data,IPRP_plans, by = c("Date", "Trading.Party.ID","Market.Performance.Standard.No.","key"))

  IPRP_plans2 <- IPRP_plans2 %>%
    filter(key %in% IPRP_tps$key) %>%
    group_by(key) %>%
    mutate(Batch = max(Batch,na.rm=TRUE)) %>%
    ungroup()

  cols <- c("Trading.Party.ID","Market.Performance.Standard.No.","Batch","key")

  IPRP_plans2 <- IPRP_plans2 %>%
    mutate_at(cols,factor)


  ##using melt to prepare data for graphs

  IPRP_plans2 <- IPRP_plans2 %>%
    filter(Date >="2018-04-01") %>%
    mutate(TaskShare = Total.number.of.tasks.compeleted.within.Period/TaskVolume) %>%
    select(Date,Trading.Party.ID,Market.Performance.Standard.No.,key,Batch,TaskCompletion,Total.number.of.tasks.compeleted.within.Period,TaskVolume,MPS_Mean,MPS_Median,TaskShare,Planned_Perf)

  IPRP_plans2 <- melt(IPRP_plans2, id.vars = c("Date", "Trading.Party.ID","Market.Performance.Standard.No.","Total.number.of.tasks.compeleted.within.Period","TaskVolume","Batch","key")) %>%
    mutate(category = str_sub(as.character(Trading.Party.ID),-1,-1),Total.number.of.tasks.compeleted.within.Period = if_else(variable %in% c("MPS_Mean","MPS_Median","Planned_Perf","TaskShare"),0,as.double(Total.number.of.tasks.compeleted.within.Period)))


  ##producing the graphs as a single PDF document

  iprp_labels <- c("Task Completion (RHS)","Mean (RHS)","Median (RHS)","Task Share (RHS)","Planned Perf (RHS)")

  IPRP_list <- unique(IPRP_plans2$key)


  for (x in IPRP_tps$Trading.Party.ID) {

    mps_list <- IPRP_plans2 %>%
      filter(Trading.Party.ID==x)
    mps_list <- as.list((unique(as.character(mps_list$Market.Performance.Standard.No.))))

    for (y in mps_list) {

      data <- IPRP_plans2 %>%
        filter(Trading.Party.ID==x,Market.Performance.Standard.No.==y)

      plot <- ggplot(data) +
        geom_bar(aes(x = data$Date, y= data$Total.number.of.tasks.compeleted.within.Period,fill="Task Volume (LHS)"), stat="identity", position = "dodge",inherit.aes=FALSE) +
        geom_line(aes(x=data$Date, y=data$value*max(data$Total.number.of.tasks.compeleted.within.Period), colour=data$variable, linetype=data$variable, size=data$variable)) +
        geom_point(aes(x=data$Date, y=data$value*max(data$Total.number.of.tasks.compeleted.within.Period), shape=data$variable,alpha=data$variable)) +
        scale_y_continuous(sec.axis = sec_axis(~./max(data$Total.number.of.tasks.compeleted.within.Period),name="Proportion")) +
        scale_fill_manual(NULL,values="azure3",na.value="red") +
        scale_size_manual(values=c(1,0.5,0.5,0.5,1),na.value="1",labels = iprp_labels) +
        scale_linetype_manual(values = c(1,2,1,3,1),na.value="1",labels = iprp_labels) +
        scale_colour_manual(values = c("darkorange", "azure4", "dodgerblue4","grey3","red"),na.value="red",labels = iprp_labels) +
        scale_shape_manual(values = c(0,0,0,0,1),na.value=0,labels = iprp_labels) +
        scale_alpha_manual(values = c(0,0,0,0,1),na.value=0,labels = iprp_labels) +
        ylab("Volume of tasks") + xlab("Date") +
        theme(legend.title=element_blank(),legend.position="right") +
        ggtitle(paste("Batch",max(as.numeric(data$Batch),na.rm=TRUE),": ",data$Trading.Party.ID," (",data$Market.Performance.Standard,")"))

      mp <- gridExtra::arrangeGrob(plot)

      ggsave(file = paste(dir, "\\graphs\\IPRP\\",x,"_",y,".png",sep=""),mp,width=7,height=5)
      print(paste(x,y,"saved"))

    }
  }



}

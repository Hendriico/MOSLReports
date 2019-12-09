graphs_MPS <- function(dir=choose.dir()){

  library(reshape2)
  library(tidyverse)
  library(lubridate)
  library(dplyr)
  library(ggplot2)


  #dir2 <- "C:\\Users\\AlexandraPiper\\OneDrive - Market Operator Services Limited\\graphs"


  #Importing data with some basic data cleaning

  mps_data <- read.csv(paste(dir,"\\data\\inputs\\MPS_data.csv",sep=""))

  mps_data$Date <- as.Date(mps_data$Date, format = "%d/%m/%Y")
  mps_data$X.Tasks.Completed.On.Time <- as.numeric(as.character(mps_data$X.Tasks.Completed.On.Time))
  mps_data$TaskCompletion <- (mps_data$Number.of.tasks.completed.on.time / mps_data$Total.number.of.tasks.compeleted.within.Period)
  mps_data <- select(mps_data, Date, Trading.Party.ID, Market.Performance.Standard.No.,TaskCompletion,Total.number.of.tasks.compeleted.within.Period)

  #Calculating MPS means and medians

  mps_summary <- mps_data %>%
    group_by(Date,Market.Performance.Standard.No.) %>%
    summarise(MPS_Mean = mean(TaskCompletion, na.rm=TRUE), MPS_Median = median(TaskCompletion, na.rm=TRUE),TaskVolume=sum(Total.number.of.tasks.compeleted.within.Period)) %>%
    arrange(Market.Performance.Standard.No.,Date)

  mps_data <- left_join(mps_data,mps_summary,by = c("Date","Market.Performance.Standard.No."))


  ##using melt to prepare data for graphs

  mps_data2 <- mps_data %>%
    filter(Date >="2018-04-01") %>%
    mutate(TaskShare = Total.number.of.tasks.compeleted.within.Period/TaskVolume)
  mps_data2 <- melt(mps_data2, id.vars = c("Date", "Trading.Party.ID","Market.Performance.Standard.No.","Total.number.of.tasks.compeleted.within.Period","TaskVolume"))
  mps_data2 <- mps_data2 %>%
    mutate(category = str_sub(as.character(Trading.Party.ID),-1,-1))

  TP_list <- mps_data2 %>%
    filter(!Trading.Party.ID %in% c("ICOSA2-R","CHOLDERTON-R"))
  TP_list$Trading.Party.ID <- as.factor(as.character(TP_list$Trading.Party.ID))
  TP_list <-as.list(unique(as.character(TP_list$Trading.Party.ID)))

  #"MPS 4", "MPS 8", "MPS 9","MPS 3","MPS 7"

  labels <- c("Task Completion (RHS)","Mean (RHS)","Median (RHS)","Task Share (RHS)")

  for (x in TP_list) {

    mps_list <- mps_data2 %>%
      filter(Trading.Party.ID==x)
    mps_list$Market.Performance.Standard.No <- as.factor(as.character(mps_list$Market.Performance.Standard.No.))
    mps_list <- as.list(unique(as.character(mps_list$Market.Performance.Standard.No.)))

    for (y in mps_list) {

      data <- mps_data2 %>%
        filter(Trading.Party.ID==x, Market.Performance.Standard.No.==y) %>%
        arrange(Market.Performance.Standard.No.,Date) %>%
        mutate(Total.number.of.tasks.compeleted.within.Period = if_else(variable %in% c("MPS_Mean","MPS_Median"),0,as.double(Total.number.of.tasks.compeleted.within.Period)))

      plot1<-ggplot(data) +
        geom_bar(aes(x = data$Date, y= data$Total.number.of.tasks.compeleted.within.Period,fill="Task Volume (LHS)"), stat="identity", position = "dodge",inherit.aes=FALSE) +
        geom_line(aes(x=data$Date, y=data$value*max(data$Total.number.of.tasks.compeleted.within.Period), colour=data$variable, linetype=data$variable, size=data$variable)) +
        scale_y_continuous(sec.axis = sec_axis(~./max(data$Total.number.of.tasks.compeleted.within.Period),name="Proportion")) +
        scale_fill_manual(NULL,values="azure3",na.value="red") +
        scale_size_manual(values=c(1,0.5,0.5,0.5),na.value="1",labels = labels) +
        scale_linetype_manual(values = c(1,2,1,3),na.value="1",labels = labels) +
        scale_colour_manual(values = c("darkorange", "azure4", "dodgerblue4","grey3"),na.value="red",labels = labels) +
        ylab("Volume of tasks") + xlab("Date") +
        theme(legend.title=element_blank(),legend.position="right") +
        ggtitle(paste(x," - ",y))

      g <- gridExtra::arrangeGrob(plot1)

      ggsave(file=paste(dir,"\\graphs\\MPS\\",paste(x,"_",y),".png",sep=""),g, width = 7, height = 5)
      print(paste(x,y,"saved"))
    }
  }

}

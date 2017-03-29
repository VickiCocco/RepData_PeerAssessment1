plot1 <- 
        #Process/transform the data (if necessary) into a format suitable for your analysis 
        # prepare the data
        #Calculate the total number of steps taken per day
        tblsteps <- tblactivity %>%
        select(steps,
               date,
               interval) %>%
        filter(!is.na(steps))%>%
        mutate(intervaldate = as.Date(date, format = "%Y-%m-%d")) %>%
        arrange(intervaldate) %>%
        group_by(intervaldate) %>%
        summarise(daysteps = sum(steps))

        # plot to a pdf file
        #pdf("issues.pdf", width = 7, height = 9)
        
        # plot the histogram of steps each day
        plot1 <- ggplot(tblsteps, aes(daysteps))
        plot1 <- plot1 + geom_histogram(bins = 10, aes(fill = ..count..))
        plot1 <- plot1 + labs(x = "Number of Steps per Day" ,
                              y = "How Many Days at Each Count", 
                              title = "Distribution of Number of Steps per Day",
                              subtitle = "2012")
        plot1 <- plot1 + theme(plot.title=element_text(size=14, 
                                                       hjust=0.5, 
                                                       face="bold", 
                                                       colour="darkorchid4", 
                                                       vjust=-1)) 
 
        
        print(plot1)
        #dev.off() 

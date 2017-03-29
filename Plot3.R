plot3 <- 
        # prepare the data - calculate the mean for each day
        
        tblintervals <- tblactivity %>%
                select(steps,
                       interval,
                       date) %>%
                filter(steps != "NA") %>%
                arrange(interval) %>%
                group_by(interval) %>%
                summarise(stepsstat = mean(steps))

        # get the interval dates
        tbldates <- tblactivity %>%
                select(date) %>%
                mutate(intervaldate = as.Date(date, format = "%Y-%m-%d")) %>%
                arrange(intervaldate) %>%
                summarise(mindate = min(intervaldate), maxdate = max(intervaldate))

        
        mindate <- tbldates[1,1]
        maxdate <- tbldates[1,2]
        
        # plot to a pdf file
        #pdf("issues.pdf", width = 7, height = 9)
        # plot the histogram of steps each day
        plot3 <- ggplot(tblintervals, aes(x =interval, 
                                      y = stepsstat))
        plot3 <- plot3 + geom_line(size=1)
        plot3 <- plot3 + labs(x = "Interval Number" ,
                              y = "Average Steps per Interval", 
                              title = "Average Steps for Each Interval Across All Days",
                              subtitle = paste("Dates from ", 
                                                format(mindate, format="%Y-%m-%d"),
                                                " to ",
                                                format(maxdate, format="%Y-%m-%d")))
        plot3 <- plot3 + theme(plot.title=element_text(size=14, 
                                                       hjust=0.5, 
                                                       face="bold", 
                                                       colour="darkorchid4", 
                                                       vjust=-1))
        plot3 <- plot3 + theme(plot.subtitle=element_text(size=10, 
                                                          hjust=0.5, 
                                                          face="bold", 
                                                          colour="black", 
                                                          vjust=-1))
        plot3 <- plot3 + theme(panel.border = element_rect(colour = "black",
                                                           fill=NA, 
                                                           size=2))
        
        print(plot3)
        #dev.off() 

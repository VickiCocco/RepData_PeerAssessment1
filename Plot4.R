plot4 <- 


        tbldaysteps <- tblcomplete %>%
                select(steps,
                       interval,
                       date) %>%
                mutate(intervaldate = as.Date(date, format = "%Y-%m-%d"))

                tbldaysteps <- tbldaysteps %>%
                select(steps,
                       interval, 
                       intervaldate) %>%
                mutate(datetype =  ifelse(weekdays(intervaldate)=="Saturday" | weekdays(intervaldate)=="Sunday", "Weekend", "Weekday")) %>%
                arrange(datetype, interval) %>%
                group_by(datetype, interval) %>%
                summarise(meansteps = mean(steps))
                
                tbldates <- tblcomplete %>%
                        select(date) %>%
                        mutate(intervaldate = as.Date(date, format = "%Y-%m-%d")) %>%
                        arrange(intervaldate) %>%
                        summarise(mindate = min(intervaldate), maxdate = max(intervaldate))
                
                mindate <- tbldates[1,1]
                maxdate <- tbldates[1,2]
                
                cbPalette <- c("purple2", 
                               "blue4")
                
                #create the plot
                #create the plot
                plot5 <- ggplot(tbldaysteps, aes(x= interval, 
                                                 y= meansteps,
                                                 group = datetype, 
                                                 colour= datetype))
                plot5 <- plot5 + geom_line(size = 1) 
                plot5 <- plot5 + scale_colour_manual(values = cbPalette)
                plot5 <- plot5 + facet_wrap(~ datetype, ncol = 1, nrow = 2)
                plot5 <- plot5 + labs(x = "Interval Number" ,
                                      y = "Average Steps per Interval", 
                                      title = "Average Steps for Each Interval Across Each Day Type")
                                        subtitle = paste("Dates from ", 
                                         format(mindate, format="%Y-%m-%d"),
                                        " to ",
                                        format(maxdate, format="%Y-%m-%d"))
                plot5 <- plot5 + theme(plot.title=element_text(size=14, 
                                               hjust=0.5, 
                                               face="bold", 
                                               colour="darkorchid4", 
                                               vjust=-1))
                plot5 <- plot5 + theme(plot.subtitle=element_text(size=10, 
                                                  hjust=0.5, 
                                                  face="bold", 
                                                  colour="black", 
                                                  vjust=-1))
                plot5 <- plot5 + theme(panel.border = element_rect(colour = "black",
                                                   fill=NA, 
                                                   size=2))
                plot5 <- plot5 + theme(legend.position="none")
                print(plot5) 

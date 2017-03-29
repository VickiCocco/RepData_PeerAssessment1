plot2 <- 
        # prepare the data - calculate the mean for each day
        
        tblmeans <- tblactivity %>%
                select(steps,
                       date) %>%
                filter(!is.na(steps))
                mutate(intervaldate = as.Date(date, format = "%Y-%m-%d")) %>%
                arrange(intervaldate) %>%
                group_by(intervaldate) %>%
                summarise(stepsstat = mean(steps))

        tblmeans <- mutate(tblmeans, statname = 'mean')
        
        # prepare the data - calculate the median for each day
        tblmedians <- tblactivity %>%
                select(steps,
                       date) %>%
                filter(steps != "NA") %>%
                mutate(intervaldate = as.Date(date, format = "%Y-%m-%d")) %>%
                arrange(intervaldate) %>%
                group_by(intervaldate) %>%
                summarise(stepsstat = median(steps))

        tblmedians <- mutate(tblmedians, statname = 'median')
        
        # bind the two long tables and sorn
        tblstats <- rbind(tblmeans, tblmedians)
        tblstats <- arrange(tblstats, intervaldate, statname)

        # plot to a pdf file
        #pdf("issues.pdf", width = 7, height = 9)
        cbPalette <- c("purple2", 
                       "blue4")
        
        # plot the histogram of steps each day
        plot2 <- ggplot(tblstats, aes(x =intervaldate, 
                                      y = stepsstat, 
                                      group = statname,
                                      colour = statname))
        plot2 <- plot2 + geom_bar(stat = "identity",
                                  aes(fill = statname),
                                  position = "dodge")
        plot2 <- plot2 + scale_fill_manual(values = cbPalette)
        plot2 <- plot2 + scale_colour_manual(values = cbPalette)
        plot2 <- plot2 + guides(fill = guide_legend(title = "Statistic"))
        #labels = paste("long", c(5, 10, 15))
        plot2 <- plot2 + labs(x = "Date" ,
                              y = "Daily Means and Medians of Steps", 
                              title = "Means and Medians of Steps for Each Day",
                              subtitle = "2010")
        plot2 <- plot2 + theme(plot.title=element_text(size=14, 
                                                       hjust=0.5, 
                                                       face="bold", 
                                                       colour="darkorchid4", 
                                                       vjust=-1))
        plot2 <- plot2 + theme(plot.subtitle=element_text(size=10, 
                                                          hjust=0.5, 
                                                          face="bold", 
                                                          colour="black", 
                                                          vjust=-1)) 
        

        
        
        print(plot2)
        #dev.off() 

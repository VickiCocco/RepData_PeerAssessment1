plot2 <- 
        # prepare the data - calculate the mean for each day
        
        tblstats <- tblactivity %>%
                select(steps,
                       date) %>%
                filter(!is.na(steps)) %>%
                group_by(date) %>%
                summarise(totalsteps = sum(steps))


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
                              title = "Means and Medians of Steps per Day",
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

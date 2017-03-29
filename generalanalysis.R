generalanalysis <-
        
        # find the number of rows with na for steps 
        tblNAs <- tblactivity %>%
        select(steps,
               interval,
               date) %>%
        filter(is.na(steps))

        numNAs <- nrow(tblNAs)
        
        tblstats <- tblactivity %>%
                select(steps,
                       date) %>%
                filter(!is.na(steps)) %>%
                group_by(date) %>%
                summarise(totalsteps = sum(steps))
        
        tblstats <- tblstats %>%
                select(totalsteps) %>%
                summarise(meansteps = mean(totalsteps), mediansteps = median(totalsteps))
        
        
        
 
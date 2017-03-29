generalanalysis <-
        
        #Calculate and report the total number of missing values in the dataset 
        #(i.e. the total number of rows with NAs
        # find the number of rows with na for steps 
        tblNAs <- tblactivity %>%
        select(steps,
               interval,
               date) %>%
        filter(is.na(steps))

        numNAs <- nrow(tblNAs)
        

        #Calculate and report the mean and median of the total number of steps taken per day
        tblstats <- tblactivity %>%
                select(steps,
                       date) %>%
                filter(!is.na(steps)) %>%
                group_by(date) %>%
                summarise(totalsteps = sum(steps))
        
        tblstats <- tblstats %>%
                select(totalsteps) %>%
                summarise(meansteps = mean(totalsteps), mediansteps = median(totalsteps))
        
        #Devise a strategy for filling in all of the missing values in the dataset. 
        #The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
        #or the mean for that 5-minute interval, etc.
        
        tblintervals <- tblactivity %>%
                select(steps,
                       interval,
                       date) %>%
                filter(!is.na(steps)) %>%
                arrange(interval) %>%
                group_by(interval) %>%
                summarise(intervalmeansteps = mean(steps))
        
        
        
        
        
        
        
 
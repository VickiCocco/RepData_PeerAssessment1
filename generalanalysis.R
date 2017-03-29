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
        
        tblintervalmeans <- tblactivity %>%
                select(steps,
                       interval,
                       date) %>%
                filter(!is.na(steps)) %>%
                arrange(interval) %>%
                group_by(interval) %>%
                summarise(intervalmeansteps = mean(steps))
        
        tblintervals <- tblactivity %>%
                select(steps,
                       interval,
                       date) %>%
                filter(!is.na(steps))
        #Create a new dataset that is equal to the original dataset 
        #but with the missing data filled in. 
        
        tblimputedNAs <- left_join(tblNAs, tblintervalmeans, by = "interval")
        tblimputedNAs <- select(tblimputedNAs, intervalmeansteps, interval, date)
        colnames(tblimputedNAs) <- c('steps', 'interval', 'date')
        
        tblcomplete <- rbind(tblintervals, tblimputedNAs)
        
        #Make a histogram of the total number of steps taken each day and Calculate 
        #and report the mean and median total number of steps taken per day. 
        #Do these values differ from the estimates from the first part of the assignment? 
        #What is the impact of imputing missing data on the estimates of the total daily number of steps?
        
        # table with imputed NAs
        tblsteps <- tblcomplete %>%
                select(steps,
                       date,
                       interval) %>%
                filter(!is.na(steps))%>%
                mutate(intervaldate = as.Date(date, format = "%Y-%m-%d")) %>%
                arrange(intervaldate) %>%
                group_by(intervaldate) %>%
                summarise(daysteps = sum(steps))
        
        # plot the histogram of steps each day
        plot1 <- ggplot(tblsteps, aes(daysteps))
        plot1 <- plot1 + geom_histogram(bins = 10, aes(fill = ..count..))
        plot1 <- plot1 + labs(x = "Number of Steps per Day" ,
                              y = "How Many Days at Each Count", 
                              title = "Distribution of Number of Steps per Day with Imputed Interval Means",
                              subtitle = "2012")
        plot1 <- plot1 + theme(plot.title=element_text(size=14, 
                                                       hjust=0.5, 
                                                       face="bold", 
                                                       colour="darkorchid4", 
                                                       vjust=-1)) 
        
        
        print(plot1)
        
        #Do these values differ from the estimates from the first part of the assignment?
        #Calculate and report the mean and median of the total number of steps taken per day
        tblimputedstats <- tblcomplete %>%
                select(steps,
                       date) %>%
                filter(!is.na(steps)) %>%
                group_by(date) %>%
                summarise(totalsteps = sum(steps))
        
        tblimputedstats <- tblimputedstats %>%
                select(totalsteps) %>%
                summarise(meansteps = mean(totalsteps), mediansteps = median(totalsteps))
        
        
        imputedmeandiff <- tblstats$meansteps - tblimputedstats$meansteps
        imputedmediandiff <- tblstats$mediansteps - tblimputedstats$mediansteps
        
        
        
        
        
        
 
plot4 <- 


        tbldaysteps <- tblcomplete %>%
                select(steps,
                       interval,
                       date) %>%
                mutate(intervaldate = as.Date(date, format = "%Y-%m-%d"))

        tbldaysteps <- mutate(tbldaysteps, datetype = ifelse(weekdays(intervaldate)=="Saturday" | weekdays(intervaldate)=="Sunday", "weekend", "weekday"))

                tbldaysteps <- tbldaysteps %>%
                select(steps,
                       interval, 
                       intervaldate) %>%
                mutate(datetype =  ifelse(weekdays(intervaldate)=="Saturday" | weekdays(intervaldate)=="Sunday", "weekend", "weekday")) %>%
                arrange(datetype, interval) %>%
                group_by(datetype, interval) %>%
                summarise(meansteps = mean(steps))

                
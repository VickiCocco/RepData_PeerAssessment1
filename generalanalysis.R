generalanalysis <-
        
        # find the number of rows with na for steps 
        tblNAs <- tblactivity %>%
        select(steps) %>%
        filter(is.na(steps))

        numNAs <- nrow(tblNAs)
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
### first loading the libraries needed

```{r installlibraries, echo = TRUE
        install.packages('dplyr', repos="http://cran.rstudio.com/")
        library(dplyr)
        install.packages('dtplyr', repos="http://cran.rstudio.com/")
        library(dtplyr)
        install.packages('ggplot2', repos="http://cran.rstudio.com/")
        library(ggplot2)
        install.packages('knitr', repos="http://cran.rstudio.com/")
        library(knitr)
        }
```
```{r loaddata, echo = TRUE       
        workingdir <- getwd()
        
        destfile <- paste(workingdir, "/activity.csv", sep = "")
        if (!file.exists(destfile)) {
                print("got here")
                print(paste("activity.csv ", 
                            "does not exist in the working direct ",
                            "- please put it there"))
                
        } else {

                activity <- read.csv("activity.csv" , header = TRUE)
                tblactivity <- tbl_df(activity)
                rm(activity)
        }
        
        tblsteps <- tblactivity %>%
                select(steps,
                       date,
                       interval) %>%
                filter(!is.na(steps))%>%
                mutate(intervaldate = as.Date(date, format = "%Y-%m-%d")) %>%
                arrange(intervaldate) %>%
                group_by(intervaldate) %>%
                summarise(daysteps = sum(steps))

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
}

```




## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?

#load packages
library(tidyverse);library(lubridate)

data_raw=read_csv(  "some_file.csv"  )

data_clean<-data_raw %>%
  mutate(Amount=Amount/sum(Amount),
    #Combine date parts into single column
    Date=make_date(Year,   Month,Day))%>%
    filter(
  # other years aren't relevant to analysis
          year(Date)==2020,
          Amount> 0.1
    )

ggplot2(data_clean,aes(Date,Amount))+geom_line()

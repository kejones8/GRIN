#this script takes in RX logs from landis runs & confirms the #, size, and seasonality of fires are as expected
#allows these "stats" to be compared across fire uses & scenarios
library(dplyr)
library(ggplot2)

high<-read.csv("output_data\\highfire_1\\scrapple-events-log.csv")
inter<-read.csv("output_data\\interfire_1\\scrapple-events-log.csv")
low<-read.csv("output_data\\lowfire_2\\scrapple-events-log.csv")


#create fire use column for disitinguishing in plots
high$fireuse<-"high"
inter$fireuse<-"inter"
low$fireuse<-"low"
  
#put all together for easy plotting
all<-rbind(high,inter,low)
all$firetype<-trimws(all$IgnitionType)

#find Rx fires by simulation year by fireuse
annual_num_fires<-all %>% filter(firetype=='Rx')%>% group_by(fireuse,SimulationYear) %>% summarize(ann_count=n_distinct(EventID))

#now plot # fires by year for each fire use
year_count <- ggplot(annual_num_fires, aes(SimulationYear, ann_count, fill=fireuse))+
  geom_bar(stat = "identity", position = 'dodge')+   
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("SimYear", labels = as.character(annual_num_fires$SimulationYear), breaks = annual_num_fires$SimulationYear)+
  ggtitle("Annual Count of RX Fires")
year_count

#now plot by fire size
ann_firesize<-all %>% filter(firetype=='Rx') %>% group_by(fireuse,SimulationYear) %>% summarize(mean_sites_burn=mean(TotalSitesBurned),mean_acres_burn=((260*260*mean_sites_burn)/4047))

year_size <- ggplot(ann_firesize, aes(SimulationYear, mean_acres_burn, fill=fireuse))+
  geom_bar(stat = "identity", position = 'dodge')+   
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("SimYear", labels = as.character(ann_firesize$SimulationYear), breaks = ann_firesize$SimulationYear)+
  ggtitle("Mean Annual Acres Burned")
year_size

#now plot by day of year
hist_doy<-all %>% filter(firetype=='Rx') %>% group_by(fireuse) %>% count(InitialDayOfYear)

doy_count <- ggplot(hist_doy, aes(InitialDayOfYear, n, fill=fireuse))+
  geom_bar(stat = "identity", position = 'dodge')+   
  theme(axis.text.x = element_text(angle = 65, vjust = 0.25)) +
  scale_x_discrete(limits= c(seq(min(hist_doy$InitialDayOfYear), max(hist_doy$InitialDayOfYear), by = 10)))+
  ggtitle("DOY Histogram")
doy_count


#based on the plots above, wonder if RX not occurring later in the year is weather restricted? 
#these do not "match" the first (31) & last (334) days of fire in the climate-generator-baseline.txt file
#think I need to go to the climate log and figure out what temp/rel. humidity?
#BUT, not sure how to see if daily weather is limiting RX or not...
#might be time to open up the hood/have it spit out daily weather log?

library(tidyverse)
library(lubridate)

projections <- read_csv("daily_df.csv") # read in daily with all models
projections$daily_cfs <- projections$total*35.31467*(287.4887/86.4) # mm to cfs
# mm to cfs for WBC = 117.5066, cfs to mm = 0.00851016
# get rid of the Miroc gcm, mutate the model string.
projections <- projections %>% 
  filter(gcm != "MIROC-ESM-CHEM_85") %>%
  mutate(model = str_c(gcm, '_', rcp))

projections$Day <- yday(projections$date) # should use EGRET instead, but this is temporary
projections <- projections %>% 
  rename(Year = yr,
         cfs = daily_cfs)

cfs_historical <- projections %>% select(Day, cfs, Year, rcp)
cfs_historical <- cfs_historical %>% 
  group_by(Day, rcp) %>% 
  summarize(mean = mean(cfs),
            median = median(cfs)) 

cfs_historical %>% ggplot(aes(x=Day, color=rcp)) +
  #geom_line(aes(y=mean), lwd=1) +
  geom_smooth(aes(y=mean), se=F, span=0.1) +
  theme_bw() + 
  labs(x="Days Since January 1st", y="Discharge (cfs)") +
  theme(legend.title=element_blank()) +
  scale_color_brewer(c("black", "yellow", "red"))
  



cfs_historical <- pivot_wider(cfs_historical, names_from = Day, values_from = cfs)
quants <- c(0.25,0.50, 0.75)
Percentiles <- apply(cfs_historical[2:dim(cfs_historical)[2]], 2, quantile, probs = quants, na.rm = TRUE)
Mean <- apply(cfs_historical[2:dim(cfs_historical)[2]], 2, mean, na.rm = TRUE)
historical_stats <- t(rbind(Percentiles, Mean))
historical_stats <- as.data.frame(historical_stats)
historical_stats$Day <- c(1:366)

historical_stats %>% ggplot(aes(x=Day)) +
  geom_line(aes(y=`50%`, color="Median"), lwd=1) +
  geom_line(aes(y=Mean, color="Mean"), lwd=1) +
  theme_bw() + 
  labs(x="Days Since January 1st", y="Discharge (cfs)") +
  nps_theme2() +
  theme(legend.title=element_blank()) +
  scale_color_manual(values = c("Mean" = "orange", "Median" = "black"))
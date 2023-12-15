#### Read in libraries and NPS font #### --------------------------------------

library(tidyverse)
library(lubridate)
library(EGRET)

windowsFonts("Frutiger LT Std 55 Roman" = 
               windowsFont("Frutiger LT Std 55 Roman"))

fontsize <- 20
nps_font <- "Frutiger LT Std 55 Roman"
nps_theme2 <- function(base_size = fontsize, 
                       base_family=nps_font,
                       title = T) {
  theme_classic(base_size = base_size, base_family = nps_font) %+replace%
    theme(axis.text.x = element_text(family=nps_font, size = base_size * 0.8),
          complete = TRUE
    ) 
}


#### Read in Projections csv and historical measured #### -------------------

projections <- read_csv("daily_df.csv") # read in daily with all models
Daily <- readNWISDaily("09505200","00060", "", "2022-10-01")
test <- as_tibble(Daily) %>% 
  mutate(cfs = Q*35.314666212661) %>% 
  select()


#### Wrangle Projections #### -------------------------------------------------


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

cfs_historical <- projections %>% 
  mutate(time_period = case_when(Year <= 2022 ~ 'low',
                            points < 25 ~ 'med',
                            points < 35 ~ 'high'))

  group_by(Day, rcp) %>% 
  summarize(mean = mean(cfs),
            median = median(cfs)) 

  
  
#### Plotting Hydrographs ####  
  

cfs_historical %>% ggplot(aes(x=Day, color=rcp)) +
  geom_smooth(aes(y=mean), se=F, span=0.2) +
  theme_bw() + 
  labs(x="Days Since January 1st", y="Discharge (cfs)") +
  theme(legend.title=element_blank()) +
  scale_color_manual(values = c("orange","red",  "black")) +
  nps_theme2()
  


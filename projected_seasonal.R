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
projections <- projections %>% 
  mutate(daily_cfs = total*35.31467*(287.4887/86.4),
         mo = as.numeric(mo),
         waterYear = if_else(mo >= 10, yr + 1, yr),
         leap = leap_year(date),
         day = as.numeric(format(date, "%j")),
         day = if_else(leap == F & day >= 60, day + 1, day)) %>%
  select(date, day, daily_cfs, waterYear, yr, mo, gcm, rcp)
  
Daily <- readNWISDaily("09505200","00060", "", "2022-10-01")
Daily <- Daily %>% 
  mutate(daily_cfs = Q*35.314666212661,
         yr = as.numeric(format(Date, "%Y")),
         mo = as.numeric(format(Date, "%m")),
         gcm = "Historical",
         rcp = "Measured") %>% 
  rename(date = Date,
         day = Day) %>% 
  select(date, day, daily_cfs, waterYear, yr, mo, gcm, rcp) %>% 
  as_tibble()

daily_proj_meas <- bind_rows(Daily, projections)

# write.csv(daily_proj_meas, "daily_proj_meas.csv")

### FIX BELOW IN OTHER PLACES
daily_proj_meas <- daily_proj_meas %>% 
  filter(gcm != "MIROC-ESM-CHEM_85") 

daily_proj_meas <-  daily_proj_meas %>%
  filter(rcp != "Hist") %>% 
  mutate(time_period = case_when(yr <= 2022 ~ 'Historical',
                            yr > 2022 & yr <= 2050 ~ 'Early',
                            yr > 2050 & yr <= 2070 ~ 'Middle',
                            yr > 2070 ~ 'Late'))

rcp_day_summary <- daily_proj_meas %>% 
  group_by(day, rcp, time_period) %>% 
  summarize(mean = mean(daily_cfs),
            median = median(daily_cfs))

rcp_day_futures <- rcp_day_summary %>% 
  filter(rcp != "Measured")

rcp_day_futures$time_period <- factor(as.factor(rcp_day_futures$time_period),
                                      levels = c("Early", "Middle", "Late"))

rcp_day_hist <- rcp_day_summary %>% 
  filter(rcp == "Measured") %>% 
  select(!time_period)


#### Plotting Hydrographs ####  

ggplot(rcp_day_hist, mapping = aes(x = day, y=mean, color=rcp)) +
  geom_smooth(color = "black", se=F, span=0.2) +
  facet_wrap(~time_period, ncol = 1) +
  geom_smooth(rcp_day_futures, mapping = aes(x = day, y=mean, color=rcp), se=F, span=0.2) +
  theme_bw() + 
  labs(x="Days Since January 1st", y="Discharge (cfs)") +
  theme(legend.title=element_blank()) +
  scale_color_manual(values = c("orange","red")) +
  nps_theme2() 

#### Script for Summarizing the table of EGRET results ####
 
#### Load Packages ####
packages <-  c("tidyverse") #create a list of all required packages
lapply(packages, library, character.only = T)


#### Read in and Wrangle Projection Results Table ####
projection_results <- read_csv("v5_EGRETprojectionresults.csv") %>% 
  separate(model, into = c("model", "rcp"), sep = "_") %>%  # create an RCP column
  filter(model != "MIROC-ESM-CHEM")
# Let's move the peak flow stuff to a different data frame
projection_peak_flow <- projection_results %>% 
  filter(discharge_stat == "peakflow_TS" | discharge_stat == "peakflow_OLS")

projection_istats <- projection_results %>% filter(!grepl("peakflow", discharge_stat))

# Join the istats to the table
istat_table <- matrix(c("1", "Minimum 1-day", 
                        "2", "Minimum 7-day mean", 
                        "3", "Minimum 30-day mean",
                        "4", "Median", 
                        "5", "Mean",
                        "6", "Maximum 30-day mean", 
                        "7", "Maximum 7-day mean",
                        "8", "Maximum 1-day"), nrow = 8, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("discharge_stat", "Discharge Statistic Name")))
istat_table <- as.data.frame(istat_table)
projection_istats <- left_join(projection_istats, istat_table, by = "discharge_stat")


#### Visualize the Slopes ####
# Istats trend slopes

# change in cfs per year
projection_istats %>% 
  filter(model != "Historical") %>% 
  ggplot(aes(x=`Discharge Statistic Name`, y = slope, fill=rcp)) + 
  geom_boxplot(position = "dodge") +
  geom_hline(yintercept = 0) +
  labs(y = "Theil-Sen Trend Slope: cfs/year (2023-2100)") +
  coord_flip() +
  theme_bw()

# or % change per year
projection_istats %>% 
  filter(model != "Historical") %>% 
  ggplot(aes(x=`Discharge Statistic Name`, y = slopepct_mag, fill=rcp)) + 
  geom_boxplot(position = "dodge") +
  geom_hline(yintercept = 0) +
  labs(y = "Theil-Sen Trend Slope: percent change per year (2023-2100)") +
  coord_flip() +
  theme_bw()




#### If you want to summarize the slopes (avg, for example) ####
slope_mean <- projection_results %>% 
  filter(model != "Historical") %>% 
  group_by(discharge_stat, rcp, `Discharge Statistic Name`) %>% 
  summarise(slope_mean = mean(slope),
            slope_sd = sd(slope))

slopepct_mag_mean <- projection_results %>% 
  filter(model != "Historical") %>% 
  group_by(discharge_stat, rcp) %>% 
  summarise(slopepct_mag = mean(slopepct_mag),
            slopepct_mag_sd = sd(slopepct_mag))


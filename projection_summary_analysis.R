#### Script for Summarizing the table of EGRET results ####
 
#### Load Packages ####
packages <-  c("tidyverse") #create a list of all required packages
lapply(packages, library, character.only = T)


#### Read in Projection Results Table ####
projection_results <- read_csv("v5_EGRETprojectionresults.csv") %>% 
  separate(model, into = c("model", "rcp"), sep = "_") # create an RCP column

# Join the istats
istat_table <- matrix(c("1", "Annual minimum 1-day daily mean discharge", 
                        "2", "Annual minimum 7-day mean of the daily mean discharges", 
                        "3", "Annual minimum 30-day mean of the daily mean discharges",
                        "4", "Annual median of the daily mean discharges", 
                        "5", "Annual mean of the daily mean discharges",
                        "6", "Annual maximum 30-day mean of the daily mean discharges", 
                        "7", "Anuual maximum 7-day mean of the daily mean discharges",
                        "8", "Annual maximum 1-day daily mean discharges"), nrow = 8, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("discharge_stat", "Discharge Statistic Name")))
istat_table <- as.data.frame(istat_table)
projection_results <- left_join(projection_results, istat_table, by = "discharge_stat")


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



slope_median <- projection_results %>% 
  filter(model != "Historical") %>% 
  group_by(discharge_stat, rcp) %>% 
  summarise(slope_median = median(slope),
            slope_sd = sd(slope))

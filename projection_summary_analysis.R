#### Script for Summarizing the table of EGRET results ####
 
#### Load Packages ####
packages <-  c("tidyverse") #create a list of all required packages
lapply(packages, library, character.only = T)

windowsFonts("Frutiger LT Std 55 Roman" = windowsFont("Frutiger LT Std 55 Roman"))
fontsize=20
nps_font <- "Frutiger LT Std 55 Roman" ###NPS fonts
nps_theme2 <- function(base_size = fontsize, base_family=nps_font) {
  theme_bw(base_size = base_size, base_family = nps_font) %+replace%
    theme(axis.text.x = element_text(family=nps_font, size = base_size * 0.8),
          complete = TRUE
    )}

save_path <- file.path("projection_summary_analysis_output")
#### Read in and Wrangle Projection Results Table ####
projection_results <- read_csv("v8_EGRETprojectionresults.csv") %>% 
  separate(model, into = c("model", "rcp"), sep = "_")

# Move the peak flow stuff to a different data frame
projection_peak_flow <- projection_results %>% 
  filter(grepl("peakflow", discharge_stat) | 
           grepl("waterYear", discharge_stat))  %>% 
  mutate(change_over_record = slope*77) # find change over the record

projection_istats <- projection_results %>% 
  filter(!grepl("peakflow", discharge_stat) & !grepl("waterYear", discharge_stat)) %>% 
  mutate(cfs_change_over_record = slope*77) # find change over the record

# Join the istats and regressions to the table
istat_table <- matrix(c("1", "Minimum 1-day", 
                        "2", "Minimum 7-day mean", 
                        "3", "Minimum 30-day mean",
                        "4", "Median", 
                        "5", "Mean",
                        "6", "Maximum 30-day mean", 
                        "7", "Maximum 7-day mean",
                        "8", "Maximum 1-day"), nrow = 8, ncol = 2, byrow = TRUE, 
                      dimnames = list(NULL, 
                                      c("discharge_stat", "Discharge Statistic Name")))
istat_table <- as.data.frame(istat_table)
projection_istats <- left_join(projection_istats, istat_table, by = "discharge_stat")

regressions_table <- tribble(
  ~discharge_stat, ~`Discharge Statistic Name`,
  "peakflow_TS", "Theil-Sen Peak Flow Day ~ Water Year",
  "peakflow_OLS", "OLS Peak Flow Day ~ Water Year",
  "Monsoon , wateryear_day ~ waterYear", "Monsoon",
  "Monsoon , cfs ~ waterYear", "Monsoon",
  "Other , wateryear_day ~ waterYear", "Non-Monsoon",
  "Other , cfs ~ waterYear", "Non-Monsoon",
  
)
projection_peak_flow <- left_join(projection_peak_flow, regressions_table, 
                               by = "discharge_stat")

#### Visualize the Slopes ####
##### Istats trend slopes

# change in CFS PER YEAR
# projection_istats %>% 
#   filter(model != "Historical") %>% 
#   ggplot(aes(x=`Discharge Statistic Name`, y = slope, fill=rcp)) + 
#   geom_boxplot(position = "dodge") +
#   geom_hline(yintercept = 0) +
#   labs(x= "Discharge Statistic",
#        y = "Theil-Sen Trend Slope: cfs/year (2023-2100)") +
#   coord_flip() +
#   theme_bw() +
#   scale_fill_manual(values = c("orange", "red")) +
#   nps_theme2()

# or CHANGE OVER RECORD 
projection_istats %>% 
  filter(model != "Historical") %>% 
  ggplot(aes(x=`Discharge Statistic Name`, y = cfs_change_over_record, fill=rcp)) + 
  geom_boxplot(position = "dodge") +
  geom_hline(yintercept = 0) +
  labs(x= "Discharge Statistic",
       y = "Cfs change over record (2023-2100)") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = c("orange", "red")) +
  nps_theme2()


# # or % change per year
# projection_istats %>% 
#   filter(model != "Historical") %>% 
#   ggplot(aes(x=`Discharge Statistic Name`, y = slopepct_mag, fill=rcp)) + 
#   geom_boxplot(position = "dodge") +
#   geom_hline(yintercept = 0) +
#   labs(y = "Theil-Sen Trend Slope: percent change per year (2023-2100)") +
#   coord_flip() +
#   theme_bw()


###### Peak flow slopes-------------------------------------------

# Boxplot: Change in DAY of peak flow over record by season (days/yr*77)
projection_peak_flow %>% 
  filter(grepl("wateryear_day", discharge_stat) &
           model != "Historical") %>% 
  ggplot(aes(x= `Discharge Statistic Name`, y = change_over_record, fill=rcp)) + 
  geom_boxplot(position = "dodge") +
  geom_hline(yintercept = 0) +
  labs(x = "Season",
       y = "Change in day of peak flow over record (2023-2100)") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = c("orange", "red")) +
  nps_theme2()
ggsave("Figure 3-5 production adjrsqs_reclass2.pdf", 
       device = cairo_pdf, dpi=300, 
       width = 12, height = 9, units="in")

# Boxplot: Change in magnitude of peak flow over record by season (cfs/yr*77)
projection_peak_flow %>% 
  filter(grepl("cfs", discharge_stat) &
           model != "Historical") %>% 
  ggplot(aes(x= `Discharge Statistic Name`, y = change_over_record, fill=rcp)) + 
  geom_boxplot(position = "dodge") +
  geom_hline(yintercept = 0) +
  labs(x = "Season",
       y = "Change in magnitude of peak flow (cfs) over record (2023-2100)") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = c("orange", "red")) +
  nps_theme2()
ggsave("Figure 3-5 production adjrsqs_reclass2.pdf", 
       device = cairo_pdf, dpi=300, 
       width = 12, height = 9, units="in")






# libraries
library(tidyverse)
library(rmarkdown)


# Read in projections ---------------------------------------------------------
projections <- read_csv("daily_df.csv") # read in daily with all models
projections$daily_cfs <- projections$total*35.31467*(287.4887/86.4) # mm to cfs
  # get rid of the Miroc gcm, mutate the model string.
projections <- projections %>% 
  filter(gcm != "MIROC-ESM-CHEM_85") %>%
  mutate(model = str_c(gcm, '_', rcp))

models <- unique(projections$model) # unique models for EGRET to loop through


# Create directories for each model and put the daily csv into the directory
# for (i in models){
#   path <- file.path("EGRETdaily_future_runoff", i)
#   dir.create(path) # create a directory 
# }

# for (i in models){
#   model <- filter(projections, model == i) %>% select(date, daily_cfs)
#   path <- file.path("EGRETdaily_future_runoff", i, "daily_input.csv")
#   write.csv(model, file = path, row.names = F)
# }



# empty df to store model results ---------------------------------------------
projection_results <- data.frame()
projection_results <- data.frame(model = character(),
                                 discharge_stat = character(),
                                 slope = numeric(),
                                 slopepct_mag = numeric(),
                                 slopepct_mag_interp = character(),
                                 p_value = numeric(),
                  stringsAsFactors=FALSE)

# slopepct_magnitude 
  #  for discharge stats this is the percent change per year
  #  for the regression lines, this is the change in number of days over 100 years




# Running the EGRET R markdown script -----------------------------------------
working_path <- "EGRETdaily_future_runoff"

for (i in models) {
  rmarkdown::render(input = "longterm_flow.Rmd",
                    params = list(ID = i),
                    output_file="projected_flow",
                    output_dir = file.path(working_path, i))
}




# Some graphs here ------------------------------------------------------------

daily_cfs_85 <- projections %>% 
  filter(rcp == "85" | rcp == "Hist") %>% 
  ggplot() +
  geom_line(aes(x=date, y=daily_cfs, color = gcm)) + 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP8.5") +
  theme_bw()

daily_cfs_45 <- projections %>% 
  filter(rcp == "45" | rcp == "Hist") %>% 
  ggplot() +
  geom_line(aes(x=date, y=daily_cfs, color = gcm)) + 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP4.5") +
  theme_bw()

gridExtra::grid.arrange(daily_cfs_45, daily_cfs_85, ncol=2)

slow_85 <- projections %>% 
  filter(rcp == "85" | rcp == "Hist") %>% 
  ggplot() +
  geom_line(aes(x=date, y=slow, color = gcm)) +ylim(0.02,0.035) + 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP8.5") +
  theme_bw()

slow_45 <- projections %>% 
  filter(rcp == "45" | rcp == "Hist") %>% 
  ggplot() +
  geom_line(aes(x=date, y=slow, color = gcm)) +ylim(0.02,0.035)+ 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP4.5") +
  theme_bw()

gridExtra::grid.arrange(slow_45, slow_85, ncol=2)
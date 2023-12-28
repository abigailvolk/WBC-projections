# libraries
library(tidyverse)
library(rmarkdown)


# Read in projections ---------------------------------------------------------
projections <- read_csv("daily_df.csv") # read in daily with all models
projections$daily_cfs <- projections$total*35.31467*(287.4887/86.4) # mm to cfs
  # mm to cfs for WBC = 117.5066, cfs to mm = 0.00851016
  # get rid of the Miroc gcm, mutate the model string.
projections <- projections %>% 
  filter(gcm != "MIROC-ESM-CHEM_85") %>%
  mutate(model = str_c(gcm, '_', rcp))

models <- unique(projections$model) # unique models for EGRET to loop through


#Create directories for each model and put the daily csv into the directory

# for (i in models){
#   path <- file.path("EGRETdaily_future_runoff", i)
#   dir.create(path) # create a directory
# }
# 
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
  rmarkdown::render(input = "projected_flow.Rmd",
                    params = list(ID = i),
                    output_file="projected_flow",
                    output_dir = file.path(working_path, i))
}

write.csv(projection_results, "v8_projectionresults.csv", row.names = F)

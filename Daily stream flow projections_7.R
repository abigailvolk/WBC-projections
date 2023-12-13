#Daily stream flow projections
#1) calibrate flow model using R script "IHACRESFlow_fcn" function that is called by R script "drainage calibration_IHACRES"
#2) obtain hydrology "a" and "b" calibration coefficients for betst fit determined as Nash-Sutcliffe measured vs. modeled flow
#3) obtain future runoff projections from Tercek water balance grids
#4) convert future runoff to stream flow using calibration coefficients and stack into a data frame
#5) plot future runoff

#code by David Thoma National Park Service

library(xts)
library(lubridate)
library(dyn)#used for lagged regression
library(reshape2)
library(ggplot2)
library(tidyverse)
library(here)
library(hydroGOF)
library(dplyr)
library(astsa)
library(Hmisc)
library("stringr") 
library(openxlsx)
library(gridExtra)

windowsFonts("Frutiger LT Std 55 Roman" = windowsFont("Frutiger LT Std 55 Roman"))
# you can repeat the above for new font names using the font names you saw in the system_fonts() call above
fontsize=20
nps_font <- "Frutiger LT Std 55 Roman"
nps_theme2 <- function(base_size = fontsize, base_family=nps_font) {
  theme_bw(base_size = base_size, base_family = nps_font) %+replace%
    theme(axis.text.x = element_text(family=nps_font, size = base_size * 0.8),
          complete = TRUE
    )} #in each ggplot figure where you want nps fonts add this to your ggplot code " + nps_theme2()"


here()
#setwd(here("daily future runoff"))
getwd()

# #####NOTE#########  (do this only once depending on formatting of files you plan to analyze or skip as needed ###################
# #############################################################################################
# #rename the daily files so that they consist of only the CMIP5 projection 
# dfiles<-(list.files(here("daily future runoff"), pattern = ".csv"));dfiles
# #dfiles<-(list.files("C:\\David\\Water balance\\Park Projects\\NCPN wetland hydroperiods\\daymet", pattern=".csv"));dfiles
# base<-strsplit(dfiles, split="wetbeaverck_runoff_daily_");base
# #file.rename(list.files(pattern="wetbeaverck_runoff_daily_"), paste(base,".csv", sep = ""))
# file.rename(list.files(pattern ='wetbeaverck_runoff_daily_'),
#             str_replace(list.files(pattern='wetbeaverck_runoff_daily_'), pattern='wetbeaverck_runoff_daily_', ''))
# ############################################################################################

############################################################################################
#import the historical modeled runoff data and convert to flow with a/b coefficients from calibration
############################################################################################
setwd(here());getwd()
hist <- read.csv("9505200_watershed_avg_water_balance_historical.csv");head(hist);tail(hist)#historical runoff from water balance model wtih Gridmet input
#there is only one day of data in the last year, so delete the last row of this file.  Otherwise there will be a year with zero flow in the
hist_ro<-subset(hist, select=c("Date", "GCM","runoff.in"));head(hist_ro);tail(hist_ro)
plot(hist_ro$runoff.in, type="l")
#annual summary file
last<-nrow(hist_ro);last
#hist_ro<-hist_ro[c(1:(last-1)),];tail(hist_ro)


#make each data frame into an xts object so they can be joined with matching dates.  provide core numeric data and an associated date for each row
hist_date<-as.Date(hist_ro$Date);hist_date
#hist_date<-as.Date(hist_ro$Date, format =("%m/%d/%Y")) # original throws an error for me
hist_xts<-xts(hist_ro[,"runoff.in"]*25.4,hist_date)
colnames(hist_xts)[1]<-"modrunoff"
head(hist_xts)
str(hist_xts)
d<-nrow(hist_xts);d# this is the length of the file in days, or the number modeled runoff days

########bias adjustment of historical and future modeled runoff if annual flows are different from measured flows#####
#add to runoff if known base flow contribution from ground water.  in semiarid systems
#estimate ground water base flow contribution as the average or median annual daily minimum.  ie. avg minimum flow on daily basis across many years 
gw<- 0.125  #set to zero if no ground water contribution expected
#if precip at watershed scale is over estimated or underestimated correct with a bias multiplier
#this is obtained from the ratio of measured/modeled total flow over the period of record
bias <- 0.357 #set to 1 if no bias adjustment needed

adj_hist_xts<-(hist_xts + gw)*bias#add ground water then bias adjust up or down for mass balance of measured vs. modeled total flow 
head(adj_hist_xts)
#copy bf (best fit) coefficients from "drainage calibration_IHACRES JEG.02.01.R"
#qa       qb        sa          sb
#0.4335787 0.486576 0.9551752 0.006318712

#calibration close-to-best values:# sa = 0.95; qa = 0.97; qb = 0.03
parms <- as.numeric(c(0.92, 0.3, 0.6))#obtain these parameter values from 
#drainage calibration_IHACRES or Excel calibrated model using 2 flows, quick and slow  "3_MikesdataWet Beaver V3_RoS_dt_gridmet_v3.xlsm"
sa <- parms[1]
qa <- parms[2]
qb <- parms[3] 
# be sure to start with valid parms -> sb >= 0.001
sb = 1 * (1-sa)-((qb/(1-qa) * (1-sa))); sb 

# drainage is total.
#mat <- matrix(data = NA, nrow = days, ncol = 2)  # empty matrix for holding data. ToDo: initialize outside function
drainage <- 0        # initial condition
#drainage2 <- NULL
#provide initial conditions for modeling future flows based on long-term average of January flows
#obtained from the historical peirod that would be minimally affected by model spinup
#for Wet Beaver Ck, AZ this was determined as January daily sum (mm) from 1990-2021.
#these initial values can be found in the Excel worksheet "initial flow conditions" in the
#workbook called "5_MikesdataWet Beaver V3_RoS_dt_gridmet_v3.xlsm"
#this can dramatically speed up spin-up to yield reasonable low flow results in low flow streams
q3 <- 0.419406417             # initial condition for quick flow
s3 <- 0.115015411          # initial condition for slow flow

for (i in 2:d){      # starting on second day into time series after initial conditions established
  # if (!is.na(coredata(runoff_xts[i]))) {
  q1 <- as.numeric(coredata(adj_hist_xts[i])) * qb; q1
  q2 <- q3[i-1] * qa;  q2
  q3[i] <- q1 + q2     # store quick flow contribution for plotting
  
  s1 <- as.numeric(coredata(adj_hist_xts[i])) * sb
  s2 <- s3[i-1] * sa
  s3[i] <- s1 + s2     # store slow flow contribution for plotting
}         #  end for (i in 2:days)loop across waterbalance runoff values to calculate quick,slow and total daily drainage values

drainage <- q3 + s3    #  sum of daily quick and daily slow flow vectors = daily total flow vector

drainage_qst <- cbind(q3,s3,drainage)   #   quick, slow and total drainage. Drainage = Flow
colnames(drainage_qst)[] <- c("quick","slow","total")
head(drainage_qst)
plot(drainage_qst[,3], type="l")
str(drainage_qst)
drainage_xts<-xts(drainage_qst, order.by = hist_date)
head(drainage_xts)
str(drainage_xts)

hist_flow<-merge(adj_hist_xts,drainage_qst);head(hist_flow)
str(hist_flow)#this is still an xts object


############################################
#summarize to monthly and annual periodicity
############################################

#use xts functionality to summarize daily to monthly and annual periodicity
#unfortunately apply.monthly with xts cannot apply by columns, that only works for means so must loop across columns or convert to dataframe
#and use standard functions to split, combine, apply by yr_mo and yr
hist_flow_month<-NULL
for(i in 1:4){
hold<-apply.monthly(hist_flow[,i], sum);head(hold)
hist_flow_month<-cbind(hist_flow_month, hold)
}
head(hist_flow_month)

#use xts functionality to summarize daily to monthly and annual periodicity
#unfortuntatly apply.monthly with xts cannot apply by columns, that only works for means so must loop across columns
hist_flow_ann<-NULL
for(i in 1:4){
  hold<-apply.yearly(hist_flow[,i], sum);head(hold)
  hist_flow_ann<-cbind(hist_flow_ann, hold)
}
head(hist_flow_ann)

#test to summarize to monthly and annual via dataframe methods
#as a data frame
df_hist<-data.frame(date=index(hist_flow), coredata(hist_flow)[,]);head(df_hist)
df2<-df_hist
df2$model<-"Historical"
head(df2)#data frame of historical data with model name "historical" for merging with future models that have various names
df2$yr<-format(as.Date(df2$date),"%Y")#;head(df2)
df2$mo<-format(as.Date(df2$date),"%m")#;head(df2)
df2$yr_mo<-format(as.Date(df2$date),"%Y-%m")#;head(df2)
df2<-df2[,c(6,1,7,8,9,2,3,4,5)]# reorder columns to match order of column in projections
head(df2)

hist_monthly<-df2 %>%
  group_by(model, yr_mo) %>%
  dplyr::summarize(modrunoff = sum(modrunoff, na.rm = TRUE),quick = sum(quick, na.rm = TRUE),
                   slow = sum(slow, na.rm = TRUE),total = sum(total, na.rm = TRUE))
head(hist_monthly)

hist_annual<-df2 %>%
  group_by(model,yr) %>%
  dplyr::summarize(modrunoff = sum(modrunoff, na.rm = TRUE),quick = sum(quick, na.rm = TRUE),
                   slow = sum(slow, na.rm = TRUE),total = sum(total, na.rm = TRUE))
head(hist_annual)
plot(hist_annual$total, type = "l")

############################################################################################
#import the projected modeled runoff data and convert to flow with a/b coefficients from calibration
############################################################################################
# setwd(here("daily future runoff"));getwd()
# dfiles<-(list.files(here("daily future runoff"), pattern = ".csv"));dfiles
# fut_names<-unlist(strsplit(dfiles, "_all_years.csv"));fut_names#model names useful for legend key with plots
# 
# n<-length(dfiles);n# used for loop counter when converting each runoff projection into calibrated daily flow

#use a single projection file to pull dates that are common to all of the projections that can be used to build xts objects 

#fut_ro <- read.csv(dfiles[1]);head(fut_ro)#projected runoff from water balance model using CMIP5 climate projections

fut_ro1 <- read.csv("9505200_watershed_avg_water_balance_future.csv") #head(fut_ro)#projected runoff from water balance model using CMIP5 climate projections
gcms<-unique(fut_ro1$GCM);gcms
fut_ro_date<-subset(fut_ro1, GCM == gcms[1]);head(fut_ro_date);tail(fut_ro_date)
#fut_date<-as.Date(fut_ro$time);fut_date#format needed for working with xts
fut_date<-as.Date(fut_ro_date$Date);fut_date#format needed for working with xts
df_date<-as.data.frame(fut_date)#format needed for use with data frames
####################################################
###################################
#####################################
####################################
q3 <- coredata(hist_flow[d,2])
s3 <- coredata(hist_flow[d,3])
d<-length(fut_date);d
j = 1
futures<-NULL #empty object to hold data after modeled runoff converted to modeled stream flow
#for (j in 1:n){#for each future model of runoff
for (j in 1:length(gcms)){
fut_ro<-subset(fut_ro1, GCM == gcms[j]);head(fut_ro)
#print(dfiles[j])#track progress
print(gcms[j])
#fut_ro<-cbind(historic_runoff[,c(1,2)], 25.4*((historic_runoff[,c(3:9)])));  head(historic_runoff_mm)
#fut_ro <- read.csv(dfiles[j]);head(fut_ro)#historical runoff from water balance model wtih Gridmet input
#fut_xts<-xts(fut_ro[,"extracted.data"],fut_date)
fut_xts<-xts(fut_ro[,"runoff.in"]*25.4,fut_date)#converts in to mm if runoff values delivered in inches
colnames(fut_xts)[1]<-"modrunoff"
head(fut_xts)


adj_fut_xts<-(fut_xts + gw)*bias#add ground water then bias adjust up or down for mass balance of measured vs. modeled total flow 
head(adj_fut_xts)

#convert modeled runoff to modeled stream flow using a/b calibration coefficients
for (i in 2:d){      # starting on second day into time series after initial conditions established
# if (!is.na(coredata(runoff_xts[i]))) {
q1 <- as.numeric(coredata(adj_fut_xts[i])) * qb; q1
q2 <- q3[i-1] * qa;  q2
q3[i] <- q1 + q2     # store quick flow contribution for plotting
s1 <- as.numeric(coredata(adj_fut_xts[i])) * sb
s2 <- s3[i-1] * sa
s3[i] <- s1 + s2     # store slow flow contribution for plotting
}         #  end for (i in 2:days)loop across water balance runoff values to calculate quick,slow and total daily drainage values
drainage <- q3 + s3    #  sum of daily quick and daily slow flow vectors = daily total flow vector
#drainage_qst <- cbind(fut_names[j],df_date,fut_ro$extracted.data,q3,s3,drainage)   # j is the future model name,  quick, slow and total drainage. Drainage = Flow
#drainage_qst <- cbind(gcms[j],df_date,fut_ro$extracted.data,q3,s3,drainage)   # j is the future model name,  quick, slow and total drainage. Drainage = Flow
drainage_qst <- cbind(gcms[j],df_date,coredata(adj_fut_xts),q3,s3,drainage)   # j is the future model name,  quick, slow and total drainage. Drainage = Flow
colnames(drainage_qst)[] <- c("model","date","modrunoff","quick","slow","total")
head(drainage_qst)
futures<-rbind(futures, drainage_qst)#stack results from each model into a heap for plotting
}



#add the yr_mo and yr columns to futures
futures$yr<-format(as.Date(futures$date),"%Y")#;head(df2)
futures$mo<-format(as.Date(futures$date),"%m")#;head(df2)
futures$yr_mo<-format(as.Date(futures$date),"%Y-%m")#;head(df2)
futures<-futures[,c(1,2,7,8,9,3,4,5,6)]# reorder columns to match order of column in projections
head(futures)
#do some test plotting.  this can be exported to excel for checking runoff to flow
test<-subset(futures, model =="BNU-ESM.rcp45" & yr ==(2044))# | 2055));head(test); tail(test)
plot(test$date, test$total, type = "l")
#do some test plotting.  this can be exported to excel for checking runoff to flow
test<-subset(futures, model =="BNU-ESM.rcp45" & yr ==(2044))# | 2055));head(test); tail(test)
head(test); tail(test)
head(futures)
unique(futures$model)
head(futures)
# base<-strsplit(futures$model, split=".");base
# futures$model
# base<-strsplit(futures$model, split=".rcp");base
# unlist(base)
# as.data.frame(unlist(base))
# data.frame(base)
# base
# head(futures)
# test2<-cbind(gcm, rcp, futures[,2:9]);head(test2)
# gcm <- as.numeric(sapply(base, "[", 1));gcm
# base
base<-strsplit(futures$model, split=".rcp");base
# gcm <- as.numeric(sapply(base, "[", 1));gcm
# rcp <- as.numeric(sapply(base, "[", 2));rcp
# test2<-cbind(gcm, rcp, futures[,2:9]);head(test2)
unique(futures$model)
gcm <- sapply(base, "[", 1);gcm
rcp <- as.numeric(sapply(base, "[", 2));rcp
test2<-cbind(gcm, rcp, futures[,2:9]);head(test2)
# #do some test plotting.  this can be exported to excel for checking runoff to flow
# test<-subset(futures, model =="BNU-ESM_rcp45" & yr ==(2044))# | 2055));
# head(test); tail(test)
# #do some test plotting.  this can be exported to excel for checking runoff to flow
# test<-subset(futures, gcm =="BNU-ESM_rcp45" & yr ==(2044))# | 2055));
# head(test); tail(test)
#do some test plotting.  this can be exported to excel for checking runoff to flow
test<-subset(test2, gcm =="BNU-ESM" & rcp == 45 & yr ==(2044))# | 2055));
head(test); tail(test)
#do some test plotting.  this can be exported to excel for checking runoff to flow
test<-subset(test2, gcm =="BNU-ESM" & rcp == 45 & yr ==(2044))# | 2055));
head(test); tail(test)
plot(test$date, test$total, type = "l")
max(test$total)
head(df2);head(test2)
names(df2)[1]<-"gcm"
###############daily data frame####################
df2$rcp<-NA
df2<-df2[,c(1,10,2:9)]
head(df2)
compiled<-rbind(df2, test2)
head(compiled);tail(compiled);nrow(compiled)
# df2
# futures<-test2
# df2
head(df_hist)
head(df2)#data frame of historical data with model name "historical" for merging with future models that have various names
tail(df2)
###############daily data frame####################

#compiled<-rbind(df2, futures)
###############daily data frame####################
# df2$rcp[,2]<-NA
# head(df2)
# df2<-df2[,c(1,10,2:9)]
# head(df2)
# compiled<-rbind(df2, futures)
# head(df2)
# head(futures)
# head(futures)
# head(drainage_qst)
# head(test2)
# tail(test2)
# head(df2)
# head(test2)
# head(df2)
# names(df2)[,1]<-"gcm"
# names(df2)[1]<-"gcm"
# head(test2)
# compiled<-rbind(df2, test2)
# head(compiled);tail(compiled);nrow(compiled)
hist<-subset(compiled, gcm == "Historical");head(hist)
hist$rcp<-"Hist";head(hist)
fut<-subset(compiled, gcm!="Historical");head(fut)
#fut<-subset(compiled, gcm!="Historical");head(fut)
#str_sub(fut$gcm, - 2, - 1)
#fut<-subset(compiled, gcm!="Historical");head(fut)
#rcp<-str_sub(fut$gcm, - 2, - 1)
#fut$rcp<-rcp
head(fut)
compiled<-NULL#delete then rebuild compiled with rcp values
compiled<-rbind(hist,fut);head(compiled)
daily_df<-as.data.frame(compiled);head(daily_df)
#write.csv(daily_df, file = "daily_df.csv", row.names = F)
################annual data frame###################
compiled_annual<-compiled %>%
group_by(gcm, rcp, yr) %>%
dplyr::summarize(modrunoff = sum(modrunoff, na.rm = TRUE),quick = sum(quick, na.rm = TRUE),
slow = sum(slow, na.rm = TRUE),total = sum(total, na.rm = TRUE))
annual_df<-as.data.frame(compiled_annual);head(annual_df);str(annual_df)#note yr is character, need to make numeric
annual_df$yr<-as.numeric(annual_df$yr)
head(annual_df);str(annual_df)
###############monthly data frame####################
compiled_monthly<-compiled %>%
group_by(gcm,rcp, yr_mo) %>%
dplyr::summarize(modrunoff = sum(modrunoff, na.rm = TRUE),quick = sum(quick, na.rm = TRUE),
slow = sum(slow, na.rm = TRUE),total = sum(total, na.rm = TRUE))
head(compiled)
###############monthly data frame####################
compiled_monthly<-compiled %>%
group_by(gcm,rcp, yr_mo) %>%
dplyr::summarize(modrunoff = sum(modrunoff, na.rm = TRUE),quick = sum(quick, na.rm = TRUE),
slow = sum(slow, na.rm = TRUE),total = sum(total, na.rm = TRUE))
monthly_df<-as.data.frame(compiled_monthly);head(monthly_df)
#make yr_mo a yearmon time index using zoo, or as here force to a date by fixing last day of month = 28
monthly_df$date<-as.Date(paste(monthly_df$yr_mo,"-28",sep=""))
#monthly_df<-monthly_df[,c(1,7,2,3,4,5,6)]# reorder columns to match order of column in projections
head(monthly_df)
############plot raw time series daily, monthly, annual faceted by model######################
plot<-ggplot(data = daily_df) + geom_line(aes(x=date, y = total))+#scale_color_viridis(discrete=TRUE)+#scale_fill_gradientn(colours=c("black","gray"))+#, alpha=westus_3_hs
facet_wrap(~gcm)+ ylab("Stream flow (mm)") + xlab("Year")+
#geom_density(aes(x=value*10),fill="blue", alpha = 0.2)+
#scale_fill_manual(values=bp_colors)+
#scale_fill_brewer(type = "qual", palette = 1, direction = 1, aesthetics = "fill")+
ggtitle("Wet Beaver Creek daily flow")
#color=NAor lwd = 0.01
#nps_theme2()#;plot+
plot
############plot raw time series daily, monthly, annual faceted by model######################
plot<-ggplot(data = daily_df) + geom_line(aes(x=date, y = total, color= rcp))+#scale_color_viridis(discrete=TRUE)+#scale_fill_gradientn(colours=c("black","gray"))+#, alpha=westus_3_hs
facet_wrap(~gcm)+ ylab("Stream flow (mm)") + xlab("Year")+
#geom_density(aes(x=value*10),fill="blue", alpha = 0.2)+
#scale_fill_manual(values=bp_colors)+
#scale_fill_brewer(type = "qual", palette = 1, direction = 1, aesthetics = "fill")+
ggtitle("Wet Beaver Creek daily flow")
#color=NAor lwd = 0.01
#nps_theme2()#;plot+
plot
head(hist)
test2<-cbind(gcm, rcp, futures[,2:9]);head(test2); tail(test2)
test2<-cbind(gcm, rcp, futures[,2:9]);head(test2); tail(test2);nrow(test2)
#drop MIROC-ESM-CHEM because it is incomplete
test2<-subset(test2, gcm !="MIROC-ESM-CHEM");nrow(test2)
#do some test plotting.  this can be exported to excel for checking runoff to flow
test<-subset(test2, gcm =="BNU-ESM" & rcp == 45 & yr ==(2044))# | 2055));
head(test); tail(test)
plot(test$date, test$total, type = "l")
max(test$total)
head(df2)

# 
#Water Rights 
#National Park Service (2012) Wet Beaver Creek Instream Flow Assessment Report. Fort Collins, CO: National Park Service, p. 23.
#order is #Oct	Nov	Dec	Jan	Feb	Mar	Apr	May	Jun	Jul	Aug	Sep
#units in mm converted from monthly average cfs runoff from 111 sq mi watershed
monthly_right<-c(2.400717033,	2.38284569,	3.165780703,	3.446616088,	13.9821981,	10.72280561,	2.901965644,	2.347954021,	1.65948182,	1.767560892,	2.348805037,	2.242427998)
ann_right<-48.55947973#units in mm converted from ac-ft runoff from 111 sq mi watershed
# 
# plot<-ggplot(data = monthly_df) + geom_line(aes(x=date, y = total))+#scale_color_viridis(discrete=TRUE)+#scale_fill_gradientn(colours=c("black","gray"))+#, alpha=westus_3_hs
#   facet_wrap(~gcm)+ ylab("Stream flow (mm)") + xlab("Year")+
#   #geom_density(aes(x=value*10),fill="blue", alpha = 0.2)+
#   #scale_fill_manual(values=bp_colors)+
#   #scale_fill_brewer(type = "qual", palette = 1, direction = 1, aesthetics = "fill")+
#   ggtitle("Wet Beaver Creek monthly flow")
# #color=NAor lwd = 0.01
# #nps_theme2()#;plot+
# plot
#add period to file for grouping by period normals
annual_df$period<-ifelse(annual_df$yr<=2022,"historical",
                     ifelse (annual_df$yr>=2023 & annual_df$yr<=2050,"early",
                             ifelse (annual_df$yr>=2051 & annual_df$yr<=2070,"middle",
                                     ifelse (annual_df$yr>=2071 & annual_df$yr<2100, "late","NA"))))

plot<-ggplot(data = annual_df) + geom_line(aes(x=yr, y = total, color=rcp))+#scale_color_viridis(discrete=TRUE)+#scale_fill_gradientn(colours=c("black","gray"))+#, alpha=westus_3_hs
  facet_wrap(~gcm)+ ylab("Stream flow (mm)") + xlab("Year")+
  #geom_density(aes(x=value*10),fill="blue", alpha = 0.2)+
  #scale_fill_manual(values=bp_colors)+
  #scale_fill_brewer(type = "qual", palette = 1, direction = 1, aesthetics = "fill")+
  geom_hline(yintercept=ann_right)+
  #this is the annual water right 11317.8 ac-ft/yr converted to mm annual runoff from 111 sq mi watershed
  #see Wet Beaver V3_RoS_dt_gridmet_v3.xlsm tab called runoff volume
  ggtitle("Wet Beaver Creek annual flow")+
  #color=NAor lwd = 0.01
  nps_theme2()#;plot+
plot

head(annual_df)
plot<-ggplot(data = annual_df) + geom_line(aes(x=yr, y = slow, color=rcp))+#scale_color_viridis(discrete=TRUE)+#scale_fill_gradientn(colours=c("black","gray"))+#, alpha=westus_3_hs
  #facet_wrap(~gcm)+ ylab("Modeled base flow (mm)") + xlab("Year")+
  #this is the annual water right 11317.8 ac-ft/yr converted to mm annual runoff from 111 sq mi watershed
  #see Wet Beaver V3_RoS_dt_gridmet_v3.xlsm tab called runoff volume
  ggtitle("Wet Beaver Creek annual modeled base flow")+
#color=NAor lwd = 0.01
nps_theme2()
theme(axis.line = element_line(color='black'),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())#;plot+#;plot+
plot

head(annual_df)
p_levels<-c("historical","early","middle","late")
plot<-ggplot(data = annual_df) + geom_boxplot(aes(x=period, y = slow, color=rcp))+ scale_x_discrete(limits = p_levels)+
  #facet_wrap(~gcm)+ ylab("Modeled base flow (mm)") + xlab("Year")+
  #geom_density(aes(x=value*10),fill="blue", alpha = 0.2)+
  #scale_fill_manual(values=bp_colors)+
  #scale_fill_brewer(type = "qual", palette = 1, direction = 1, aesthetics = "fill")+
  #geom_hline(yintercept=ann_right)+
  #this is the annual water right 11317.8 ac-ft/yr converted to mm annual runoff from 111 sq mi watershed
  #see Wet Beaver V3_RoS_dt_gridmet_v3.xlsm tab called runoff volume
  ggtitle("Wet Beaver Creek annual modeled base flow")+
  #color=NAor lwd = 0.01
  xlab("")+ ylab("Modeled annual base flow (mm)")+
  nps_theme2()+
  theme(axis.line = element_line(color='black'),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())#;plot+
plot

head(annual_df)
p_levels<-c("historical","early","middle","late")
plot<-ggplot(data = annual_df) + geom_boxplot(aes(x=period, y = total, color=rcp))+ scale_x_discrete(limits = p_levels)+
  #facet_wrap(~gcm)+ ylab("Modeled base flow (mm)") + xlab("Year")+
  #geom_density(aes(x=value*10),fill="blue", alpha = 0.2)+
  #scale_fill_manual(values=bp_colors)+
  #scale_fill_brewer(type = "qual", palette = 1, direction = 1, aesthetics = "fill")+
  #geom_hline(yintercept=ann_right)+
  #this is the annual water right 11317.8 ac-ft/yr converted to mm annual runoff from 111 sq mi watershed
  #see Wet Beaver V3_RoS_dt_gridmet_v3.xlsm tab called runoff volume
  ggtitle("Wet Beaver Creek annual flow")+
  #color=NAor lwd = 0.01
  xlab("")+ ylab("Annual flow (mm)")+
  nps_theme2()+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())#;plot+
plot

####################################################################################
#count # years annual water right not met and # months water right not met
#need to use monthly df for both since water rights are established for the water year
#also need to determine how often water rights were not met in the historical measured gage record
######################################################################################
################annual water rights###################################################
#read in and prep the historical gage record
g <- read.csv("wet beaver gage.csv", skip = 31, header = FALSE, stringsAsFactors=FALSE, sep="");head(g)
str(g)
historic_flow<-g[,3:4]
names(historic_flow)[]<-c("Date","cfs");head(historic_flow)
str(historic_flow)
historic_flow[,2]<-as.numeric(historic_flow[,2])
str(historic_flow)
#split date into year and month then add water year to data frame
historic_flow$yr<-format(as.Date(historic_flow$Date),"%Y")#;head(historic_flow)
historic_flow$mo<-format(as.Date(historic_flow$Date),"%m")#;head(historic_flow)
historic_flow$yr_mo<-format(as.Date(historic_flow$Date),"%Y-%m")#;
head(historic_flow)
#flow_xts <- xts(historic_flow[,2],order.by = ymd(historic_flow$Date))#create an extensible time series; can't add yr, mo, yr_mo bc char not allowed in xts
# head(flow_xts)
# names(flow_xts)[1] <- c("cfs")
# head(flow_xts)
#convert daily cfs to daily mm stream flow
ws_area<- 111# watershed area in square miles
historic_flow$meas_mm<-flow_xts$cfs * 28316847*86400/(2590000000000*ws_area)
head(historic_flow)
###############monthly data frame####################
meas_monthly<-historic_flow %>%
  group_by(yr_mo) %>%
  dplyr::summarize(monthly_meas = sum(meas_mm, na.rm = TRUE));head(meas_monthly)
Split <- strsplit(meas_monthly$yr_mo,"-");Split
yr <- as.numeric(sapply(Split, "[", 1));yr
mo <- as.numeric(sapply(Split, "[", 2));mo
meas_monthly<-cbind(meas_monthly, yr, mo);head(meas_monthly)
meas_monthly$wy <- ifelse(meas_monthly$mo>9,meas_monthly$yr+1, meas_monthly$yr);head(meas_monthly)

#summarize by water year total flow
ann_sum_meas<-as.data.frame(meas_monthly %>%
                         group_by(wy) %>%
                         summarise(months=n(),sumflow = sum(monthly_meas, is.na=TRUE)) %>% #returns "1" if na's present
                         distinct(.keep_all = TRUE));head(ann_sum_meas)
a<-which(ann_sum_meas$sumflow == 1);a
ann_sum_meas[a,3]<-NA;head(ann_sum_meas)
b<-which(ann_sum_meas$sumflow<ann_right);b#years annual water right not met
meas_ann_fail_pct<-length(b)/(nrow(ann_sum_meas)-length(a))*100;meas_ann_fail_pct


##########
head(monthly_df)
test<-monthly_df
#split year_mo into year and month then add water year to data frame
Split <- strsplit(test$yr_mo,"-");Split
yr <- as.numeric(sapply(Split, "[", 1));yr
mo <- as.numeric(sapply(Split, "[", 2));mo
test2<-cbind(test, yr, mo);head(test2)
test2$wy <- ifelse(test2$mo>9,test2$yr+1, test2$yr);test2[1:12,]

#add period to file for grouping by period normals
test2$period<-ifelse(test2$yr<=2022,"historical",
                     ifelse (test2$yr>=2023 & test2$yr<=2050,"early",
                             ifelse (test2$yr>=2051 & test2$yr<=2070,"middle",
                                     ifelse (test2$yr>=2071 & test2$yr<2100, "late","NA"))))
head(test2)
str(test2)

#summarize by water year total flow
ann_sum<-as.data.frame(test2 %>%
                         group_by(gcm,rcp, wy) %>%
                         summarise(months=n(),sumflow = sum(total), period = period) %>% 
                         distinct(.keep_all = TRUE));head(ann_sum)

#count of years when water right met or not met
ann_ex<-as.data.frame(ann_sum %>%
                         mutate(rights_lost = sumflow < ann_right) %>% 
                         filter(months == 12) %>% 
                         group_by(gcm,rcp,period) %>% 
                         dplyr::summarize(rights_lost = sum(rights_lost == T),yrs=length(wy)))

#write.csv(ann_sum, "ann_sum.csv")
head(ann_ex)
unique(ann_ex$period)
unique(ann_ex$gcm)
ann_ex$pct_fail<-(ann_ex$rights_lost/ann_ex$yrs)*100
#determine the historical frequency of years when annual water right not met
hist_ex<-subset(ann_ex, gcm == "Historical", select = pct_fail );hist_ex

#drop MIROC-ESM-CHEM because it is missing rcp 4.5
ann_ex<-subset(ann_ex, gcm !="MIROC-ESM-CHEM" & gcm!="Historical")
head(ann_ex)
#write.csv(ann_ex, "ann_ex.csv")


#plot frequency of years water right not met as a percentage
p_levels<-c("early","middle","late")
plot <- ggplot(data = ann_ex, aes(x=period, y = pct_fail, fill=rcp)) + geom_col(position = "dodge")+ scale_x_discrete(limits = p_levels)+
  #geom_segment(aes(x=factor(period),xend=factor(period),yend=hist_ex))+
  geom_hline(yintercept=31, color="blue") +#31 is the % years fail historicall, not the water right
  #geom_segment(aes(x=factor(period),xend=factor(period),yend=hist_ex))+
  facet_wrap(vars(gcm))+
  ylab("Percent years fail to meet annual water right") +
  #scale_y_continuous(labels = percent)+
  nps_theme2()+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot

head(ann_sum);unique(ann_sum$gcm)
ann_sum_sub<-subset(ann_sum, gcm !="Historical" & gcm !="MIROC-ESM-CHEM");unique(ann_sum_sub$gcm)
ann_right#this is the annual water right as hline y-intercept
plot <- ggplot(data = ann_sum_sub, aes(x=period, y = sumflow, fill=rcp)) + geom_boxplot(position = "dodge")+ scale_x_discrete(limits = p_levels)+
  #geom_segment(aes(x=factor(period),xend=factor(period),yend=hist_ex))+
  geom_hline(yintercept=48, color="blue") +#this is the annual water right
  #geom_segment(aes(x=factor(period),xend=factor(period),yend=hist_ex))+
  facet_wrap(vars(gcm))+
  ylab("Annual flow versus annual water right (mm)") +
  #scale_y_continuous(labels = percent)+
  nps_theme2()+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot

meas_ann_fail_pct#hline y-intercept
plot <- ggplot(data = ann_ex, aes(x=period, y = pct_fail, fill=rcp)) + geom_boxplot(position = "dodge")+ scale_x_discrete(limits = p_levels)+
  #geom_point(aes(x=period, y = pct_fail, fill=rcp))+
  geom_hline(yintercept=36.6, color="blue") +
  #geom_segment(aes(x=factor(period),xend=factor(period),yend=hist_ex))+
  #facet_wrap(vars(gcm))+
  ylab("Percent years fail to meet annual water right") +
  #scale_y_continuous(labels = percent)+
  nps_theme2()+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot

#summarize annual water rights exceedance by rcp
ann_ex_summary<- ann_ex %>% 
  group_by(rcp, period) %>%
  summarise(mean=mean(pct_fail));ann_ex_summary

  
#########################monthly water rights###################################
#monthly water rights evaluation
monthly_right
str(monthly_right)
dfmr<-data.frame(monthly_right)
dfmr$mo<-c(1:12)#add a month that goes with monthly water right
dfmr;str(dfmr)

#join the monthly water right to the monthly modeled future flow data so comparisons can be made to determine if water right is met each month
test3<-merge(test2,dfmr,by.x="mo",by.y="mo");head(test3);tail(test3)
write.csv(test3,"test3.csv")

head(test3);unique(test3$gcm)
#count of months when water right met or not met
month_ex<-as.data.frame(test3 %>%
                        mutate(rights_lost = total < monthly_right) %>% 
                        #filter(months == 12) %>% 
                        group_by(gcm,rcp,period,mo) %>% 
                        dplyr::summarize(rights_lost = sum(rights_lost == T),yrs=length(wy)));head(month_ex)

#convert #months rights fail over # yrs in period to a percentage failure
month_ex$pct_fail<-(month_ex$rights_lost/month_ex$yrs)*100;head(month_ex)
unique(month_ex$gcm)
month_ex_hist<-subset(month_ex, gcm == "Historical");month_ex_hist#for MODELED flow

#determine monthly exceed historic using measured flow##
meas_monthly
#join the monthly water right to the monthly modeled future flow data so comparisons can be made to determine if water right is met each month
test4<-merge(meas_monthly,dfmr,by.x="mo",by.y="mo");head(test4);tail(test4)

#count of months when water right met or not met
meas_month_ex<-as.data.frame(test4 %>%
                          mutate(rights_lost = monthly_meas < monthly_right) %>% 
                          #filter(months == 12) %>% 
                          group_by(mo) %>% 
                          dplyr::summarize(rights_lost = sum(rights_lost == T),yrs=length(wy)));head(meas_month_ex)

#convert #months rights fail over # yrs in period to a percentage failure
meas_month_ex$pct_fail<-(meas_month_ex$rights_lost/meas_month_ex$yrs)*100;head(meas_month_ex)#for MEASURED flow
month_ex_hist<-subset(month_ex, gcm == "Historical");month_ex_hist#for MODELED flow


#provide information needed for labeling facets
month_names <- c(
  `1` = "January",
  `2` = "February",
  `3` = "March",
  `4` = "April",
  `5` = "May",
  `6` = "June",
  `7` = "July",
  `8` = "August",
  `9` = "September",
  `10` = "October",
  `11` = "November",
  `12` = "December"
)

meas_month_ex#frequency of water rights failure in historic gage record
month_ex_hist#frequecy of water rights failure in historic modeled record
#plot frequency of years water right not met using historic modeled flow
plot <- ggplot(data = month_ex, aes(x=period, y = pct_fail, fill=rcp)) + geom_boxplot(position = "dodge")+ scale_x_discrete(limits = p_levels)+
  geom_hline(data = month_ex_hist, aes(yintercept = pct_fail))+#make this historical monthly frequency of exceedance
  facet_wrap(vars(mo), labeller = as_labeller(month_names))+
  ylab("Percent years fail to meet monthly water right") +
  #scale_y_continuous(labels = percent)+
  nps_theme2()+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot

#plot frequency of years water right not met using historic MEASURED flow
plot <- ggplot(data = month_ex, aes(x=period, y = pct_fail, fill=rcp)) + geom_boxplot(position = "dodge")+ scale_x_discrete(limits = p_levels)+
  geom_hline(data = meas_month_ex, aes(yintercept = pct_fail))+#make this historical monthly frequency of exceedance
  facet_wrap(vars(mo), labeller = as_labeller(month_names))+
  ylab("Percent years fail to meet monthly water right") +
  #scale_y_continuous(labels = percent)+
  nps_theme2()+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot

head(test3)
#plot monthly projected flows vs water right
plot <- ggplot(data = test3, aes(x=period, y = total, fill=rcp)) + geom_boxplot(position = "dodge")+ scale_x_discrete(limits = p_levels)+
  geom_hline(data = dfmr, aes(yintercept = monthly_right), color="blue")+
  facet_wrap(vars(mo), labeller = as_labeller(month_names))+
  scale_y_continuous (trans='log10') +
  ylab("Projected monthly flow (mm, log scale)") +
  #scale_y_continuous(labels = percent)+
  nps_theme2()+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot


############################################################################
############################################################################
#Check last year in historical time series to make sure it is a full year
head(daily_df)
daily_sub<-subset(daily_df, gcm =="Historical");tail(daily_sub)
plot(daily_sub$date, daily_sub$total, type="l")
ann_sub<-subset(annual_df, gcm =="Historical");ann_sub
plot(ann_sub$yr, ann_sub$total, type="l")#last value should not go to zero


##################ensemble data frames###########################
ens_daily <- compiled %>%
  group_by(rcp, date) %>%
  dplyr::summarize(modrunoff = mean(modrunoff, na.rm = TRUE),quick = mean(quick, na.rm = TRUE),
                   slow = mean(slow, na.rm = TRUE),total = mean(total, na.rm = TRUE))
ens_daily<-as.data.frame(ens_daily);head(ens_daily)

ens_monthly <- monthly_df %>%
  group_by(rcp, yr_mo) %>%
  dplyr::summarize(modrunoff = mean(modrunoff, na.rm = TRUE),quick = mean(quick, na.rm = TRUE),
                   slow = mean(slow, na.rm = TRUE),total = mean(total, na.rm = TRUE))
ens_monthly<-as.data.frame(ens_monthly);head(ens_monthly)

ens_annual <- annual_df %>%
  group_by(rcp, yr) %>%
  dplyr::summarize(modrunoff = mean(modrunoff, na.rm = TRUE),quick = mean(quick, na.rm = TRUE),
                   slow = mean(slow, na.rm = TRUE),total = mean(total, na.rm = TRUE))
ens_annual<-as.data.frame(ens_annual);head(ens_annual)

ens_annual_45<-subset(ens_annual, rcp == 45)
ens_annual_45$yr<-as.numeric(ens_annual_45$yr)#http://127.0.0.1:25719/graphics/plot_zoom_png?width=1920&height=1137
head(ens_annual_45); str(ens_annual_45)#http://127.0.0.1:25719/graphics/plot_zoom_png?width=1920&height=1137

ens_annual_85<-subset(ens_annual, rcp == 85)
ens_annual_85$yr<-as.numeric(ens_annual_85$yr)
head(ens_annual_85); str(ens_annual_85)#http://127.0.0.1:8693/graphics/plot_zoom_png?width=1920&height=1137
###########################################################################
#plottinghttp://127.0.0.1:8741/graphics/plot_zoom_png?width=1904&height=964
###########################################################################
head(annual_df);tail(annual_df)
madf<-melt(annual_df, id.vars=c("gcm", "yr", "rcp"), measure.vars=c("total"));head(madf)
madf_sub<-subset(madf, gcm == c("Historical","BNU-ESM_rcp45"));head(madf_sub)#for plotting just one future with historical
madf_sub<-subset(madf, gcm == c("Historical"));head(madf_sub)#for plotting just one future with historical
#remove historical from futures for plotting and color coding separately
madf2<-subset(madf, gcm !="Historical");unique(madf2$gcm)
hist_sub<-subset(madf, gcm =="Historical")#pull historical data for plotting separately if needed

plot<-ggplot(data = madf2, aes(x=yr, y = value, color=gcm)) + geom_line()+#scale_color_viridis(discrete=TRUE)+#scale_fill_gradientn(colours=c("black","gray"))+#, alpha=westus_3_hs
  geom_line(data = ens_annual_85, aes(x=yr, y = total), col="red", size = 2)+#facet_wrap(~gcm)+ ylab("Stream flow (mm)") + xlab("Year")+
  geom_line(data = ens_annual_45, aes(x=yr, y = total), col="blue", size = 2)+
  geom_line(data = madf_sub, aes(x=yr, y=value), col = "black", size = 2)+
  #facet_wrap()
  xlab("Year")+ ylab("Annual flow (mm)")+
  ggtitle("Wet Beaver Creek annual flow")+
#color=NAor lwd = 0.01
nps_theme2()#;plot+
plot

plot(ens_annual_45$yr, ens_annual_45$total, type = "l")


#### Abby Code ####

total_85 <- daily_df %>% filter(rcp == "85" | rcp == "Hist") %>% ggplot() +
  geom_line(aes(x=date, y=total, color = gcm)) +ylim(0,30) + 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP8.5") +
  theme_bw()

total_45 <- daily_df %>% filter(rcp == "45" | rcp == "Hist") %>% ggplot() +
  geom_line(aes(x=date, y=total, color = gcm)) +ylim(0,30)+ 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP4.5") +
  theme_bw()

gridExtra::grid.arrange(total_45, total_85, ncol=2)


slow_85 <- daily_df %>% filter(rcp == "85" | rcp == "Hist") %>% ggplot() +
  geom_line(aes(x=date, y=slow, color = gcm)) +ylim(0.02,0.035) + 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP8.5") +
  theme_bw()

slow_45 <- daily_df %>% filter(rcp == "45" | rcp == "Hist") %>% ggplot() +
  geom_line(aes(x=date, y=slow, color = gcm)) +ylim(0.02,0.035)+ 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP4.5") +
  theme_bw()

gridExtra::grid.arrange(slow_45, slow_85, ncol=2)

quick_85 <- daily_df %>% filter(rcp == "85" | rcp == "Hist") %>% ggplot() +
  geom_line(aes(x=date, y=quick, color = gcm)) +
  ylim(0, 30) + 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP8.5") +
  theme_bw()

quick_45 <- daily_df %>% filter(rcp == "45" | rcp == "Hist") %>% ggplot() +
  geom_line(aes(x=date, y=quick, color = gcm)) +
  ylim(0, 30) + 
  xlim(as.Date.character(c("1978-12-31", "2101-01-01"))) +
  labs(title = "RCP4.5") +
  theme_bw()

gridExtra::grid.arrange(quick_45, quick_85, ncol=2)

daily <- daily_df %>% 
  mutate(daily_cfs = (total*35.31467*(287.4887/86.4))) %>% 
  filter(gcm != "MIROC-ESM-CHEM") %>% 
  select(c("date", "yr", "gcm", "yr_mo", "daily_cfs", "rcp"))

write.csv(daily, file = "daily.csv", row.names = F)
daily <- read_csv("daily.csv")


##### Abby Functions Below ####


# Function wrangle_projections
wrangle_projections <- function(dataframe, summary_var, ...) {
  #' dataframe = the input dataframe to wrangle. Should be dailys
  #' summary_column = the column you are performing the summary on (tidy eval)
  #' ... = the variables to group_by (tidy eval)
  summary_var <- enquo(summary_var) # tidy eval  
  
  df <- dataframe %>%
    group_by(...) %>%
    dplyr::summarize_at(vars(!! summary_var),
                        list(mean=mean,
                             Q01=~quantile(., probs = 0.01),
                             Q05=~quantile(., probs = 0.05),
                             Q25=~quantile(., probs = 0.25),
                             median=median,
                             Q75=~quantile(., probs = 0.75),
                             Q95=~quantile(., probs = 0.95),
                             Q99=~quantile(., probs = 0.99)))
  split(df, df$rcp)
}

# Function graph_timeseries_quantile
graph_timeseries_quantile <- function(ts_list,
                                      time_step,
                                      hist_rcp_name = "Hist",
                                      rcp = "45",
                                      ylow = Q05,
                                      yhigh = Q95,
                                      xaxis = date,
                                      ysmooth = mean,
                                      titles = F,
                                      nps = F,
                                      proj_col = "red"
) {
  
  df_hist <- ts_list[[hist_rcp_name]]
  df_rcp <- ts_list[[rcp]]
  get_label <- function(x) {
    x <- rlang::as_label(x)
    switch(x,
           "Q01" = "1st percentile",
           "Q05" = "5th percentile",
           "Q25" = "25th percentile",
           "median" = "median",
           "mean" = "mean",
           "Q75" = "75th percentile",
           "Q95" = "95th percentile",
           "Q99" = "99th percentile",
           "unknown percentile"
    )
  }
  
  labels <- c(
    ylow = get_label(enquo(ylow)),
    yhigh = get_label(enquo(yhigh)),
    ysmooth_fut = paste("ensemble", get_label(enquo(ysmooth))),
    ysmooth = get_label(enquo(ysmooth))
  )
  
  pal_color <- c(
    ylow = "dodgerblue4",
    yhigh = "aquamarine",
    ysmooth = "black",
    ysmooth_fut = proj_col
  )
  
  pal_lty <- c(
    ylow = 2,
    yhigh = 3,
    ysmooth = 1,
    ysmooth_fut = 1
  )
  
  p <- df_hist %>% 
    ggplot(aes(x = {{ xaxis }}, y = {{ ysmooth }})) +
    # geom_line(aes(y = {{ ylow }}, color = "ylow", lty = "ylow"), lwd = 1) +
    # geom_line(aes(y = {{ yhigh }}, color = "yhigh", lty = "yhigh"), lwd = 1) +
    # geom_ribbon(aes(x = {{ xaxis }}, ymin = {{ ylow }}, ymax = {{ yhigh }}),
    #             fill = "#E0EEEE", alpha = 0.5
    # ) +
    geom_smooth(method = "loess", se = F, col = "gray") +
    geom_line(aes(color = "ysmooth", lty = "ysmooth"), lwd = 1) +
    
    
    geom_ribbon(data = df_rcp, 
                aes(x={{xaxis}}, ymin = {{ylow}}, ymax = {{yhigh}}), 
                fill = "#E0EEEE", alpha = 0.5) +
    geom_smooth(data = df_rcp, 
                aes(x={{xaxis}}, y = {{ysmooth}}), 
                method = "loess", se=F, col="gray") +
    geom_line(data = df_rcp, aes(x={{xaxis}}, y =  {{ysmooth}}, 
                                 color = "ysmooth_fut", 
                                 lty = "ysmooth_fut"), lwd = 1)+
    geom_line(data = df_rcp, 
              aes(y={{ylow}}, color="ylow", lty="ylow"), lwd=1) +
    geom_line(data = df_rcp, 
              aes(y={{yhigh}}, color="yhigh", lty="yhigh"), lwd=1) +
    theme_bw() +
    scale_color_manual(
      name = " ",
      limits = rev,
      labels = labels,
      values = pal_color) +
    scale_linetype_manual(
      name = " ",
      limits = rev,
      labels = labels,
      values = pal_lty) +
    labs(
      x = "Year", y = "Flow (cfs)",
    ) 
  if(nps == T & titles == T) {
    p <- p + 
      nps_theme2() + labs(
        title = paste("Historical and Projected RCP", rcp, time_step, "Streamflow") 
      )
  } else if(nps == T & titles == F) {
    p <- p + nps_theme2()
  } else if(nps == F & titles == T) {
    p <- p + 
      labs(
        title = paste("Historical and Projected RCP", rcp, time_step, "Streamflow") 
      )
  } else{
    p
  }
  return(p)
}

# Function timeseries_quantile_rcp_grid
  # Uses gridExtra to automate the display of the graph generated from above func
timeseries_quantile_rcp_grid <- function(ts_list,
                                         time_step,
                                         hist_rcp_name = "Hist",
                                         ylow = Q05,
                                         yhigh = Q95,
                                         xaxis = date,
                                         ysmooth = mean,
                                         titles = F,
                                         nps = F,
                                         rcp_45_col = "orange",
                                         rcp_85_col = "red") {
  plot_1 <- graph_timeseries_quantile(ts_list,
                                      time_step,
                                      rcp = "45",
                                      hist_rcp_name,
                                      {{ylow}},
                                      {{yhigh}},
                                      {{xaxis}},
                                      {{ysmooth}},
                                      titles = titles,
                                      nps = nps,
                                      proj_col = rcp_45_col)
  plot_2 <- graph_timeseries_quantile(ts_list,
                                      time_step,
                                      rcp = "85",
                                      hist_rcp_name,
                                      {{ylow}},
                                      {{yhigh}},
                                      {{xaxis}},
                                      {{ysmooth}},
                                      titles = titles,
                                      nps = nps,
                                      proj_col = rcp_85_col)
  gridExtra::grid.arrange(plot_1, plot_2, ncol=2)
}


#### Apply Abby Functions for figures :) ####

wrangled_daily <- wrangle_projections(daily, daily_cfs, rcp, date)
wrangled_monthly <- wrangle_projections(daily, daily_cfs, rcp, yr_mo)
wrangled_annual <- wrangle_projections(daily, daily_cfs, rcp, yr)

graph_timeseries_quantile(wrangled_annual, time_step = "Annual", xaxis = yr, nps = F, proj_col = "pink")
timeseries_quantile_rcp_grid(wrangled_annual, ysmooth = mean, ylow = Q05, yhigh = Q95,
                             time_step = "Annual", xaxis = yr, nps = T)
graph_timeseries_quantile(wrangled_annual, rcp = "85", time_step = "Annual", xaxis = yr, nps = F, proj_col = "pink")

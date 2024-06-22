# ----------------------------------------------------------------------------------------
# R Script to search optimal robot parameters
# ----------------------------------------------------------------------------------------
# (C) 2024 Vladimir Zhbanko
# https://www.udemy.com/course/self-learning-trading-robot/?referralCode=B95FC127BA32DA5298F4
#
# Make sure to setup Environmental Variables, see script set_environment.R
#
# Main idea to implement:
#' 1. MT4 robot writes key parameters during optimization passes 
#' 2. MT4 robot also writes order trade results
#' 3. The optimization is started from the Windows Task Scheduler (Weekly)
#' 4. R script is started soon after the optimization completed (Weekly)
#' 5. R script reads Settings file from the robot
#' 6. R script reads parameters logged during optimization
#' 7. R script reads trading results during optimization
#' 8. R script joins both parametes and trading results
#' 9. R script calculates best optimization result
#' 10.R script finds the best parameter
#' 11.R script overwrites the new setting
#' 12.R script writes new settings file to the MT4 folder
#' 13.R script deletes the files recorded during optimization
#' 14.Process repeats during another week
#' 15.Robots awaits the result of the backtest found in the procedure opt_PerformBacktesting.R
#' 16.Trades will only resume should the result of backtest indicates positive outcome
#'
# 
# load libraries to use and custom functions
library(dplyr)
library(readr)
library(magrittr)
library(lazytrade)
library(lubridate)


# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path *** make sure to customize this path
path_T1 <- normalizePath(Sys.getenv('PATH_T1'), winslash = '/')
path_T1_E <- normalizePath(Sys.getenv('PATH_T1_E'), winslash = '/')
path_T1_I <- normalizePath(Sys.getenv('PATH_T1_I'), winslash = '/')
path_T1_P <- normalizePath(Sys.getenv('PATH_T1_P'), winslash = '/')
path_T1_te <- normalizePath(Sys.getenv('PATH_T1_te'), winslash = '/')
path_T1_tf <- normalizePath(Sys.getenv('PATH_T1_tf'), winslash = '/')



#path to user repo:
path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')

#path to folder with bat script
path_dss <- normalizePath(Sys.getenv('PATH_DSS_Repo'), winslash = '/')

# Read parameters of the robot, Robot settings are in C:\Program Files (x86)\FxPro - Terminal1\MQL4\Presets
DF_presets <- read_lines(file.path(path_T1_P, 'Falcon_D.set'))


# Find new parameters by optimization (launch the bat script from R)
# Specify the path to your batch script
#batch_script_path <- file.path(path_dss, "FALCON_D/AUTO_BACKTEST", "FalconDKillOptimizeAndAutoLaunch.bat")

# Launch the batch script
#system(batch_script_path, wait = TRUE)

# sleep 30 seconds to allow it to run ?
#Sys.sleep(30)

# Read order results
DFT1 <- try(import_data(path_T1_tf, "OrdersResultsT1.csv"), silent = TRUE)

# Read log with corresponding parameters
DF_Pars <- readr::read_csv(file.path(path_T1_tf, "ParameterLog9142101.csv"), 
                           col_names = c("MagicNumber",
                                         "TicketNumber",
                                         "OrderStartTime",
                                         "StartHour",
                                         "MinPipLimit",
                                         "UseMAFilter",
                                         "FastMAPeriod",
                                         "SlowMAPeriod",
                                         "RSI_NoBuyFilter",
                                         "RSI_NoSellFilter",
                                         "TimeMaxHold",
                                         "Buy_True",
                                         "Sell_True"))

DF_Pars$OrderStartTime <- lubridate::ymd_hms(DF_Pars$OrderStartTime)

# Combine columns from both datasets
matched_combined <- inner_join(DF_Pars_filtered, DFT1, by = c("OrderStartTime", "TicketNumber"))

# Group by StartHour and any other selected parameters, calculate summary statistics
summary <- matched_combined %>%
  group_by(StartHour) %>% # or group_by(StartHour, Par1) if more unique combinations of optimization
  summarize(
    MaxProfit = max(Profit),
    TotalProfit = sum(Profit),
    NumRows = n(), # Calculate the number of rows in each StartHour group
    ProfitFactor = ifelse(TotalProfit != 0, TotalProfit / sum(abs(Profit)), NA) # Calculate profit factor with a check for zero sum of Profit
  ) %>%
  arrange(desc(TotalProfit)) # Sort by MaxProfit column in descending order

# Find the best parameter StartHour
# Extract the StartHour value from the top row of the summary dataframe
#' test start_hour <- 7
start_hour <- summary$StartHour[1]

# Filter DF_Pars based on the extracted StartHour value
DF_Res <- DF_Pars %>%
  filter(StartHour == start_hour) %>% 
  head(1)

# Find the element containing "StartHour"
index <- grep("StartHour=", DF_presets)

# Replace the value after = sign with the new value found earlier in start_hour

if (length(index) > 0) {
  DF_presets[index] <- gsub("=\\d+", paste0("=", start_hour), DF_presets[index])
}


# Write file back
# test: write_lines(DF_presets, file.path(path_T1_P, 'Falcon_D_new.set'))
write_lines(DF_presets, file.path(path_T1_P, 'Falcon_D.set'))


# Erase files from the tester\files folder which was eventually used earlier
# Specify the paths to the files you want to remove
file1 <- file.path(path_T1_tf, "OrdersResultsT1.csv")
file2 <- file.path(path_T1_tf, "ParameterLog9142101.csv")

# If any of the files exist, remove them
if (file.exists(file1)) { file.remove(file1)}
if (file.exists(file2)) { file.remove(file2)}


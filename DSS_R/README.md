
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Lazytrade System Deployment Instructions on Windows PC

## Introduction

This document shall be used to deploy `lazytrade` system on PC Windows.

This text is intended to:

-   Help with general understanding of system component and function
-   Facilitate setup in windows PC computer

### General system description

This system attempts to setup autonomous decision making process:

-   Gather past data
-   Manipulating to fit for a purpose
-   Analysis: Finding pattern, learning process to predict future based
    on the past
-   automated tests or simulation of such learning
-   decision making in present (unknown) demo environment
-   automatic selection of best performing models
-   lesson learn checking and update learning

The system is very complex and contain many single modules. The system
is quite complex to set and understand because the input to one module
is an output from another one

### Benefits

Nevertheless, there are two main advantages of studying and implementing
this:

-   Follow instructions will simplify installation, most of tasks are
    automated
-   Deep study of this system is a source of inspiration and Data
    Science knowledge

### Challenges

Of course, a series of disadvantages must be highlighted as well:

-   High complexity of the system
-   Many parameters were arbitrary selected, almost impossible to study
    them
-   Multiple failure points as system was built based on free open
    source software, lack of testing, redundancy and supervision
    infrastructure
-   It is no novel or holy grail system - past data can’t predict future
    results!!!

## Systems studied

Below list indicates some characteristics of the methods used and
systems ready for replication

-   Indicator Pattern as Price Prediction using Deep Regression
    Learning, further *DSS_Bot*
-   Reinforcement Deep Learning Experiment, further *DSS_DRL_Bot*

Above systems could be deployed by activating dedicated options using
environmental variables. Further described in the section ‘Installation’

Please deploy only one of these systems at the time

## Installation

### Prepare PC Windows

Reset you PC and setup it fresh with *Local Account*, for detailed help
follow Course 1 Setup Your Trading Environment

### Get the Code for the whole system

#### Install GitDesktop software

Create Github.com account

#### Clone Repositories

Clone them all to the default folder as follows:

`C:/Users/<username>/Documents/GitHub/<RepositoryName>`. This location
is mandatory \[see environmental variables\]

Location of repositories on GitHub.com:

-   <https://github.com/vzhomeexperiments/DSS_Public>
-   <https://github.com/vzhomeexperiments/Include>
-   <https://github.com/vzhomeexperiments/WatchDog>
-   <https://github.com/vzhomeexperiments/AutoLaunchMT4>
-   <https://github.com/vzhomeexperiments/Indicators>
-   <to be created> - Shiny app for system monitoring

### Install R and R Studio

Install R and R Studio

### Install Java

Java SE development Kit 8 shall be downloaded and installed

e.g.
<https://www.oracle.com/technetwork/java/javase/downloads/index.html>

Java is required to use deep learning functionalities of h2o, see
<https://docs.h2o.ai/>

### Install R packages

To facilitate R packages installation run script:
`DSS_Setup/1_install_packages.R`

### Installation of MT4 terminals

Install two MT4 terminals from your preferred broker but do not start
them yet. Finish installation and close terminals. Follow below steps:

-   rename the installation folder as Terminal1
-   copy and paste installation folder and rename it to be Terminal3
-   change security settings of both folders to allow `Write` mode

Note:

-   Edit shortcut to the Terminal.exe program to make sure that
    terminals are starting in a ‘portable’ mode

### Change security windows EDIT settings

make sure to change edit security settings for:

-   Trading Terminals

### Environmental Variables

Environmental Variables allow to reproduce `lazytrade` system on any PC
in a quickest possible way. To facilitate this process all EV’s could be
set by running a script.

Modify the following script example:
`DSS_Setup/2_set_environment_DRL_example.bat`

Customize and execute this bat script to automatically setup those E.V.
Re-execute this script in case variables were changed Don’t forget to
restart R Studio to see changes

#### Environmental variables Specification

*PATH_DSS* Windows path to the folder with R project

*PATH_DSS_Bot* Windows path to the folder with file setup.csv.

*PATH_DSS_Repo* Windows path to the version control repositories

*PATH_T1* Path to the terminal sandbox

*PATH_T3* Path to the terminal sandbox

*PATH_T1_E* Path to the experts folder

*PATH_T3_E* Path to the experts folder

*PATH_T1_I* Path to the Include folder

*PATH_T3_I* Path to the Include folder

*Script names*

These are the variables to keep track on the available code to be
executed on the schedule

*SCR_AML_Collect* `aml_CollectData.R` *SCR_AML_Score* `aml_ScoreData.R`
*SCR_AML_TestBuild* `aml_TestBuildTestModel.R` *SCR_MT_BuildModel*
`mt_stat_BuildModel.R` *SCR_MT_Score* `mt_stat_Score_Data.R`
*SCR_NEWS_Check* `news_CheckMacroEvents.R` *SCR_NEWS_Read*
`news_ReadFxCalendar.R` *SCR_RL_Adapt* `rl_Adapt_control.R`
*SCR_RL_Trigger* `rl_TradeTrigger.R` *SCR_UTIL_Clean* `util_CleanTemp.R`
*SCR_SYSTEM_Test* `util_SystemTest.R`

### Select instruments to work with

Open Symbols menu and select instruments to work with. Add those
instruments to the file: `5_pairs.txt`, separate symbols with commas
e.g.: EURUSD,GBPUSD,AUDUSD Do not use quotes

### Copy Code to the destination

Execute bat file `DSS_Setup/6_PushToMT4Prod.bat` this will copy all the
relevant code to the right places

In addition simple csv file will be written to indicate terminal numbers

### Manually copy file to the Startup Folder

Edit and copy `MetaTraderAutoLaunch.cmd` from AutoLaunchMT4 repository
to the Windows Startup Folder

### Generate initialization files to launch terminals automatically

Rename file `7_generate_ini_files_example.R` to the file
`7_generate_ini_files.R`

Edit this file `7_generate_ini_files.R` by using passwords and other
credentials from your broker

Note: file `7_generate_ini_files.R` must be ‘ignored’ from the version
control

Launch the content of the script, this will ensure that dedicated ini
files are written so MT4 terminals will be launched automatically with
required settings

### Deploy Robot on M15 chart

Deploy DSS_DRL_Bot on M15 chart

### Run task dss_schedule

Open Windows Task Scheduler and run the task `dss_schedule` this will
launch necessary working scripts

Manually stop tasks when needed or execute script `dss_delete` directly
from Windows Task Manager

### R Scripts Automation

All R scripts could be automated using R dedicated package.

### Passwords encryption

-   Setup ssh key e.g. using R studio interface:
    `Tools -> Global Options -> Git -> Create RSA key`

Accept defaults don’t create passphrase

-   Manually run commented code in `DSS_Setup/3_encr_passwords.R`

-   Comment out code again to avoid password overwrite

-   Clean History and Console \[CTRL + L\]

Schedule tasks that will schedule or delete tasks, see next

### Deploy tasks

User shall manually run the script `DSS_Setup/3_task_scheduling.R` This
script will deploy 2 tasks:

-   Task to schedule all tasks (this will create all tasks required to
    run lazytrade system)
-   Task to delete recurrent tasks (this will delete tasks that should
    not be running on Sunday. This is done to prevent tasks conflicts.
    Note that script will not delete tasks that should be executed on a
    weekly basis)

### Deploy MT4 robots

#### WatchDog

-   <https://github.com/vzhomeexperiments/WatchDog>

Robot Watchdog’s purpose is to ensure automatic login to the account at
reconnection or restart Make sure to add your Account to Favorites, then
deploy robot Watchdog on each of the terminals using M1 chart EURUSD Use
default settings

#### DSS_Bot

Purpose of this Robot is to execute decisions generated by Decision
Support System. Robot will trade by reading commands from files.

Notes:

-   Robot will only trade once all files are written
-   Robot shall be deployed on desired assets only
-   It will start executing trades only when all DSS steps are complete
-   Robot Name: DSS_Bot.mq4
-   Include files are required for this to run
-   Robot shall be deployed both in Terminal 1 and in Terminal 3

#### DSS_Bot requirements

Files required to run/test DSS_Bot:

#### MT4 Autolaunch

<https://github.com/vzhomeexperiments/AutoLaunchMT4>

Purpose of this repository is to let terminals start / restart
automatically after a power failure.

#### System Failure Alert Emails

It is highly recommended to use email alerts to control the system. For
a greater simplicity, dedicated gmail account (non secure) can be used

These are variables to use System Test and email alerts

*UTIL_EMAIL_Sender* `email@gmail.com` *UTIL_EMAIL_Reciever*
`email@gmail.com`

## DSS Description

### Detection of Market Type

Data for Market Type. Purpose of the Market Type is to assess the
current price action data pattern. Such assessment will be conducted
through the series of:

-   statistical data transformation
-   unsupervised learning (clustering)
-   supervised learning (classification)

#### Data for model build

Data to build a model is a pure price action data.

-   Use Robot DataWriter_v4.02
-   Deploy on Terminal 1
-   Chart EURUSD, D1 chart which means new data will be updated every 1
    day
-   UseBarsCollect=1600
-   Timeframe = 1 Hour
-   Collect Close Price = True
-   Pair Selection - Own Pair List
-   Own Pairs - according to the file 5_pairs.txt

• Check the Output: o File: AI_CP60-1600.csv

#### Data for production (scoring)

Data to score data with a model is a pure price action data.

-   Use Robot DataWriter_v4.02
-   Deploy on Terminal 1
-   Chart EURUSD, H1 chart which means new data will be updated every 1
    Hour
-   UseBarsCollect=300
-   Timeframe = 1 Hour
-   Collect Close Price = True
-   Pair Selection - Own Pair List
-   Own Pairs - according to the file 5_pairs.txt

• Check the Output: o File: AI_CP60-300.csv

#### R script to train model for MT

Script `mt_stat_BuildModel.R` will train the model that will identify
Market Type of the Asset

Open script and run it in R Studio

Check the Output: Model object: DL_Classification_60M

#### R script to classify Market Type

Script `mt_stat_ScoreData.R` will use the model and latest data to
classify current Market Type

Note that Market Type assignment will be random.

Open script mt_stat_Score_Data.R, run the script and check the output:

Number files: AI_MarketType\_\[SYMBOL\]\[TIMEFRAME\].csv will be
generated. File will contain a string with a market type as well as a
confidence level of prediction

Script will be executed every hour thanks to the task `dss_mt_score`

### Price Change Prediction

#### Data for Model Build - to find the best input

Data is generated by robot DataWriterv7.00 see:

`/DSS_Bots/DataWriter_v7.00.mq4`

Purpose of this robot is to provide data input for the Neural Network
Model (further NN). Such NN Model will get the input data pattern and
the respective future price change of the asset Model Building process
will involve finding the best model parameters which explain the
relationship between the input pattern and the price change.

As it is unknown which inputs are working best system will be using
several input configurations

DataWriter v 7.00 shall be deployed on D1 chart as follows:

-   Terminal 1
-   EURUSD D1 chart which means new data will be updated every 1 day
-   Use Bars Collect = 2400 which means that robot will collect max 2400
    bars in the past
-   Choose Timeframe = 15 Minutes, 30 Minutes, etc parameter to decide
    timeframe to collect
-   Parameter `Choose True… = False`
-   Parameters `Use6_xx` = True select all desired inputs to work with
-   Pair Selection = Own Pair list
-   OwnPairs - paste content of the file 5_pairs.txt

Check the Output:

-   Sub-Folders with Files: AI_RSIADX\[SYMBOL\]\[TIMEFRAME\].csv
-   Scroll and check files are filled with data containing 2400 rows,
    zeroes below indicate that history is not filled with data

#### Script to find the best NN input

Dedicated script
<https://github.com/vzhomeexperiments/DSS_Bot/blob/master/DSS_R/_PROD/aml_Simulation.R>
is capable to perform model training and test to determine which input
is best for the aml algorithm Output of this script is the file
AccountBestInput.csv. File will be written to PATH_T1

Manually override file AccountBestInput.csv if something is wrong with
the execution of such script

such file must contain:

value 6_06

and be located in the sandox of the Terminal1

#### Data for Model Build - to use one NN input

Data for the model was previously defined. A file `AccountBestInput.csv`
must be present in the sandbox of the Terminal 1. Such file must
contain:

value 6_06

Run script `_PROD/aml_TestBuildTestModel.R`

Purpose of this script is to build a NN model for every single symbol.

#### Data for Model Score - to allow DSS to score data in production

It will be necessary to generate runtime data for using NN model in
production.

For that purpose `DataWriter v 7.00` shall be deployed on M15 or H1
chart. The timeframe M15 or H1 will define frequency of data refresh.
Robot will use the best input as define in the file
`AccountBestInput.csv`

Following parameters and conditions shall be set:

-   Terminal 1
-   EURUSD, M15 Chart
-   Use Bars Collect = 500
-   Parameter `Choose True… = True`
-   Parameters `Use6_xx` = False
-   Timeframe = 15 Minutes Timeframe to retrieve data pattern
-   Pair Selection = `Own Pair List`

• Output:

-   number of Files: AI_RSIADX\[SYMBOL\]\[TIMEFRAME\].csv

Scroll and check files are filled with data containing 500 rows, zeroes
below indicate that history is not filled with data

#### R script for Model Score - DSS to score data in production

A dedicated script `aml_ScoreData.R` is used to generate prediction of
the future price change Such prediction is based on the data pattern and
NN model

Script is executed by the task `dss_aml_score` every 15 min

### Initiated Trades on Terminal 1

Once above tasks are deployed trading robots on MT4 terminal 1 will
start to trade. Such trades are intended to be done using demo money. It
might take several days until first orders are actually closed. Further
scripts will only be feasible once sufficient amount of closed trades
are generated inside the file `OrderResultsT1.csv`

### Reinformcement Learning

#### Script to collect data

This script will be collecting the data from all dedicated sub-folders
and write rds files with aggregated data. Files will be written into
\_DATA/6_xx sub folders Rds files will be used to train new or update
existing models • Script \_PROD/aml_CollectData.R • Check output: o
\_DATA/6_xx/AI_RSIADX\[symbol\]\[timeframe\].rds o
\_DATA/AI_RSIADX\[symbol\]\[timeframe\].rds • Script will be running on
the hourly basis In addition to that, script will also collect recent
data to the input used ‘in production’. Such data will be needed to
support script aml_BoostModel.R see Script Boost Model

#### Build Model First time

This script is used to quickly build model first time. Script will first
read which input is the best input to use then it will create model
within that sub-folder.

• Script aml_ForceModelUpdate.R • Run that script manually once the
system is deployed first time • Check Output: o 28 models inside folder
\_MODELS/6_xx o 28 models inside sub-folders \_MODELS/6_xx/x o
DL_Regression-\[SYMBOL\]-60 Test Train Test Model This script is used to
improve the model on the weekly basis. The model that will be improved
will be selected based on the simulation • Script
aml_TestBuildTestModel.R • run script • Note: script runs several
hours!!! • check output: o updated models in a folder models o files
StrTest-\[SYMBOL\]M60.csv

#### Script Boost Model

Data for Scoring • Generated by robot see DataWriter v 7.00 deployed on
H1 chart: • Output: o Files: AI_RSIADX\[SYMBOL\]60.csv o Scroll and
check files are filled with data containing 50 rows Script to predict
price change • Script aml_ScoreData.R o Check Output:  Predicted price
change inside T1 and T3: AI_M60_Change\[SYMBOL\].csv  Files with a
strategy StrTest-\[SYMBOL\]M60.csv

## Automated tests to check the system

Dedicated task is created to test if DSS works fine.

### Health checks

Several checks will be conducted: Check that Input data is refreshed •
Check how old are the files • Check how many files are generated • etc
Check that New Predictions are being generated • Check how old are the
files • Check how many files are generated • etc Types of Emails alerts
Two email alerts will be activated: Watchdog alert Will run every week
at the point when script util_CleanTemp.R is executed. This is to make
sure email account is not blocked by google security as ‘unused’ Failure
Alert Will run every hour at the point when script util_SystemTest.R is
executed. Alerts eventually be triggered every hour as soon as one of
the test conditions fails…

### News Reading

#### Read Forex Calendar

#### Check Macro Events

### Reinforcement Learning

#### Adapt Control

#### Trade Trigger

### MT4 Terminals restart

As it is a healthy practice to restart MT4 terminals at least 1 x week…
Script to close terminals

Script is capable to programmatically close running MT4 Terminals WIP:
programmatically start MT4 terminals

## Development and Maintenance

### Version Control

General notes on how to use the system from the Version Control …
because this code could be deployed on several computers / servers while
still being under development Creating a branch from master In order to
have a stable system it’s advised to work from a branch. Branches could
be named as follows: • dev_name – under development • preprod_name –
under pre-production • prod_name – under production Bringing updates
from master to current branch In order to keep few local changes and
‘hot update’ the branch from master it is possible to follow these
steps: Open GitHub Desktop – Fetch origin

Go to History and select master branch to compare

Press field ‘behind’ and merge

Press ‘push origin’ to send changes to the GitHub.com

All changes from master are now on dev branch

### System Update

If required, general software update could be performed: Stop tasks From
Task Scheduler manually run task  
Software Update Watch youtube video ‘hot to update R’ e.g. Restart
system Run scripts Manually run script `/DSS_Setup/4_task_scheduling.R`
From Task Scheduler manually run to start DSS working Delete old models
Manually delete \[to automate\] all Models in the folder \_MODEL and all
sub-folders in the folder \_SIM Manually run task simulate

Manually run task test – build

Manually run task schedule

System would be running again

A combination of just two models will be implementing trade routine:

A. Function *WriteDataSetRLUnit()* A. *drl_BuildScore.R* B. *drl_Exit.R*

#### WriteDataSetRLUnit()

This facility, part of `Include/DSS_Functions.mqh`, will:

-   generate dataset with indicator values and class BUY or SELL (class
    is generated using past price knowledge with shift)
-   part of the input is the price delta knowledge from the past
-   write this dataset to the file `RLUnitXXXXXX.csv`

#### drl_BuildScore.R

This facility, part of `_PROD/drl_BuildScore.R` will:

-   read this file `RLUnitXXXXXX.csv` and generate decision model for
    the latest data
-   decision model will be binomial classification
-   data input will be the latest indicator data
-   script will aggregate data continuously
-   script will write decision to open a trade to the file
    `RLUnitOutxxxxxx.csv`

#### WriteDataSetRLUnitExit()

This facility, part of `Include/DSS_Functions.mqh`, will:

-   write current open order status and indicators
-   it’s main purpose is to provide data for decision to close trades
-   file `RLUnitxxxxxxExit.csv` will be written to the sandbox of the
    terminal

#### drl_Exit.R

This facility will:

-   read the file `RLUnitxxxxxxExit.csv`
-   aggregate data from this file
-   read the file `OrderResultsT1.csv`
-   Check which closed orders have a match with the aggregated data
-   Create class based on the result in the closed order
-   Generate decision model for the available data
-   decision model will be bi nomial classification
-   data input will be the latest order data
-   write lines of ticket numbers that are predicted to close
-   file `RLUnitOutXXXXXXExit.csv` will be generated

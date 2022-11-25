rem Script to Deploy files from Version Control repository to Development Terminal
rem Use in case some content needs to be replaced (reverted from Version Control History)

@echo off
setlocal enabledelayedexpansion

:: Source Directory where Version Control Repository is located
set SOURCE_DIR="%PATH_DSS_Repo%\DSS_Bot\DSS_Bots"
:: Destination Directory where Expert Advisor is located
set DEST_DIR="%PATH_T2_E%\DSS_Bot"

ROBOCOPY %SOURCE_DIR% %DEST_DIR% *.mq4


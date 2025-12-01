

# Proporção de domicilios com < 1.4 s.m. com insegurança alimentar


library(PNADcIBGE)
library(tidyverse)
library(hutils)
library(survey)
library(dplyr)

surveyPnad <- get_pnadc(year = 2023, topic=4)
dfPnadOrig <- surveyPnad$variables 

"V5001A" %in% names(dfPnadOrig)
"SD17001" %in% names(dfPnadOrig)



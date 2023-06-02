
library(DAAG)
library(tidyverse)
library(dplyr)

# read data
alc_claims = read.csv("C:/Users/Katon/Documents/JHU/ReasoningUnderUncertainty/ResearchPaper/data/alc_claims.csv")

alc_claims %>% select(ClaimID, TotalPaid_End)



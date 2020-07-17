# Set working directory to shared/saqib/covid/covid on BEAR
# setwd("/shared/saqib/covid/covid")


## !!!Two variables with "loyalty" name in the dataset. Dropping one in process_data. Check with new data ##


library(data.table)
library(lubridate)
library(dplyr)
library(stargazer)
library(lfe)
library(stringr)
library(skimr)
library(fastDummies)
library(broom)
library(dotwhisker)
library(texreg)


begin_date = ymd("2020-03-01")
end_date = ymd("2020-04-15")

naics_digit = 3 ## NAICS level used to aggregate places

naics_essentail_cutoff = .30   #Percent closed threshold to identify essential services





####################################
####################################

source(file = "code/Functions_small.r")

## Read Master File and create add covriates 
process_data()

## Subset for local stores
generate_nb()

generate_summary_table()


source(file = "code/main_IV_Reg.R")

source(file="code/moderation_Reg.R")


fix_names_summary("tables/table1a.tex")
fix_names_summary("tables/table1b.tex")
fix_names("tables/table2a.tex")
fix_names("tables/table2b.tex")
fix_names("tables/table3.tex")


library(data.table)
library(lubridate)
library(dplyr)
library(stargazer)
library(lfe)
library(stringr)

#data = fread("preRegData.csv") %>% as.data.frame()

data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)

forhp = fread("forhp-eligible-zips.csv")
forhp = forhp %>% mutate(rural = TRUE)
forhp$ZIP = as.integer(forhp$ZIP)

census_names = fread("rawdata/safegraphcensus219_VariableNamesFixed.csv") %>% as.data.frame()
census = fread("rawdata/safegraphcensus219.csv") %>% as.data.frame()
names(census) = names(census_names)

census = census %>% mutate(CBGFIPS = CENSUS_BLOCK_GROUP)
census = census %>% group_by(CBGFIPS) %>% mutate(meanI = sum(5*I10k + 12.5*I10k15k + 17.5*I15k20k + 22.5*I20k25k + 27.5*I25k30k + 32.5*I30k35k + 37.5*I35k40k + 42.5*I40k45k + 47.5*I45k50k + 55*I50k60k + 67.5* I60k75k + 82.5*I75k100k + 112.5* I100k125k + 132.5*I125k150k + 175*I150k200k + 300*I200k)/Itotal)
census = census %>% group_by(CBGFIPS) %>% mutate(MeanLogDev = sum( I10k*log(meanI/5) + I10k15k*log(meanI/12.5) + I15k20k*log(meanI/17.5) + I20k25k*log(meanI/22.5) + I25k30k*log(meanI/27.5) + I30k35k*log(meanI/32.5) + I35k40k*log(meanI/37.5) + I40k45k*log(meanI/42.5) + I45k50k*log(meanI/47.5) + I50k60k*log(meanI/55) + I60k75k*(log(meanI/67.5)) + I75k100k*log(meanI/82.5) + I100k125k*log(meanI/112.5) + I125k150k*log(meanI/132.5) + I150k200k*log(meanI/175) + I200k*log(meanI/300) ))

census['PercentWhite'] = census$RWhite/census$RTotal
census['PercentBlack'] = census$RBlackAlone/census$RTotal
census['PercentAsian'] = census$RAsianalone/census$RTotal


data_nb$CBGFIPS = str_pad(data_nb$CBGFIPS,12,side = "left",pad = "0")
census$CBGFIPS = str_pad(census$CBGFIPS,12,side = "left",pad = "0")

temp = anti_join(data_nb,census)
missed_CBGs = temp %>% select(CBGFIPS)  %>% distinct()

## Join with Census
data_nb = left_join(data_nb,census)

#temp = anti_join(data_nb,forhp, by = c("postal_code"="ZIP"))
#missed_CBGs = temp %>% select(CBGFIPS)  %>% distinct()

data_nb = left_join(data_nb,forhp, by = c("postal_code" = "ZIP"))

#data_nb$rural = ifelse(is.na(data_nb$rural),FALSE,TRUE)



data_nb$CBGFIPS = str_pad(data_nb$CBGFIPS,12,side = "left",pad = "0")

age_data = fread("rawdata/census_age_block.csv")
names(age_data) = gsub("Geo_","",names(age_data))
names(age_data)[14:26] = c("TotalPopulation","Under5","A5to9","A10to14","A15to17","A18to24","A25to34","A35to44","A45to54","A55to64","A65to74","A75to84","A85andOver")

age_data = age_data %>% group_by(FIPS,STATE,COUNTY,TRACT)  %>% mutate(avg_age = (Under5*2.5 + A5to9*7.5 + A10to14*12 + A15to17*16 + A18to24*21 + A25to34*29.5 + A35to44*39.5 + A45to54*49.5 + A55to64*59.5 + A65to74*69.5 + A75to84*79.5 + A85andOver*92)/TotalPopulation)

age_data = age_data %>% select(FIPS,STATE,COUNTY,TRACT,avg_age,TotalPopulation)

age_data$FIPS = as.character(age_data$FIPS)

data_nb = left_join(data_nb, age_data, by = c("CBGFIPS" = "FIPS"))
data_nb = data_nb %>% filter(!is.na(avg_age))
data_nb = data_nb %>% group_by(postal_code) %>% mutate(avg_age_zip = sum(avg_age*TotalPopulation,na.rm = T)/sum(TotalPopulation,na.rm = T))

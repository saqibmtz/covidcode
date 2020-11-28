
census_names = fread("rawdata/safegraphcensus219_VariableNamesFixed.csv") %>% as.data.frame()
census = fread("rawdata/safegraphcensus219.csv") %>% as.data.frame()
names(census) = names(census_names)
census = census %>% mutate(CBGFIPS = CENSUS_BLOCK_GROUP)

data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)

data_nb$CBGFIPS = str_pad(data_nb$CBGFIPS,12,side = "left",pad = "0")
census$CBGFIPS = str_pad(census$CBGFIPS,12,side = "left",pad = "0")

census = left_join(census, data_nb %>% select(CBGFIPS,postal_code))

meanIncome = census %>% group_by(postal_code) %>% summarise(meanI = sum(5* sum(I10k) + 12.5* sum(I10k15k) + 17.5*sum(I15k20k) + 22.5*sum(I20k25k) + 27.5*sum(I25k30k) + 32.5*sum(I30k35k) + 37.5*sum(I35k40k) + 42.5*sum(I40k45k) + 47.5*sum(I45k50k) + 55*sum(I50k60k) + 67.5* sum(I60k75k) + 82.5*sum(I75k100k) + 112.5* sum(I100k125k) + 132.5*sum(I125k150k) + 175*sum(I150k200k) + 300*sum(I200k))/sum(Itotal))

MeanLogDevIncome = census %>% group_by(postal_code) %>% select(starts_with('I'),Itotal) %>% summarise_all(sum)
MeanLogDevIncome = left_join(MeanLogDevIncome,meanIncome)
MeanLogDevIncome = MeanLogDevIncome %>% group_by(postal_code) %>% mutate(MeanLogDev = sum( I10k*log(meanI/5) + I10k15k*log(meanI/12.5) + I15k20k*log(meanI/17.5) + I20k25k*log(meanI/22.5) + I25k30k*log(meanI/27.5) + I30k35k*log(meanI/32.5) + I35k40k*log(meanI/37.5) + I40k45k*log(meanI/42.5) + I45k50k*log(meanI/47.5) + I50k60k*log(meanI/55) + I60k75k*(log(meanI/67.5)) + I75k100k*log(meanI/82.5) + I100k125k*log(meanI/112.5) + I125k150k*log(meanI/132.5) + I150k200k*log(meanI/175) + I200k*log(meanI/300) ))


RaceVars = census %>% group_by(postal_code) %>% summarise(PercentWhite = sum(RWhite)/sum(RTotal), PercentBlack = sum(RBlackAlone)/sum(RTotal), PercentAsian = sum(RAsianalone)/sum(RTotal))

census_data = left_join(meanIncome,MeanLogDevIncome) %>% left_join(.,RaceVars) %>% filter(!is.na(MeanLogDev))


data_nb = left_join(data_nb,census_data)
data_nb = data_nb %>% filter(!is.na(prop_home_device_zip)) %>% filter(!is.na(meanI)) %>% filter(!is.na(BrandPostalProp)) %>% filter(!is.nan(MeanLogDev))

#fitted_values = fs3$fitted.values %>% as.data.frame()
#names(fitted_values) = "fitted_values"
#data_nb = cbind(data_nb, fitted_values)

#local_iv_means = data_nb %>% group_by(postal_code) %>% summarise(prop_local = mean(proption_BigBrands_naics_postal_open),prop_fitted = mean(fitted_values))

###########################
### Instrument Variable ###
###########################


reg_x <- data_nb %>% felm(BrandPostalProp ~ 1 | newfactor + postal_code + date,.)
resid_x_2 <- resid(reg_x) + mean(data_nb$BrandPostalProp)

### Mean Income
reg_y <- felm(meanI ~ 1 | newfactor + postal_code + date, data_nb)
resid_y_2 <- resid(reg_y) + mean(data_nb$meanI)
plot2 = ggplot() + stat_summary_bin(aes(y = resid_y_2, x = resid_x_2),fun='mean',bins = 20,geom = "point",color="red")  + xlab("National Chain Opening Exposure")+ ylab("Mean Income")  + labs(title ="Mean Income vs. National Chain Opening Exposure") + ylim(0,max(data_nb$meanI,na.rm=T)) + theme_bw()
ggsave("plots/iv/bin_IV_income_IV.jpg",plot2) 


### Log Dev Income
reg_y <- felm(MeanLogDev ~ 1 | newfactor + postal_code + date, data_nb)
resid_y_3 <- resid(reg_y) + mean(data_nb$MeanLogDev)
plot3 =  ggplot() + stat_summary_bin(aes(y = resid_y_3, x = resid_x_2),fun='mean',bins = 20,geom = "point",color="red")  + xlab("National Chain Opening Exposure")+ ylab("Mean Log Deviation of Income")  + labs(title ="Income Inequality vs. National Chain Opening Exposure") + ylim(0,max(data_nb$MeanLogDev,na.rm=T)/100) + theme_bw()
ggsave("plots/iv/bin_IV_incomeIneq_IV.jpg",plot3) 

### Percent White
reg_y <- felm(PercentWhite ~ 1 | newfactor + postal_code + date, data_nb)
resid_y_4 <- resid(reg_y) + mean(data_nb$PercentWhite)

plot4 = ggplot()  + stat_summary_bin(aes(y = resid_y_4, x = resid_x_2),fun='mean',bins = 20,geom = "point",color="red")  + ylab("Prop. White") + xlab("National Chain Opening Exposure") + labs(title ="Prop. White vs. National Chain Opening Exposure") + ylim(0,1) + theme_bw()
ggsave("plots/iv/bin_IV_PercentWhite_IV.jpg",plot4)

#reg_y <- felm(PercentAsian ~ 1 | newfactor + postal_code + date, data_nb)
#resid_y_5 <- resid(reg_y) + mean(data_nb$PercentAsian)
#plot5 =  ggplot()  + stat_summary_bin(aes(y = resid_y_5, x = resid_x_2),fun='mean',bins = 20,geom = "point",color="red")  + ylab("Prop. Asian") + xlab("National Chain Opening Exposure") + labs(title ="Prop. Asian vs. National Chain Opening Exposure") + ylim(0,1) + theme_bw()
#ggsave("plots/iv/bin_IV_PercentAsian_IV.jpg",plot5)

#########################
########### Age ########
########################

data_nb = fread("filedata/data_nb_state.csv")
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

reg_x <- data_nb %>% felm(BrandPostalProp ~ 1 | newfactor + postal_code + date,.)
resid_x_3 <- resid(reg_x) + mean(data_nb$BrandPostalProp)

reg_y <- felm(avg_age_zip ~ 1 | newfactor + postal_code + date, data_nb)
resid_y_6 <- resid(reg_y) + mean(data_nb$avg_age)
plot5 =  ggplot()  + stat_summary_bin(aes(y = resid_y_6, x = resid_x_3),fun='mean',bins = 20,geom = "point",color="red")  + ylab("Mean Age") + xlab("National Chain Opening Exposure") + labs(title ="Mean Age vs. National Chain Opening Exposure") + theme_bw() + ylim(0,100)
ggsave("plots/iv/bin_IV_Age.jpg",plot5)



###########################################
####### COVID Cases & Death Data #########
###########################################

data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)

covidcases = fread("filedata/covidcases.csv")
zip2fips = fread("filedata/ZIP-COUNTY-FIPS_2017-06.csv")
covidcases$date = ymd(covidcases$date)

data_nb = left_join(data_nb, zip2fips, by = c("postal_code" = "ZIP"))
data_nb = left_join(data_nb,covidcases, by = c("STCOUNTYFP" = "fips","date" = "date"))

missing = sum(is.na(data_nb$cases))/nrow(data_nb)
print(paste("Missing: ",missing))

data_nb = data_nb %>% filter(!is.na(prop_home_device_zip))  %>% filter(!is.na(deaths)) %>% as.data.frame()
data_nb['DateZip'] = paste(data_nb$date, data_nb$postal_code)

reg_x <- felm(BrandPostalProp ~ 1 | newfactor + DateZip, data_nb)
resid_x_6 <- resid(reg_x) + mean(data_nb$BrandPostalProp)

#### Covid Cases ### 
reg_y <- felm(cases ~ 1 | newfactor + DateZip, data_nb)
resid_y_7 <- resid(reg_y) + mean(data_nb$cases)

plot6 =  ggplot() + stat_summary_bin(aes(y = resid_y_7, x = resid_x_6),fun='mean',bins = 20,geom = "point",color="red")  + xlab("National Chain Opening Exposure")+ ylab("Number of COVID Cases")  + labs(title ="Covid Cases vs. National Chain Opening Exposure") + ylim(0,max(data_nb$cases,na.rm=T)/100) + theme_bw()
ggsave("plots/iv/bin_IV_covidCases.jpg",plot6) 

plots = grid.arrange(plot2,plot4,plot5,plot6,ncol=2)
ggsave("plots/iv/IV_plots.jpg",plots,width=12,height=12)

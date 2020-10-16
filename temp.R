## October 5 Brand within NAICS avg. foot-traffic 

temp2 =  data %>% filter((BigBrandStores>0) & (LocalStores>0) & (BrandLocal == 0))
length(unique(temp2$naics_code))
length(unique(temp2$naics_code_6digit))
## Sept 17 Adding covidcases binscatter plot

covidcases = fread("filedata/covidcases.csv")
zip2fips = fread("filedata/ZIP-COUNTY-FIPS_2017-06.csv")
covidcases$date = ymd(covidcases$date)

data_nb = left_join(data_nb, zip2fips, by = c("postal_code" = "ZIP"))
data_nb = left_join(data_nb,covidcases, by = c("STCOUNTYFP" = "fips","date" = "date"))

missing = sum(is.na(data_nb$cases))/nrow(data_nb)
print(paste("Missing: ",missing))

data_nb = data_nb %>% filter(!is.na(prop_home_device_zip))  %>% filter(!is.na(deaths)) %>% as.data.frame()

plot2 = data_nb %>% filter(!is.na(cases)) %>% ggplot() + stat_summary_bin(aes(y = cases, x = BrandPostalProp),fun='mean',bins = 20,geom = "point",color="red")  + xlab("Prop. Brach Est. Open")+ ylab("Number of COVID Cases")  + labs(title ="Covid Cases vs. Prop. Branch Est. Open (fit)") + ylim(0,1200) 
ggsave("plots/eventstudy/bin_IV_covidCases_fitted.jpg",plot2) 


plot3 = data_nb %>% filter(!is.na(cases)) %>% ggplot() + geom_smooth(aes(y = cases, x = BrandPostalProp, group = naics_code, color = naics_code),se = F)  + xlab("Prop. Brach Est. Open")+ ylab("Number of COVID Cases")  + labs(title ="Covid Cases vs. Prop. Branch Est. Open (fit)") + ylim(0,1200) 
ggsave("plots/eventstudy/bin_IV_covidCases_fitted.jpg",plot3) 


date_naics_counts = data_nb %>% group_by(date,naics_code,postal_code) %>% distinct(BrandPostalProp,cases) 
plot3 = date_naics_counts %>% filter(!is.na(cases)) %>% ggplot() + geom_smooth(aes(y = cases, x = BrandPostalProp, group = naics_code, color = naics_code),se = F)  + xlab("Prop. Brach Est. Open")+ ylab("Number of COVID Cases")  + labs(title ="Covid Cases vs. Prop. Branch Est. Open (fit)") + ylim(0,1200) 
ggsave("plots/eventstudy/bin_IV_covidCases_fitted.jpg",plot3) 

### Plotting cases,Ind Var, Inst. Var vs. date

p1 = data_nb %>% ggplot(.) + geom_smooth(aes(x = date, y = cases/max(cases)*10,color="red")) 
p1 = p1 + geom_smooth(aes(x = date, y = BrandPostalProp, color = "green")) + geom_smooth(aes(x = date, y = proption_BigBrands_naics_postal_open, color = "blue"),se=F) + xlab("Outcome") + ylab("Date")

ggsave("plots/eventstudy/IV_date_plot.jpg",p1)

data_nb_zip = data_nb %>% group_by(naics_code,date,postal_code) %>% summarise(BrandPostalProp = mean(BrandPostalProp), proption_BigBrands_naics_postal_open = mean(proption_BigBrands_naics_postal_open),cases = mean(cases))
p2 = data_nb_zip %>% ggplot(.) + geom_smooth(aes(x = date, y = cases/max(cases)*10, color = "red"))
p2 = p2 + geom_smooth(aes(x = date, y = BrandPostalProp, color = "green")) + geom_smooth(aes(x = date, y = proption_BigBrands_naics_postal_open, color = "blue"),se=F) + xlab("Date") + ylab("Outcome")
ggsave("plots/eventstudy/IV_date_plot_zipLevel.jpg",p2)



## July 28th Adding county*date fixed effects
pincodes = fread("rawdata/zip_code_database_new.csv") %>% select(zip,county,state)
data_nb = left_join(data_nb, pincodes, by = c("postal_code" = "zip"))

data_nb['countyDate'] = paste(data_nb$county,data_nb$date)




## July 24th paper figure descriptives
filein = "filedata/preRegData_state.csv"
data = fread(filein)

zipcode = fread("rawdata/zip_code_database_new.csv") %>% select(zip,state)

data = left_join(data,pincodes, by = c("postal_code"="zip")) %>% filter(!is.na(state_name))

gym_naics = 713
counties = c(74012,74146)
figure_date = ymd("2020-03-24")

data = data %>% filter(naics_code==gym_naics)
data$date = ymd(data$date)

data_begindate = data %>% filter(date == begin_date)

county_data = data %>% filter(postal_code %in% counties)
county_data_begindate = county_data %>% filter(date == figure_date)

county_data_begindate %>% group_by(postal_code, brands,naics_code_6digit) %>% summarise(total = n())

county_data_date = county_data %>% filter(date == figure_date)
county_data_date %>% select(postal_code,BrandPostalProp,proption_BigBrands_naics_postal_open) %>% arrange(postal_code) %>% distinct()
county_data_date %>% filter(BrandNational==1) %>% select(postal_code,brands,open)

data_bb = data %>% filter(big_brands) %>% filter(brands %in% c("CrossFit","Planet Fitness"))
data_bb %>% filter(state_id!="OK") %>% filter(date == figure_date) %>% group_by(brands) %>% summarise(total_open = sum(open),total_stores= n(), pct_open = sum(open)/n())
data_bb %>% filter(state_id=="OK") %>% filter(date == figure_date) %>% group_by(brands) %>% summarise(total_open = sum(open),total_stores= n(), pct_open = sum(open)/n())

data_bb %>% filter(date == figure_date) %>% group_by(brands) %>% summarise(total_open = sum(open),total_stores= n(), pct_open = sum(open)/n())

national_open = data_bb%>% filter(date == figure_date) %>% group_by(brands,date) %>% summarise(nat_total=n(),nat_open=sum(open))

state_open = data_bb%>% filter(date == figure_date) %>% group_by(brands,date,state_id) %>% summarise(state_total=n(),state_open=sum(open))

state_open = left_join(national_open,state_open)
state_open = state_open %>% mutate(exclude_state_open = nat_open - state_open,exclude_state_tot = nat_total-state_total)
state_open %>% filter(state_id =="OK") %>% as.data.frame()
#bb_daily_open = data_bb %>% group_by(brands,date,naics_code) %>% summarise(prop_open_national = sum(open)/n()) %>% filter(wday(date)!=1 & wday(date)!=7)
bb_daily_open = data_bb %>% group_by(brands,date,naics_code) %>% summarise(prop_open_national = sum(open)/n())
bb_daily_open = bb_daily_open %>% group_by(brands,naics_code) %>% mutate(prop_change_national = prop_open_national - lag(prop_open_national,order_by=date)) %>% drop_na()

bb_daily_open = bb_daily_open %>% filter(brands %in% c("CrossFit","Planet Fitness"))
bb_daily_open %>% pivot_wider(id_cols = c(date,naics_code),names_from = brands, values_from = c("prop_open_national","prop_change_national")) %>% as.data.frame() %>% head(46)

bb_daily_open %>% filter(date >= ymd("2020-03-13") & (date <= ymd("2020-04-01"))) %>% arrange(brands,date) %>% as.data.frame()
p1 = ggplot(bb_daily_open) + geom_line(aes(x = date, y = prop_open_national, group = brands, color = brands)) + xlab("Date") + ylab("Prop. Open")
ggsave("plots/iv/gyms.png",p1)


#July 20th  Adding top 30 brands
filein = "filedata/preRegData_state.csv"
data = fread(filein)
data$date = ymd(data$date)
data_day = data %>% filter(date == begin_date)
big_brands_all = unique(data_day$brands[data_day$big_brands]) %>% as.data.frame()

top30 = fread("filedata/brandstop30.csv")
top30$Brand[top30$Brand == "Nissan"] = "Nissan North America"

sum(top30$Brand %in% big_brands_all$.)
top30 = top30 %>% select(Brand) %>% mutate(top30 = T)

data_day = left_join(data_day, top30, by = c("brands" = "Brand"))
data_day$top30 = ifelse(is.na(data_day$top30),F,T)

naics_include = data_day %>% group_by(naics_code) %>% summarise(top30_brands = sum(top30,na.rm=T)) %>% filter(top30_brands>0)
data_day = data_day %>% filter(naics_code %in% naics_include$naics_code)

top30_postal_naics = data_day %>% group_by(postal_code,naics_code) %>% summarise(top30 = ifelse(sum(top30)>0,T,F), prop_top30 = sum(top30)/n())
top30_naics_prop = top30_postal_naics %>% group_by(naics_code) %>% summarise(mean_prop = mean(prop_top30))

top30_naics_prop = left_join(top30_naics_prop,naics_codes)
top30_naics_prop %>% arrange(desc(mean_prop))  %>% select(mean_prop,naics_code,naics_name) %>% head(20)



data_nb = fread("filedata/data_nb_state.csv")

#Naics Include
#data_nb = data_nb %>% filter(naics_code %in% naics_include$naics_code)

data_nb = left_join(data_nb,top30_postal_naics)
#Postal Include
data_nb = data_nb %>% filter(top30==T)


dummies = dummy_cols(data_nb$top30) %>% select(-1)
names(dummies) = c("NotTop30","Top30")
data_nb = cbind(data_nb,dummies)

dummies = dummy_cols(data_nb$top30) %>% select(-1)
dummies = data_nb$proption_BigBrands_naics_postal_open * dummies
names(dummies) = c("Proportion_NotTop30","Proportion_Top30")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of loyal visitors * IV
dummies = dummy_cols(data_nb$top30) %>% select(-1)
dummies = data_nb$BrandPostalProp * dummies
names(dummies) = c("Expsoure_NotTop30","Exposure_Top30")
data_nb = cbind(data_nb,dummies)

model2<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip + Top30 |  newfactor + postal_code  | (  proption_BigBrands_naics_postal_open + Proportion_Top30  ~  Exposure_Top30 + BrandPostalProp) | postal_code,.)
stargazer(model2, type="text", covariate.labels = c("Avg. Feb Visitors","Prop. Devices Home","Top30 Brand","Prop Branch Est Open","Prop Branch Est Open x Top30 Brand"))

## Continuous * Continuous
data_nb['interact'] = data_nb["prop_top30"]*data_nb$proption_BigBrands_naics_postal_open
data_nb['interact_expsoure'] = data_nb["prop_top30"]*data_nb$BrandPostalProp

model4<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip + prop_top30 |  newfactor + postal_code  | (proption_BigBrands_naics_postal_open + interact  ~   BrandPostalProp + interact_expsoure) | postal_code,.)
stargazer(model4, type = "text",covariate.labels = c("Avg. Feb Visitors","Prop. Devices Home", "Prop. Top30 Brands","Prop Branch Est. Open","Prop Branch Est Open x Prop. Top30 Brands"))
 

#################################
###############################

data_nb = data_nb %>% filter(!is.na(prop_home_device_zip))

fitted_values = fs3$fitted.values %>% as.data.frame()
names(fitted_values) = "fitted_values"
data_nb = cbind(data_nb, fitted_values)

local_iv_means = data_nb %>% group_by(postal_code) %>% summarise(prop_local = mean(proption_BigBrands_naics_postal_open),prop_fitted = mean(fitted_values))

#plot1 = data_nb %>% filter(!is.na(meanI)) %>% ggplot()  + stat_summary_bin(aes(y = meanI, x = proption_BigBrands_naics_postal_open),fun='mean',bins = 20,geom = "point",color="blue")  + xlab("Prop. Brach Est. Open") + ylab("Mean Income") + labs(title ="Mean Income vs. Prop. Branch Est. Open") + ylim(0,max(data_nb$meanI,na.rm=T))
#ggsave("plots/iv/bin_IV_income.png",plot1)

plot2 = data_nb %>% filter(!is.na(meanI)) %>% ggplot() + stat_summary_bin(aes(y = meanI, x = fitted_values),fun='mean',bins = 20,geom = "point",color="red")  + xlab("Prop. Brach Est. Open")+ ylab("Mean Income")  + labs(title ="Mean Income vs. Prop. Branch Est. Open (fit)") + ylim(0,max(data_nb$meanI,na.rm=T)) 
ggsave("plots/iv/bin_IV_income_fitted.png",plot2) 


plot3 = data_nb %>% filter(!is.na(MeanLogDev)) %>% ggplot() + stat_summary_bin(aes(y = MeanLogDev, x = fitted_values),fun='mean',bins = 20,geom = "point",color="red")  + xlab("Prop. Brach Est. Open")+ ylab("Mean Log Deviation of Income")  + labs(title ="Income Inequality vs. Prop. Branch Est. Open (fit)") + ylim(0,max(data_nb$MeanLogDev,na.rm=T)) 
ggsave("plots/iv/bin_IV_incomeIneq_fitted.png",plot3) 


plot4 = data_nb %>% filter(!is.na(PercentWhite)) %>% ggplot()  + stat_summary_bin(aes(y = PercentWhite, x = fitted_values),fun='mean',bins = 20,geom = "point",color="red")  + ylab("PercentWhite") + xlab("Prop. Brach Est. Open") + labs(title ="PercentWhite vs. Prop. Branch Est. Open (fit)") + ylim(0,1)
ggsave("plots/iv/bin_IV_PercentWhite_fitted.png",plot4)

plot5 = data_nb %>% filter(!is.na(PercentAsian)) %>% ggplot()  + stat_summary_bin(aes(y = PercentAsian, x = fitted_values),fun='mean',bins = 20,geom = "point",color="red")  + ylab("PercentAsian") + xlab("Prop. Brach Est. Open") + labs(title ="PercentAsian vs. Prop. Branch Est. Open (fit)") + ylim(0,1)
ggsave("plots/iv/bin_IV_PercentAsian_fitted.png",plot5)


plot1 = data_nb %>% filter(!is.na(meanI)) %>% ggplot()  + stat_summary_bin(aes(y = meanI, x = BrandPostalProp),fun='mean',bins = 20,geom = "point",color="blue")  + xlab("Brand Exposure") + ylab("Mean Income") + labs(title ="Mean Income vs. Brand Exposure") + ylim(0,max(data_nb$meanI,na.rm=T))
ggsave("plots/iv/bin_BrandPostal_income.png",plot1)

plot3 = data_nb %>% filter(!is.na(PercentWhite)) %>% ggplot()  + stat_summary_bin(aes(y = PercentWhite, x = BrandPostalProp),fun='mean',bins = 20,geom = "point",color="blue")  + ylab("PercentWhite") + xlab("Brand Exposure") + labs(title ="PercentWhite vs. Brand Exposure") + ylim(0,1)
ggsave("plots/iv/bin_BrandPostal_PercentWhite.png",plot3)



# 16th July
## Looking at changes in IV
naics_codes = fread("Naics_2017.csv") %>% select(Seq,naics_code,naics_name) 
naics_codes$naics_code = as.integer(naics_codes$naics_code)

pincodes = read.csv("uszips.csv")
pincodes = pincodes %>% select(zip,state_name,state_id) 

data_nb = fread("data_nb.csv")
data_nb$date = ymd(data_nb$date)
data_nb = data_nb %>% mutate(difference = proption_BigBrands_naics_postal_open - BrandPostalProp)



postal_naics_difference = data_nb %>% group_by(postal_code,naics_code,date) %>% summarise(r2 = sqrt(sum(difference^2))/n())
postal_naics_difference = left_join(postal_naics_difference,naics_codes)
#naics_difference$naics_code = as.factor(naics_difference$naics_code)

postal_naics_difference = postal_naics_difference %>% group_by(postal_code,naics_code) %>% mutate(avg_r2 = mean(r2)) %>% arrange(desc(avg_r2)) %>% ungroup()
postal_difference_30day_avg = postal_naics_difference %>% group_by(postal_code) %>% summarise(avg_r2 = mean(r2),var_r2=sd(r2)) %>% arrange(desc(avg_r2))
postal_difference_30day_avg = left_join(postal_difference_30day_avg,pincodes,by = c("postal_code" = "zip"))

postal_naics_difference = left_join(postal_naics_difference,pincodes,by = c("postal_code" = "zip"))
state_naics_difference = postal_naics_difference %>% group_by(naics_code,state_name) %>% summarise(avg_r2 = mean(r2),var_r2=sd(r2)) %>% arrange(desc(avg_r2))

state_naics_difference$naics_code = as.factor(state_naics_difference$naics_code)  
state_naics_plt = ggplot(state_naics_difference) + geom_line(aes(x=naics_code,y=avg_r2,group = state_name,color = state_name))
ggsave("plots/iv/state_naics.png",state_naics_plt,width=15,height=8)

####
p1 =  naics_difference %>% ggplot() + geom_line(aes(x=date,y=r2,group = naics_name,color = naics_name))
ggsave("plots/iv/naics_IV.png",p1,width=15,height=8)

p3 =  naics_difference %>% top_n(5*46) %>% ggplot() + geom_line(aes(x=date,y=r2,group = naics_name,color = naics_name))
ggsave("plots/iv/naics_IV_top.png",p3,width=10,height=8)

p2 =  naics_difference %>% top_n(-5*46)%>% ggplot() + geom_line(aes(x=date,y=r2,group = naics_name,color = naics_name))
ggsave("plots/iv/naics_IV_bottom.png",p2,width=10,height=8)


## NAICS Only



naics_difference = data_nb %>% group_by(naics_code,date) %>% summarise(r2 = sqrt(sum(difference^2))/n())
naics_difference = left_join(naics_difference,naics_codes)
#naics_difference$naics_code = as.factor(naics_difference$naics_code)

naics_difference = naics_difference %>% group_by(naics_code) %>% mutate(avg_r2 = mean(r2)) %>% arrange(desc(avg_r2)) %>% ungroup()




p1 =  naics_difference %>% ggplot() + geom_line(aes(x=date,y=r2,group = naics_name,color = naics_name))
ggsave("plots/iv/naics_IV.png",p1,width=15,height=8)

p3 =  naics_difference %>% top_n(5*46) %>% ggplot() + geom_line(aes(x=date,y=r2,group = naics_name,color = naics_name))
ggsave("plots/iv/naics_IV_top.png",p3,width=10,height=8)

p2 =  naics_difference %>% top_n(-5*46)%>% ggplot() + geom_line(aes(x=date,y=r2,group = naics_name,color = naics_name))
ggsave("plots/iv/naics_IV_bottom.png",p2,width=10,height=8)

#################





data_nb = fread("filedata/data_nb_state.csv") %>% as.data.frame()  
data_nb$date = ymd(data_nb$date)
    

data_nb$open2 = ifelse(data_nb$visits_by_date/data_nb$feb_daywise_avg>0.2,T,F)
data_nb$open3 = ifelse(data_nb$visits_by_date>5,T,F)

iv_open2 <- data_nb %>% felm(open2 ~  Feb_Avg + prop_home_device |  newfactor + postal_code | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | postal_code,.)

#iv_open1 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device |  newfactor + postal_code | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | postal_code,.)

iv_open3 <- data_nb %>% felm(open3 ~  Feb_Avg + prop_home_device |  newfactor + postal_code | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | postal_code,.)


data_nb['interact'] = data_nb["Feb_Avg"]*data_nb$proption_BigBrands_naics_postal_open
data_nb['interact_expsoure'] = data_nb["Feb_Avg"]*data_nb$BrandPostalProp

data_nb['cutoff'] = cut(data_nb[["pct_same_tract"]]*100,breaks=c(0,33,66,100),labels = c("1","2","3"),include.lowest = T)

dummies = dummy_cols(data_nb$cutoff) %>% select(-1)
names(dummies) = c("Visitors_Low","Visitors_Mid","Visitors_High")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of local visitors * Branch Est. Open
dummies = dummy_cols(data_nb$cutoff) %>% select(-1)
dummies = data_nb$proption_BigBrands_naics_postal_open * dummies
names(dummies) = c("Proportion_Low","Proportion_Mid","Proportion_High")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of local visitors * IV
dummies = dummy_cols(data_nb$cutoff) %>% select(-1)
dummies = data_nb$BrandPostalProp * dummies
names(dummies) = c("Expsoure_Low","Exposure_Mid","Exposure_High")
data_nb = cbind(data_nb,dummies)


model3<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Visitors_Mid + Visitors_High|  newfactor + postal_code  | ( Proportion_Mid + Proportion_High +  proption_BigBrands_naics_postal_open + interact  ~  Exposure_Mid + Exposure_High + BrandPostalProp + interact_expsoure) | postal_code,.)

model3_orig<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Visitors_Mid + Visitors_High|  newfactor + postal_code  | ( Proportion_Mid + Proportion_High +  proption_BigBrands_naics_postal_open  ~  Exposure_Mid + Exposure_High + BrandPostalProp) | postal_code,.)

stargazer(model3_orig,model3,type = "text",covariate.labels=c("Feb Avg","Pct Devices Home","Local Customers(Mid)","Local Customers (high)","Prop Open X Local Customers(Med)","Prop Open X Local Customers(High)"," Prop. Open"," Prop Open X Feb Avg"))




dummies = dummy_cols(data_nb$loyal) %>% select(-1)
names(dummies) = c("NotLoyal","Loyal")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of loyal visitors * Branch Est. Open
dummies = dummy_cols(data_nb$loyal) %>% select(-1)
dummies = data_nb$proption_BigBrands_naics_postal_open * dummies
names(dummies) = c("Proportion_NotLoyal","Proportion_Loyal")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of loyal visitors * IV
dummies = dummy_cols(data_nb$loyal) %>% select(-1)
dummies = data_nb$BrandPostalProp * dummies
names(dummies) = c("Expsoure_NotLoyal","Exposure_Loyal")
data_nb = cbind(data_nb,dummies)

model2_orig<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Loyal |  newfactor + postal_code  | ( Proportion_Loyal +  proption_BigBrands_naics_postal_open  ~  Exposure_Loyal + BrandPostalProp) | postal_code,.)

model2<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Loyal |  newfactor + postal_code  | ( Proportion_Loyal +  proption_BigBrands_naics_postal_open + interact  ~  Exposure_Loyal + BrandPostalProp + interact_expsoure) | postal_code,.)

stargazer(model2_orig,model2, type = "text",covariate.labels=c("Feb Avg","Pct Devices Home","Loyal Customers","Prop Open X Loyal Customers","Prop. Open","Prop Open X Feb Avg"))


### Running for Above Median(Feb_Avg) and Below Median
data_nb["above_median_FebAvg"] = ifelse(data_nb$Feb_Avg>median(data_nb$Feb_Avg),T,F)

dummies = dummy_cols(data_nb$above_median_FebAvg) %>% select(-1)
names(dummies) = c("Above_FebMed","Below_FebMed")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of local visitors * Branch Est. Open
dummies = dummy_cols(data_nb$above_median_FebAvg) %>% select(-1)
dummies = data_nb$proption_BigBrands_naics_postal_open * dummies
names(dummies) = c("Proportion_Above_FebMed","Proportion_Below_FebMed")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of local visitors * IV
dummies = dummy_cols(data_nb$above_median_FebAvg) %>% select(-1)
dummies = data_nb$BrandPostalProp * dummies
names(dummies) = c("Expsoure_Above_FebMed","Exposure_Below_FebMed")
data_nb = cbind(data_nb,dummies)



data_nb['cutoff'] = cut(data_nb[["pct_same_tract"]]*100,breaks=c(0,33,66,100),labels = c("1","2","3"),include.lowest = T)

dummies = dummy_cols(data_nb$cutoff) %>% select(-1)
names(dummies) = c("Visitors_Low","Visitors_Mid","Visitors_High")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of local visitors * Branch Est. Open
dummies = dummy_cols(data_nb$cutoff) %>% select(-1)
dummies = data_nb$proption_BigBrands_naics_postal_open * dummies
names(dummies) = c("Proportion_Low","Proportion_Mid","Proportion_High")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of local visitors * IV
dummies = dummy_cols(data_nb$cutoff) %>% select(-1)
dummies = data_nb$BrandPostalProp * dummies
names(dummies) = c("Expsoure_Low","Exposure_Mid","Exposure_High")
data_nb = cbind(data_nb,dummies)


model3_orig<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Visitors_Mid + Visitors_High|  newfactor + postal_code  | ( Proportion_Mid + Proportion_High +  proption_BigBrands_naics_postal_open  ~  Exposure_Mid + Exposure_High + BrandPostalProp) | postal_code,.)

model3<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Visitors_Mid + Visitors_High + Above_FebMed |  newfactor + postal_code  | ( Proportion_Mid + Proportion_High +  proption_BigBrands_naics_postal_open + Proportion_Above_FebMed  ~  Exposure_Mid + Exposure_High + BrandPostalProp + Expsoure_Above_FebMed) | postal_code,.)

stargazer(model3_orig,model3,type = "text",covariate.labels=c("Feb Avg","Pct Devices Home","Local Customers(Mid)","Local Customers (high)", "Above Feb Median","Prop Open X Local Customers(Med)","Prop Open X Local Customers(High)"," Prop. Open"," Prop Open X Above Feb Median"))




dummies = dummy_cols(data_nb$loyal) %>% select(-1)
names(dummies) = c("NotLoyal","Loyal")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of loyal visitors * Branch Est. Open
dummies = dummy_cols(data_nb$loyal) %>% select(-1)
dummies = data_nb$proption_BigBrands_naics_postal_open * dummies
names(dummies) = c("Proportion_NotLoyal","Proportion_Loyal")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of loyal visitors * IV
dummies = dummy_cols(data_nb$loyal) %>% select(-1)
dummies = data_nb$BrandPostalProp * dummies
names(dummies) = c("Expsoure_NotLoyal","Exposure_Loyal")
data_nb = cbind(data_nb,dummies)

model2_orig<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Loyal |  newfactor + postal_code  | ( Proportion_Loyal +  proption_BigBrands_naics_postal_open  ~  Exposure_Loyal + BrandPostalProp) | postal_code,.)

model2<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Loyal + + Above_FebMed |  newfactor + postal_code  | ( Proportion_Loyal +  proption_BigBrands_naics_postal_open + Proportion_Above_FebMed ~  Exposure_Loyal + BrandPostalProp  + Expsoure_Above_FebMed) | postal_code,.)

stargazer(model2_orig,model2, type = "text",covariate.labels=c("Feb Avg","Pct Devices Home","Loyal Customers","Above Feb Median","Prop Open X Loyal Customers","Prop. Open","Prop Open X Above Feb Med"))


######## ################ ####################### ##########################

##################






### #### #### Open-Brand Analysis ### ### ####
data = fread("preRegData.csv")

data_bb = data %>% filter(big_brands)

data_bb$date = ymd(data_bb$date)
pincodes = read.csv("uszips.csv")
pincodes = pincodes %>% select(zip,state_name,state_id) 
names(pincodes)[names(pincodes)=="zip"] = "postal_code"

data_bb = left_join(data_bb,pincodes)

bb_daily_open = data_bb %>% group_by(brands,date) %>% summarise(prop_open_national = sum(open)/n()) %>% filter(wday(date)!=1 | wday(date)!=7)
bb_daily_open = bb_daily_open %>% group_by(brands) %>% mutate(prop_change_national = prop_open_national - lag(prop_open_national,order_by=date)) %>% drop_na()




### Other COde #####

plot1 = ggplot(bb_daily_open) + geom_histogram(aes(x=prop_change_national))
ggsave("plots/plot_change.png")

plot2 = ggplot(bb_daily_open) + geom_histogram(aes(x=prop_open_national))
ggsave("plots/plot_open.png",plot2)



#### Difference in states in estimates #####

bb_daily_open_state = data_bb %>% group_by(brands,date,state_name) %>% summarise(prop_open = sum(open)/n())
bb_daily_open_state = bb_daily_open_state %>% group_by(brands,state_name) %>% mutate(prop_change = prop_open - lag(prop_open,order_by=date))

bb_daily_open_state = bb_daily_open_state %>% drop_na()

bb_daily_open_state = left_join(bb_daily_open_state,bb_daily_open)

est1 <- felm(prop_change ~ prop_change_national + prop_open_national:state_name | brands, bb_daily_open_state)
est2 <- felm(prop_change ~ prop_change_national + state_name | brands, bb_daily_open_state)


##  ## ## For community Establishments ## ## ##
data_nb = fread("data_nb.csv") %>% as.data.frame()
data_nb$date = ymd(data_nb$date)

nb_daily_open = data_nb %>% group_by(date) %>% summarise(prop_open_national = sum(open)/n())
nb_daily_open = nb_daily_open %>% mutate(prop_change_national = prop_open_national - lag(prop_open_national,order_by=date))

plot1 = ggplot(nb_daily_open) + geom_histogram(aes(x=prop_change_national))
ggsave("plots/plot_change_nb.png")

plot2 = ggplot(nb_daily_open) + geom_histogram(aes(x=prop_open_national))
ggsave("plots/plot_open_nb.png",plot2)

data_nb = left_join(data_nb,pincodes)

nb_daily_open_state = data_nb %>% group_by(date,state_name) %>% summarise(prop_open = sum(open)/n())
nb_daily_open_state = nb_daily_open_state %>% group_by(state_name) %>% mutate(prop_change = prop_open - lag(prop_open,order_by=date))

nb_daily_open_state = nb_daily_open_state %>% drop_na()

nb_daily_open_state = left_join(nb_daily_open_state,nb_daily_open)


est1 <- felm(prop_change ~ prop_change_national + prop_open_national:state_name, nb_daily_open_state)
est2 <- felm(prop_change ~ prop_change_national + state_name, nb_daily_open_state)


### #### #### Brand National Analysis ### ### ####

data_bl = data %>% filter(!big_brands)

data_bl$date = ymd(data_bl$date)
pincodes = read.csv("uszips.csv")
pincodes = pincodes %>% select(zip,state_name,state_id) 
names(pincodes)[names(pincodes)=="zip"] = "postal_code"

data_bl = left_join(data_bl,pincodes)

bl_daily_open = data_bl %>% group_by(brands,date) %>% summarise(prop_open_national = sum(open)/n())
bl_daily_open = bl_daily_open %>% group_by(brands) %>% mutate(prop_change_national = prop_open_national - lag(prop_open_national,order_by=date))

plot1 = ggplot(bl_daily_open) + geom_histogram(aes(x=prop_change_national))
ggsave("plots/plot_change_bl.png")

plot2 = ggplot(bl_daily_open) + geom_histogram(aes(x=prop_open_national))
ggsave("plots/plot_open_bl.png",plot2)


bl_daily_open_state = data_bl %>% group_by(brands,date,state_name) %>% summarise(prop_open = sum(open)/n())
bl_daily_open_state = bl_daily_open_state %>% group_by(brands,state_name) %>% mutate(prop_change = prop_open - lag(prop_open,order_by=date))

bl_daily_open_state = bl_daily_open_state %>% drop_na()

bl_daily_open_state = left_join(bl_daily_open_state,bl_daily_open)

est1_bl <- felm(prop_change ~ prop_change_national + prop_open_national:state_name | brands, bl_daily_open_state)
est2_bl <- felm(prop_change ~ prop_change_national + state_name | brands, bl_daily_open_state)

stargazer(est1,est1_bl,type = "text")
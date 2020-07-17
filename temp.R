
local_iv_means = data_nb %>% group_by(postal_code) %>% summarise(prop_local = mean(proption_BigBrands_naics_postal_open),prop_fitted = mean(fitted_values))

plot1 = data_nb %>% filter(!is.na(meanI)) %>% ggplot()  + stat_summary_bin(aes(x = meanI, y = proption_BigBrands_naics_postal_open),fun='mean',bins = 20,geom = "point",color="blue") + stat_summary_bin(aes(x = meanI, y = fitted_values),fun='mean',bins = 20,geom = "point",color="red") + xlab("Mean Income") + ylab("Prop. Brach Est. Open") + labs(title ="Prop. Branch Est. Open vs. Mean Income") 
ggsave("plots/iv/bin_IV_income.png",plot1)

plot2 = data_nb %>% filter(!is.na(PercentWhite)) %>% ggplot()  + stat_summary_bin(aes(x = PercentWhite, y = proption_BigBrands_naics_postal_open),fun='mean',bins = 20,geom = "point",color="blue") + stat_summary_bin(aes(x = PercentWhite, y = fitted_values),fun='mean',bins = 20,geom = "point",color="red") + xlab("PercentWhite") + ylab("Prop. Brach Est. Open") + labs(title ="Prop Branch Est. Open vs. PercentWhite")
ggsave("plots/iv/bin_IV_PercentWhite.png",plot2)

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





data_nb = fread("data_nb.csv") %>% as.data.frame()  
data_nb$date = ymd(data_nb$date)
    

data_nb$open2 = ifelse(data_nb$visits_by_date/data_nb$feb_daywise_avg>0.2,T,F)
data_nb$open3 = ifelse(data_nb$visits_by_date>5,T,F)

iv_open2 <- data_nb %>% felm(open2 ~  Feb_Avg + prop_home_device |  newfactor + postal_code | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | postal_code,.)

iv_open1 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device |  newfactor + postal_code | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | postal_code,.)

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
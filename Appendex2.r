

data_nb = fread("filedata/data_nb_state.csv") %>% as.data.frame()  
data_nb$date = ymd(data_nb$date)


#############################################
############# Continuous Models #############
#############################################



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



#############################################
############# Discrete Models ###############
#############################################


### Running for Above Median(Feb_Avg) and Below Median
data_nb["above_median_FebAvg"] = ifelse(data_nb$Feb_Avg>median(data_nb$Feb_Avg),T,F)

data_nb = data_nb %>% group_by(naics_code) %>% mutate(above_median_FebAvg = ifelse(Feb_Avg>median(Feb_Avg),T,F)) %>% ungroup()

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

model2<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Loyal + Above_FebMed |  newfactor + postal_code  | ( Proportion_Loyal +  proption_BigBrands_naics_postal_open + Proportion_Above_FebMed ~  Exposure_Loyal + BrandPostalProp  + Expsoure_Above_FebMed) | postal_code,.)

stargazer(model2_orig,model2, type = "text",covariate.labels=c("Feb Avg","Pct Devices Home","Loyal Customers","Above Feb Median","Prop Open X Loyal Customers","Prop. Open","Prop Open X Above Feb Med"))

screenreg(list(model3,model2),digits=3,caption = "Regression",caption.above = T, custom.model.names =  c("Model 12","Model 13"),custom.coef.names = c("Feb_Avg","prop_home_device_zip","Local Visitors (Med)","Local Visitors (High)","Avg. February Visitors(High)","Prop. Branch Est. Open XX Local Visitors (Med)","Prop. Branch Est. Open XX Local Visitors (High)","Prop. Branch Est. Open","Prop. Branch Est. Open XX Avg. February Visitors(High)","Loyal Customers","Prop. Branch Est. Open XX Loyal Customers"),reorder.coef=c(8,6,7,11,3,4,10,1,2,5,9),custom.gof.rows=list("Fixed Effect Date-NAICS"=c("Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes")),include.fstatistic = T,table=F,custom.note = paste("\\item %stars. Note"))

texreg(list(model3,model2),digits=3,file="tables/appendix2.tex",caption = "Regression",caption.above = T, custom.model.names =  c("Model 12","Model 13"),custom.coef.names = c("Feb_Avg","prop_home_device_zip","Local Visitors (Med)","Local Visitors (High)","Avg. February Visitors(High)","Prop. Branch Est. Open XX Local Visitors (Med)","Prop. Branch Est. Open XX Local Visitors (High)","Prop. Branch Est. Open","Prop. Branch Est. Open XX Avg. February Visitors(High)","Loyal Customers","Prop. Branch Est. Open XX Loyal Customers"),reorder.coef=c(8,6,7,11,3,4,10,1,2,5,9),custom.gof.rows=list("Fixed Effect Date-NAICS"=c("Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes")),include.fstatistic = T,table=F,custom.note = paste("\\item %stars. Note"))


source(file = "code/fix_names.r")
fix_names("tables/appendix2.tex")
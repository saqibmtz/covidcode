## Event study

#### Using alternate definition of closing. A brand establishment in NAICS-Zip closed for three days or more

data = fread("filedata/preRegData_state.csv")
data_bb = data %>% filter(big_brands)
data_bb$date = ymd(data_bb$date)
rm(data)

data_bb = data_bb %>% filter(wday(date)!=1 & wday(date)!=7)
data_bb = data_bb %>% group_by(safegraph_place_id) %>% mutate(closingStatus = ifelse(!open & !lead(open,1,ordery_by = date) & !lead(open,2,ordery_by = date), T, NA), closingStatus_1day = ifelse(!open,T,NA))

#Display Data
#data_bb %>% filter(safegraph_place_id == "sg:03bc8f3dfdf64cffa956303dd1ab923a") %>% select(date, open, closingStatus) %>% as.data.frame() 

data_bb = data_bb %>% group_by(safegraph_place_id) %>% arrange(date) %>% fill(closingStatus)
data_bb = data_bb %>% group_by(safegraph_place_id) %>% arrange(date) %>% fill(closingStatus_1day)

data_bb$closingStatus[is.na(data_bb$closingStatus)] = F
data_bb$closingStatus_1day[is.na(data_bb$closingStatus_1day)] = F

### Aggregating at zip-naics-date level
data_bb_agg = data_bb %>% group_by(postal_code,naics_code,date) %>% summarise(closedStatusBrand = ifelse(sum(closingStatus)>0,T,F),closedStatusBrand_1day = ifelse(sum(closingStatus_1day)>0,T,F))

##Displaying the data
#data_bb_agg %>% arrange(postal_code,naics_code,date) %>% select(naics_code,closedStatusBrand) %>% head(.,40) %>% as.data.frame()

## Plot
#p1 = data_bb_agg %>% group_by(naics_code,date) %>% summarise(percent_closed = sum(closedStatusBrand)/n())  %>% ggplot(.) + geom_line(aes(x = date, y = percent_closed, color = naics_code, group = naics_code))
#ggsave("plots/eventstudy/naics_plot_3day.jpg",p1)

#p1 <- data_bb_agg %>% ggplot(.) + geom_smooth(aes(x = date, y = as.integer(closedStatusBrand)),se=T) + ylab("Percent Unit in Treatment") +xlab("Date")
#ggsave("plots/eventstudy/treatmentStatus_3days.jpg",p1)

## List of NAICS for end-date
#data_bb_agg %>% group_by(naics_code,date) %>% summarise(percent_closed = sum(closedStatusBrand)/n()) %>% filter(date == end_date) %>% as.data.frame() %>% arrange(percent_closed) %>% left_join(naics_code)

### Reading community Est. Data and Merging
data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)

data_nb = data_nb %>% filter(wday(date)!=1 & wday(date)!=7)
data_nb = data_nb %>% group_by(safegraph_place_id) %>% mutate(closingStatusCommunity = ifelse(!open & !lead(open,1,ordery_by = date) & !lead(open,2,ordery_by = date), T, NA), closingStatusCommunity_1day = ifelse(!open,T,NA))
data_nb = data_nb %>% group_by(safegraph_place_id) %>% arrange(date) %>% fill(closingStatusCommunity)
data_nb = data_nb %>% group_by(safegraph_place_id) %>% arrange(date) %>% fill(closingStatusCommunity_1day)

data_nb$closingStatusCommunity[is.na(data_nb$closingStatusCommunity)] = F
data_nb$closingStatusCommunity_1day[is.na(data_nb$closingStatusCommunity_1day)] = F

data_nb_agg = data_nb %>% group_by(naics_code,postal_code,date,countyName) %>% summarise(percent_closed_community = sum(closingStatusCommunity)/n(),percent_closed_community_1day = sum(closingStatusCommunity_1day)/n())


data_nb_agg = left_join(data_nb_agg,data_bb_agg)

fwrite(data_nb_agg, "filedata/EventStudyData_3days.csv")

data_nb_agg = fread("filedata/EventStudyData_3days.csv")
data_nb_agg$date = ymd(data_nb_agg$date)

## Running Regression
data_nb_agg$post = (data_nb_agg$closedStatusBrand_ ==T)
data_nb_agg = data_nb_agg %>% group_by(naics_code,postal_code) %>% mutate(change_date = ifelse(closedStatusBrand & !lag(closedStatusBrand,1,order_by = date),T,F))
temp = data_nb_agg %>% group_by(naics_code,postal_code) %>% filter(change_date) %>% summarise(treated_date = min(date))

data_nb_agg = left_join(data_nb_agg,temp)
#data_nb_agg$treated_date[is.na(data_nb_agg$treated_date)] = ymd(end_date + years(1))

data_nb_agg = data_nb_agg %>% mutate(lag = date - treated_date)
data_nb_agg$lag[is.na(data_nb_agg$lag)] = 0
data_nb_agg$lag[data_nb_agg$lag > 14] = 14
data_nb_agg$lag[data_nb_agg$lag < -14] = -14
data_nb_agg$lag2 = data_nb_agg$lag

data_nb_agg = data_nb_agg %>% mutate(DateNaics = paste(date,naics_code), DateZip = paste(date,postal_code), DateCounty = paste(date,countyName))
data_nb_agg$lag = relevel(factor(data_nb_agg$lag),ref = "-1")

temp = data_nb %>% select(date,postal_code, naics_code, prop_home_device_zip) %>% distinct(date,postal_code, naics_code, prop_home_device_zip)
data_nb_agg = left_join(data_nb_agg,temp)
#data_nb_agg = data_nb_agg %>% mutate(DateNaics = paste(date, naics_code),DateZip = paste(date, postal_code))

#model1 = data_nb_agg %>% felm(percent_closed_community ~ factor(lag),.)
#model2 = data_nb_agg %>% felm(percent_closed_community ~ factor(lag) | date + postal_code + naics_code,.)
#model3 = data_nb_agg %>% felm(percent_closed_community ~ factor(lag)| date + postal_code + DateNaics,.)
#model4 = data_nb_agg %>% felm(percent_closed_community ~ factor(lag) | DateNaics + DateZip,.)

model5 = data_nb_agg %>% felm(percent_closed_community ~ lag | DateNaics + DateCounty,.)
model6 = data_nb_agg %>% felm(percent_closed_community ~ lag  + prop_home_device_zip | DateNaics + DateCounty,.)

screenreg(list(model5, model6, digits = 3))

#stargazer(model1, model2, model3, model4, type = "text",covariate.labels = c("Treatment (BrandClosedStatus) "))

plot_results = function(model, outputLocation){
    coef <- tidy(model5, conf.int = TRUE) %>% mutate(term = extract_numeric(term))
    coef = coef %>% mutate(grp = ifelse(as.integer(coef$term) <0,T,F))
    p1 = ggplot(coef,aes(term,estimate,group = grp)) + geom_point() + geom_line(color = "darkblue") + geom_line(aes(term,conf.low),linetype = "dashed",color = "blue") + geom_line(aes(term,conf.high),linetype = "dashed",color = "blue") + theme_bw() + xlab("Lag") + ylab("Coefficient")
    ggsave(outputLocation,p1)
}

plot_results(model5,"plots/eventstudynew/laggedPlot5_relevel_1.jpg")


p3 = tidy(model5,conf.int=T) %>% mutate(term = extract_numeric(term)) %>% ggplot(.,aes(x=term,y=estimate, ymin = conf.low, ymax = conf.high)) + geom_crossbar(fill = "#D55E00", color = "#D55E00", alpha = 1,width=.3) + geom_point(size = 1) + xlab("Days to/Since Brand Est. Closing") + ylab("Coefficient") + ylim(-.05,.05) + geom_vline(xintercept = -1,linetype = "dashed",size = 0.3) + geom_hline(yintercept = 0,size = 0.5)  + theme_bw() + geom_point(aes(x=-1,y=0), size = 1)
ggsave("plots/eventstudynew/temp.jpg",p3,width  = 7, height = 5)
ggsave("plots/eventstudynew/temp2.jpg",p1,width  = 8, height = 5)

p2 = dwplot(tidy(model2)) + theme_bw() + ylab("Coefficient Estimate") + xlab("Days Lead Treatment") + ggtitle("Effect of Treatment (3 days) on Prop. Of Local Store Closing") 
ggsave("plots/eventstudynew/laggedPlot_3days_Zip_DateNaics.jpg",p2)

###########################################################
######## Generating Event study table  ###################
###########################################################


model6 = data_nb_agg %>% felm(percent_closed_community ~ post  + prop_home_device_zip | DateNaics + DateCounty,.)

model5 = data_nb_agg %>% felm(percent_closed_community ~ post | DateNaics + DateCounty,.)
stargazer(model5, model6, type = "text")


data_nb_agg = data_nb_agg %>% filter(abs(lag2)<=7)

model7 = data_nb_agg %>% felm(percent_closed_community ~ lag | DateNaics + DateCounty,.)
model8 = data_nb_agg %>% felm(percent_closed_community ~ lag  + prop_home_device_zip | DateNaics + DateCounty,.)


screenreg(list(model5,model6,model7, model8),digits = 3, custom.header = list("Community Est. Closing" = 1:4),custom.model.names = c("Model 17", "Model 18","Model 19", "Model 20"),custom.gof.rows=list("Controls" = c("No","Yes","No","Yes"), "Fixed Effect Naics x Date "=c("Yes","Yes","Yes","Yes"), "Fixed Effect County x Date" = c("Yes","Yes","Yes","Yes")),table=F)

texreg(list(model5,model6,model7, model8),digits = 3, file = "tables/EventStudy.tex", custom.header = list("Community Est. Closing" = 1:4),custom.model.names = c("Model 17", "Model 18","Model 19", "Model 20"),custom.gof.rows=list("Controls" = c("No","Yes","No","Yes"), "Fixed Effect Naics x Date "=c("Yes","Yes","Yes","Yes"), "Fixed Effect County x Date" = c("Yes","Yes","Yes","Yes")),table=F)

fix_names("tables/EventStudy.tex")
fix_names("tables/EventStudy2.tex")
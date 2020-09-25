## Event study

data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)

date_naics_counts = data_nb %>% group_by(date,naics_code,postal_code) %>% distinct(proption_BigBrands_naics_postal_open) %>% group_by(date,naics_code) %>% summarise(naics_open = sum(proption_BigBrands_naics_postal_open==1))
data_naics_prescence = data_nb %>% filter(date == begin_date) %>% group_by(naics_code,postal_code) %>% summarise(totalStores = n()) %>% group_by(naics_code) %>% summarise(totalPrescence = sum(totalStores>0))

date_naics_counts = left_join(date_naics_counts,data_naics_prescence)
date_naics_counts = date_naics_counts %>% mutate(percent_open = naics_open/totalPrescence)


## Exploratory analysis of data

p1 = date_naics_counts %>% ggplot(.) + geom_line(aes(x = date, y = percent_open, color = naics_open, group = naics_code))
ggsave("plots/eventstudy/naics_plot.jpg",p1)

#### Taking out weekdays
date_naics_counts = date_naics_counts %>% filter(wday(date)!=1 & wday(date)!=7)
p1 = date_naics_counts %>% ggplot(.) + geom_line(aes(x = date, y = percent_open, color = naics_open, group = naics_code))
ggsave("plots/eventstudy/naics_plot_weekday.jpg",p1)


#### Making sense of early distributions March 2-5 
naics_open_begin = date_naics_counts %>% filter(date <= begin_date + days(4)) %>% group_by(naics_code) %>% summarise(open_percent = mean(percent_open))
naics_open_begin = left_join(naics_open_begin,naics_codes) %>% arrange(open_percent)
write.csv(naics_open_begin, "plots/eventstudy/naics_early_open.csv")


## Absorbing State
date_naics_counts = data_nb %>% filter(wday(date)!=1 & wday(date)!=7) %>% group_by(date,naics_code,postal_code) %>% summarise(percent_local_open = sum(open)/n(), current_status = sum(proption_BigBrands_naics_postal_open ==1)/n())
date_naics_counts = date_naics_counts %>% mutate(past_status =NA)
date_naics_counts$past_status = ifelse(date_naics_counts$current_status==0,T,NA)

date_naics_counts = date_naics_counts %>% group_by(naics_code,postal_code) %>% arrange(date) %>% fill(past_status)

##### /Diagnostics
date_naics_counts %>% filter(current_status ==1) %>% arrange(desc(naics_code)) %>% head(10)
date_naics_counts %>% filter(naics_code == 813) %>% filter(postal_code == 13204) %>% as.data.frame() %>% select(current_status,past_status,date)
##### /End Diagnostics


date_naics_counts = date_naics_counts %>% mutate(past_status = replace_na(past_status,F))
temp = cbind(lapply(date_naics_counts, function(x) {length(unique(x))})) %>% as.data.frame()
#### Display the data
stargazer(distinct(as.data.frame(date_naics_counts)), type = "text")
p1 <- date_naics_counts %>% ggplot(.) + geom_smooth(aes(x = date, y = as.integer(past_status)),se=T) + ylab("Percent Unit in Treatment") +xlab("Date")
ggsave("plots/eventstudy/treatmentStatus.jpg",p1)


date_naics_counts %>% arrange(naics_code,date)
date_naics_counts = date_naics_counts %>% mutate(percent_local_closed = 1 - percent_local_open)
date_naics_counts = date_naics_counts %>% mutate(ZipDate = paste(postal_code,date), NaicsDate = paste(naics_code,date))

data_day = data_nb %>% filter(date == begin_date)
data_day = data_day %>% select(naics_code,postal_code,big_brands,LocalStores, BrandStores)

date_naics_counts = left_join(date_naics_counts,data_day)

fwrite(date_naics_counts, "filedata/EventStudyData.csv")
#date_naics_counts = fread("filedata/EventStudyData.csv")





days = -14:14

get_delayed_estimates = function(data_in, days_lagged){
    if(days_lagged<0){
        data_in = data_in %>% group_by(postal_code,naics_code) %>% mutate(past_status_lagged = lag(past_status,-1*days_lagged,ordery_by = date))
    }
    else{
        data_in = data_in %>% group_by(postal_code,naics_code) %>% mutate(past_status_lagged = lead(past_status,1*days_lagged,ordery_by = date))
    }
    totalObs = sum(!is.na(data_in$past_status_lagged))
    model5 = tidy(data_in %>% felm(percent_local_closed ~ past_status_lagged | ZipDate + NaicsDate,.)) %>% mutate(lag = days_lagged, Obs = totalObs)

    return(model5)
}

out = lapply(days, function(x) get_delayed_estimates(date_naics_counts,x))

out2 = out %>% do.call(rbind, .)

out2['model'] = out2$lag
out2$term[out2$term == "past_status_laggedTRUE"] = "LaggedTreatment"

p2 = small_multiple(out2) + theme_bw() + ylab("Coefficient Estimate") + xlab("Days Lagged Treatment") + ggtitle("Effect of Treatment (atleast one brand store closing in NAICS-ZIP) on Prop. Of Local Store Closing")
ggsave("plots/eventstudy/laggedPlot_ZipDate_NaicsDate.jpg",p2)


## Restricting to March where we see most change
date_naics_counts_march = date_naics_counts %>% filter(date<=ymd("2020-03-30"))

out = lapply(days, function(x) get_delayed_estimates(date_naics_counts_march,x))
out2 = out %>% do.call(rbind, .)
out2['model'] = out2$lag
out2$term[out2$term == "past_status_laggedTRUE"] = "LaggedTreatment"
p2 = small_multiple(out2) + theme_bw() + ylab("Coefficient Estimate") + xlab("Days Lagged Treatment") + ggtitle("Effect of Treatment (atleast one brand store closing in NAICS-ZIP) on Prop. Of Local Store Closing")
ggsave("plots/eventstudy/laggedPlot_march.jpg",p2)




#### Using alternate definition of closing. A brand establishment in NAICS-Zip closed for three days or more

data = fread("filedata/preRegData_state.csv")
data_bb = data %>% filter(big_brands)
data_bb$date = ymd(data_bb$date)
rm(data)

data_bb = data_bb %>% filter(wday(date)!=1 & wday(date)!=7)
data_bb = data_bb %>% group_by(safegraph_place_id) %>% mutate(closingStatus = ifelse(!open & !lead(open,1,ordery_by = date) & !lead(open,2,ordery_by = date), T, NA))
#Display Data
data_bb %>% filter(safegraph_place_id == "sg:03bc8f3dfdf64cffa956303dd1ab923a") %>% select(date, open, closingStatus) %>% as.data.frame() 

data_bb = data_bb %>% group_by(safegraph_place_id) %>% arrange(date) %>% fill(closingStatus)

data_bb$closingStatus[is.na(data_bb$closingStatus)] = F

### Aggregating at zip-naics-date level
data_bb_agg = data_bb %>% group_by(postal_code,naics_code,date) %>% summarise(closedStatusBrand = ifelse(sum(closingStatus)>0,T,F))

##Displaying the data
data_bb_agg %>% arrange(postal_code,naics_code,date) %>% select(naics_code,closedStatusBrand) %>% head(.,40) %>% as.data.frame()

## Plot
p1 = data_bb_agg %>% group_by(naics_code,date) %>% summarise(percent_closed = sum(closedStatusBrand)/n())  %>% ggplot(.) + geom_line(aes(x = date, y = percent_closed, color = naics_code, group = naics_code))
ggsave("plots/eventstudy/naics_plot_3day.jpg",p1)

p1 <- data_bb_agg %>% ggplot(.) + geom_smooth(aes(x = date, y = as.integer(closedStatusBrand)),se=T) + ylab("Percent Unit in Treatment") +xlab("Date")
ggsave("plots/eventstudy/treatmentStatus_3days.jpg",p1)

## List of NAICS for end-date
data_bb_agg %>% group_by(naics_code,date) %>% summarise(percent_closed = sum(closedStatusBrand)/n()) %>% filter(date == end_date) %>% as.data.frame() %>% arrange(percent_closed) %>% left_join(naics_codes)

### Reading community Est. Data and Merging
data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)

data_nb = data_nb %>% filter(wday(date)!=1 & wday(date)!=7)

data_nb_agg = data_nb %>% group_by(naics_code,postal_code,date) %>% summarise(percent_closed_community = sum(!open)/n())

data_nb_agg = left_join(data_nb_agg,data_bb_agg)

fwrite(data_nb_agg, "filedata/EventStudyData_3days.csv")

## Running Regression

data_nb_agg = data_nb_agg %>% mutate(DateNaics = paste(date, naics_code),DateZip = paste(date, postal_code))

model1 = data_nb_agg %>% felm(percent_closed_community ~ closedStatusBrand,.)
model2 = data_nb_agg %>% felm(percent_closed_community ~ closedStatusBrand | date + postal_code + naics_code,.)
model3 = data_nb_agg %>% felm(percent_closed_community ~ closedStatusBrand | date + postal_code + DateNaics,.)
model4 = data_nb_agg %>% felm(percent_closed_community ~ closedStatusBrand | DateNaics + DateZip,.)


stargazer(model1, model2, model3, model4, type = "text",covariate.labels = c("Treatment (BrandClosedStatus) "))



### Generating Event study figure


days = -14:14

get_delayed_estimates = function(data_in, days_lagged){
    if(days_lagged<0){
        data_in = data_in %>% group_by(postal_code,naics_code) %>% mutate(closedStatusBrand_lagged = lag(closedStatusBrand,-1*days_lagged, ordery_by = date))
    }
    else{
        data_in = data_in %>% group_by(postal_code,naics_code) %>% mutate(closedStatusBrand_lagged = lead(closedStatusBrand,1*days_lagged,ordery_by = date))
    }
    totalObs = sum(!is.na(data_in$closedStatusBrand_lagged))
    model5 = tidy(data_in %>% felm(percent_closed_community ~ closedStatusBrand_lagged | date + postal_code + DateNaics,.)) %>% mutate(lag = days_lagged, Obs = totalObs)

    return(model5)
}

out = lapply(days, function(x) get_delayed_estimates(data_nb_agg,x))

out2 = out %>% do.call(rbind, .)
out2['model'] = out2$lag
out2$term[out2$term == "closedStatusBrand_laggedTRUE"] = "LaggedTreatment"
p2 = small_multiple(out2) + theme_bw() + ylab("Coefficient Estimate") + xlab("Days Lead Treatment") + ggtitle("Effect of Treatment (3 days) on Prop. Of Local Store Closing")
ggsave("plots/eventstudy/laggedPlot_3days_Zip_DateNaics.jpg",p2)


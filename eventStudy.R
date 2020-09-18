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

#### Display the data
p1 <- date_naics_counts %>% ggplot(.) + geom_smooth(aes(x = date, y = as.integer(past_status)),se=T) + ylab("Percent Unit in Treatment") +xlab("Date")
ggsave("plots/eventstudy/treatmentStatus.jpg",p1)


date_naics_counts %>% arrange(naics_code,date)
date_naics_counts = date_naics_counts %>% mutate(percent_local_closed = 1 - percent_local_open)
date_naics_counts = date_naics_counts %>% mutate(ZipDate = paste(postal_code,date), NaicsDate = paste(naics_code,date))





days = -14:14

get_delayed_estimates = function(data_in, days_lagged){
    if(days_lagged>0){
        data_in = data_in %>% group_by(postal_code,naics_code) %>% mutate(past_status_lagged = lag(past_status,days_lagged,ordery_by = date))
    }
    else{
        data_in = data_in %>% group_by(postal_code,naics_code) %>% mutate(past_status_lagged = lead(past_status,-1*days_lagged,ordery_by = date))
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
ggsave("plots/eventstudy/laggedPlot.jpg",p2)


## Restricting to March where we see most change
date_naics_counts_march = date_naics_counts %>% filter(date<=ymd("2020-03-30"))

out = lapply(days, function(x) get_delayed_estimates(date_naics_counts_march,x))
out2 = out %>% do.call(rbind, .)
out2['model'] = out2$lag
out2$term[out2$term == "past_status_laggedTRUE"] = "LaggedTreatment"
p2 = small_multiple(out2) + theme_bw() + ylab("Coefficient Estimate") + xlab("Days Lagged Treatment") + ggtitle("Effect of Treatment (atleast one brand store closing in NAICS-ZIP) on Prop. Of Local Store Closing")
ggsave("plots/eventstudy/laggedPlot_march.jpg",p2)


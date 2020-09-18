

data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)

days = 1:7

get_delayed_estimates = function(data_in, delay_day){
    data_in = data_in %>% group_by(safegraph_place_id) %>% mutate(open = lead(open,delay_day,ordery_by = date))
    totalObs = sum(!is.na(data_in$open))
    model5 = tidy(data_in %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | newfactor + postal_code + date | 0 |countyName,.)) %>% mutate(delay = delay_day, Obs = totalObs)

    return(model5)
}

out = lapply(days, function(x) get_delayed_estimates(data_nb,x))

#out = get_delayed_estimates(data_nb,1)
out2 = out %>% do.call(rbind, .)

out2['model'] = out2$delay
out2$term[out2$term == "proption_BigBrands_naics_postal_open"] = "Prop. Brand Est. Open"

p1 = dwplot(out2 %>% select(-delay,-Obs)) + coord_flip() + theme_bw() + scale_colour_grey(start = .3, end = .9, name = "Days Lag")
p2 = dwplot(out2 %>% select(-delay,-Obs) %>% filter(term == "Prop. Brand Est. Open")) + coord_flip() + theme_bw() + scale_colour_grey(start = .3, end = .9, name = "Days Lag")


ggsave("plots/delay/delay.jpg",p1)
ggsave("plots/delay/delay2.jpg",p2)

out2 %>% select(-model) %>% filter(term == "Prop. Brand Est. Open")  %>% mutate_if(is.numeric, round, digits = 3) %>% stargazer( type = "text",summary = F,digits.extra=1,digits=1) 
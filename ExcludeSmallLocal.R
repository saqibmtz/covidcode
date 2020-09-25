
data = fread("filedata/preRegData_state.csv")

bbrands = data %>% filter(big_brands) %>% group_by(naics_code) %>% summarise(meanVisitors = mean(Feb_Avg),varianceVisitors = sd(Feb_Avg), medianVisitor = median(Feb_Avg))
rm(data)

data_nb = fread("filedata/data_nb_state.csv")

data_nb = left_join(data_nb, bbrands)
data_nb_large = data_nb %>% filter(Feb_Avg >= medianVisitor)
rm(data_nb)

model5 <- data_nb_large %>% felm(open ~ Feb_Avg + prop_home_device_zip + proption_BigBrands_naics_postal_open | newfactor + postal_code + date | 0 |countyName,.)
stargazer(model5,type = "text")

iv3 <- data_nb_large %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + postal_code + date | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)
stargazer(iv3,type = "text")

stargazer(model5, iv3, type = "text",covariate.labels = c("Avg Feb Traffic","Prop. Devices at home","Prop. Brach Est. Open","Prop. Branch Est. Open (fit)"))
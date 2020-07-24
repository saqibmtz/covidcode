
filein = "filedata/preRegData_state.csv"
data = fread(filein)
data$date = ymd(data$date)
data_day = data %>% filter(date == begin_date)
big_brands_all = unique(data_day$brands[data_day$big_brands]) %>% as.data.frame()

top30 = fread("filedata/brandstop30.csv")
top30$Brand[top30$Brand == "Nissan"] = "Nissan North America"

sum(top30$Brand %in% big_brands_all$.)
top30 = top30 %>% select(Brand) %>% mutate(top30 = T)


BrandNaicsPostal_prop  = fread("filedata/PostalBrandDictState.csv")
BrandNaicsPostal_prop = left_join(BrandNaicsPostal_prop,top30,by = c("brands" = "Brand"))

BrandNaicsPostal_prop$top30 = BrandNaicsPostal_prop$top30 %>% replace_na(F)

### Creating Instrument Variables ###

BrandNaicsPostal_prop = BrandNaicsPostal_prop %>% 
                                group_by(naics_code,postal_code,date) %>%   
                                    summarise(BrandPostalProp_Top30 = sum(proption_naics_national * top30)/sum(top30), 
                                                    BrandPostalProp_NonTop30 = sum(proption_naics_national * (!top30)) / sum(!top30) )
                                    ## ^^Weighted mean at postal-day-naics level 
BrandNaicsPostal_prop$BrandPostalProp_Top30 = ifelse(is.na(BrandNaicsPostal_prop$BrandPostalProp_Top30),0,BrandNaicsPostal_prop$BrandPostalProp_Top30)

BrandNaicsPostal_prop$BrandPostalProp_NonTop30 = ifelse(is.na(BrandNaicsPostal_prop$BrandPostalProp_NonTop30),0,BrandNaicsPostal_prop$BrandPostalProp_NonTop30)

BrandNaicsPostal_prop$date = ymd(BrandNaicsPostal_prop$date)

data = left_join(data,BrandNaicsPostal_prop,by = c("naics_code" = "naics_code" , "postal_code" = "postal_code", "date" = "date"))

## Generating Main Independent Variables
data = left_join(data,top30,by = c("brands" = "Brand"))
data$top30 = data$top30 %>% replace_na(F)

naics_postal= data %>% filter(big_brands) %>% 
                            group_by(naics_code,date,postal_code) %>% 
                                summarise(proption_BigBrands_naics_postal_open_Top30 = sum(open * top30)/ sum(top30),
                                           proption_BigBrands_naics_postal_open_NonTop30 = sum(open * (!top30) )/ sum(!top30))

data = left_join(data,naics_postal)

fwrite(data, "temp/preRegData_state_top30.csv")
#data = fread("temp/preRegData_state_top30.csv")

data$proption_BigBrands_naics_postal_open_Top30 = data$proption_BigBrands_naics_postal_open_Top30 %>% replace_na(0)
data$proption_BigBrands_naics_postal_open_NonTop30 = data$proption_BigBrands_naics_postal_open_NonTop30 %>% replace_na(0)


data_nb = data %>% filter(BrandNational==0)
rm(data)

data_nb = data_nb %>% filter(!is.na(BrandPostalProp))

data_nb['newfactor'] = paste(data_nb$date,data_nb$naics_code)
data_nb$newfactor = ifelse(is.na(data_nb$naics_code),NA,data_nb$newfactor)

model5 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open_Top30 + proption_BigBrands_naics_postal_open_NonTop30 + Feb_Avg + prop_home_device_zip | newfactor + postal_code | 0 |postal_code,.)

stargazer(model5,type = "text")

fs3_top30 <- data_nb %>% felm(proption_BigBrands_naics_postal_open_Top30  ~ BrandPostalProp_Top30 + BrandPostalProp_NonTop30 + Feb_Avg + prop_home_device_zip| newfactor + postal_code | 0 | postal_code,.)

fs3_NonTop30 <- data_nb %>% felm(proption_BigBrands_naics_postal_open_NonTop30  ~ BrandPostalProp_Top30 + BrandPostalProp_NonTop30 + Feb_Avg + prop_home_device_zip| newfactor + postal_code | 0 | postal_code,.)

iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + postal_code | (proption_BigBrands_naics_postal_open_Top30| proption_BigBrands_naics_postal_open_NonTop30 ~ BrandPostalProp_Top30 + BrandPostalProp_NonTop30) | postal_code,.)

iv4 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + postal_code | (proption_BigBrands_naics_postal_open_Top30 ~ BrandPostalProp_Top30 ) | postal_code,.)

iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + postal_code | (proption_BigBrands_naics_postal_open_NonTop30 ~ BrandPostalProp_NonTop30) | postal_code,.)

stargazer(fs3_top30, fs3_NonTop30, type = "text", dep.var.caption = "Prop. Branch Est. Open", column.labels = c("Top 30", "Other"), dep.var.labels   = c("",""),  covariate.labels = c("Brand Exposure Top 30","Brand Exposure Other"," Avg. Feb Visits"," Prop. Devices At Home"))

stargazer(iv3, type = "text",covariate.labels = c(" Avg. Feb Visits"," Prop. Devices At Home","Prop. Branch Est. Open Top 30","Prop. Branch Est. Open Other"))
## Variance between brands in closing decisions

brand_data  = fread("temp/postcovr.csv")
brand_data = brand_data %>% filter(big_brands)

brand_data$date = ymd(brand_data$date)

variances = brand_data %>% group_by(brands,date) %>% summarise(var = sd(open)) %>% group_by(brands) %>% summarise(meanVar = mean(var),varvar = sd(var))
var_30 = variances %>% filter(meanVar <= .30)

BrandNaicsPostal_prop = fread("filedata/PostalBrandDictState.csv")
BrandNaicsPostal_prop$date = ymd(BrandNaicsPostal_prop$date)
BrandNaicsPostal_prop = left_join(BrandNaicsPostal_prop,var_30)
BrandNaicsPostal_prop = BrandNaicsPostal_prop %>% filter(!is.na(meanVar))


brand_data  = fread("temp/postcovr.csv")
brand_data$date = ymd(brand_data$date)

BrandNaicsPostal_prop = BrandNaicsPostal_prop %>% 
                            group_by(naics_code,postal_code,date) %>%   
                                summarise(BrandPostalProp = mean(proption_naics_national))
                                ## ^^Weighted mean at postal-day-naics level 
brand_data = left_join(brand_data,BrandNaicsPostal_prop,by = c("naics_code" = "naics_code" , "postal_code" = "postal_code", "date" = "date"))


naics_postal= brand_data %>% filter(big_brands) %>% 
                        group_by(naics_code,date,postal_code) %>% 
                            summarise(proption_BigBrands_naics_postal_open = sum(open)/n())

brand_data = left_join(brand_data,naics_postal)
rm(naics_postal)

 

## Generate NB

   brandsTot = length(unique(brand_data$brands[brand_data$big_brands]))
    print(paste("Unique Brands: ",brandsTot))

    big_brands_all = unique(brand_data$brands[brand_data$big_brands]) %>% as.data.frame()
    #fwrite(big_brands_all,"bigBrandsList.csv")

    data_nb = brand_data %>% filter(BrandNational==0)

    brand_data = brand_data %>% group_by(naics_code,postal_code) %>% mutate(haveLocal = ifelse(sum(BrandNational==0)>0,T,F))
    brand_data %>% filter(big_brands) %>% filter(haveLocal) %>% nrow()
    rm(brand_data)

    data_nb = data_nb %>% filter(!is.na(BrandPostalProp))

    data_nb['newfactor'] = paste(data_nb$date,data_nb$naics_code)
    data_nb$newfactor = ifelse(is.na(data_nb$naics_code),NA,data_nb$newfactor)

    data_nb['countyDate'] = paste(data_nb$countyName,data_nb$date)

   fwrite(data_nb,"temp/data_nb_state_lowVar.csv")


model5 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip + proption_BigBrands_naics_postal_open | newfactor + postal_code + date ,.)
iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + postal_code + date | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp),.)
stargazer(model5,iv3, type = "text", covariate.labels = c("Avg Feb Traffic","Prop. Devices at home","Prop. Brach Est. Open","Prop. Branch Est. Open (fit)"))

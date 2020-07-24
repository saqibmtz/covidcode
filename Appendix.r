data = fread("filedata/preRegData_state.csv") %>% as.data.frame()
data$open2 = ifelse(data$visits_by_date/data$feb_daywise_avg>0.2,T,F)
data$open3 = ifelse(data$visits_by_date>5,T,F)

data_bb = data %>% filter(big_brands)
data_bb = left_join(data_bb,pincodes,by = c("postal_code" = "zip")) %>% filter(!is.na(state_name))

brand_state_naics_date = data_bb %>% group_by(naics_code,state_name,date,brands) %>% mutate(total_state = n(),state_open2=sum(open2),state_open3=sum(open3))
brand_national_naics_date = data_bb %>% group_by(naics_code,date,brands) %>% summarise(total_national = n(),national_open2 = sum(open2),national_open3 = sum(open3))

brand_state_naics_date = left_join(brand_state_naics_date,brand_national_naics_date)
    
brand_state_naics_date = brand_state_naics_date %>% 
                            mutate(proption_naics_national2 = (national_open2-state_open2)/(total_national-total_state),proption_naics_national3 = (national_open3-state_open3)/(total_national-total_state))
                                
brand_state_naics_date = brand_state_naics_date %>% select(naics_code,postal_code,brands,date,proption_naics_national2,proption_naics_national3)


BrandNaicsPostal_prop = brand_state_naics_date

BrandNaicsPostal_prop = BrandNaicsPostal_prop %>% 
                                group_by(naics_code,postal_code,date) %>%   
                                    summarise(BrandPostalProp2 = mean(proption_naics_national2),BrandPostalProp3 = mean(proption_naics_national3))
                                    ## ^^Weighted mean at postal-day-naics level 
data = left_join(data,BrandNaicsPostal_prop,by = c("naics_code" = "naics_code" , "postal_code" = "postal_code", "date" = "date"))

naics_postal= data %>% filter(big_brands) %>% 
                            group_by(naics_code,date,postal_code) %>% 
                                summarise(proption_BigBrands_naics_postal_open2 = sum(open2)/n(),proption_BigBrands_naics_postal_open3 = sum(open3)/n())

data = left_join(data,naics_postal)
rm(naics_postal)
fwrite(data,"temp/preRegData_open2_open3.csv")

data = fread("temp/preRegData_open2_open3.csv")
data$date = ymd(data$date)

data_nb = data %>% filter(BrandNational==0)
rm(data)

data_nb = data_nb %>% filter(!is.na(BrandPostalProp))

data_nb['newfactor'] = paste(data_nb$date,data_nb$naics_code)
data_nb$newfactor = ifelse(is.na(data_nb$naics_code),NA,data_nb$newfactor)


#iv_open1 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device |  newfactor + postal_code | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | postal_code,.)
fs_open2 <- data_nb %>% felm(proption_BigBrands_naics_postal_open2  ~ BrandPostalProp2 + Feb_Avg + prop_home_device_zip| newfactor + postal_code | 0 | postal_code,.)
iv_open2 <- data_nb %>% felm(open2 ~  Feb_Avg + prop_home_device_zip |  newfactor + postal_code | (proption_BigBrands_naics_postal_open2  ~ BrandPostalProp2) | postal_code,.)

fs_open3 <- data_nb %>% felm(proption_BigBrands_naics_postal_open3  ~ BrandPostalProp3 + Feb_Avg + prop_home_device_zip| newfactor + postal_code | 0 | postal_code,.)
iv_open3 <- data_nb %>% felm(open3 ~  Feb_Avg + prop_home_device_zip |  newfactor + postal_code | (proption_BigBrands_naics_postal_open3  ~ BrandPostalProp3) | postal_code,.)

screenreg(list(fs_open2,iv_open2,fs_open3,iv_open3),digits=3,caption = "Regression",caption.above = T,custom.header = list("Model 10" = 1:2, "Model 11" = 3:4),custom.model.names = c("First Stage","IV","First Stage","IV"),custom.coef.names = c("BrandPostalProp","Feb_Avg","prop_home_device_zip","proption_BigBrands_naics_postal_open","BrandPostalProp","proption_BigBrands_naics_postal_open"),reorder.coef=c(4,1,2,3),custom.gof.rows = list("Fixed Effect Date-NAICS"=c("Yes","Yes","Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes","Yes","Yes")), include.fstatistic = T,table=F)

texreg(list(fs_open2,iv_open2,fs_open3,iv_open3),digits=3,file = "tables/table_appendex_open.tex",caption = "Regression",caption.above = T,custom.header = list("Model 10" = 1:2, "Model 11" = 3:4),custom.model.names = c("First Stage","IV","First Stage","IV"),custom.coef.names = c("BrandPostalProp","Feb_Avg","prop_home_device_zip","proption_BigBrands_naics_postal_open","BrandPostalProp","proption_BigBrands_naics_postal_open"),reorder.coef=c(4,1,2,3),custom.gof.rows = list("Fixed Effect Date-NAICS"=c("Yes","Yes","Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes","Yes","Yes")), include.fstatistic = T,table=F)

source(file = "code/fix_names.r")
fix_names("tables/table_appendex_open.tex")



######################################################
rm(list=ls())

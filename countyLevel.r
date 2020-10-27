data = fread("temp/postcovr.csv")

    ### 2.5 Creating Dictionary for Brand-Date-Postal Openings #####
    #source(file = "code/generate_brand_day_state_dictionary.r")
    #BrandNaicsPostal_prop = generate_brand_day_state_dictionary(data,pincodes)
    #BrandNaicsPostal_prop = fread("filedata/PostalBrandDictState.csv")   # Read the saved version
    
    data_bb = data %>% filter(big_brands)
    data_bb = left_join(data_bb,pincodes,by = c("postal_code" = "zip")) %>% filter(!is.na(state_name))
    
    #temp = data_bb %>% group_by(postal_code) %>% summarise(UniqueCountyNames = length(unique(countyName)), newnames = length(unique(county_name)))

    brand_state_naics_date = data_bb %>% group_by(naics_code,state_name,date,brands) %>% mutate(total_state = n(),state_open=sum(open))
    brand_national_naics_date = data_bb %>% group_by(naics_code,date,brands) %>% summarise(total_national = n(),national_open = sum(open))

    brand_state_naics_date = left_join(brand_state_naics_date,brand_national_naics_date)
    
    brand_state_naics_date = brand_state_naics_date %>% 
                                mutate(proption_naics_national = (national_open-state_open)/(total_national-total_state))
                                
    brand_state_naics_date = brand_state_naics_date %>% select(naics_code,postal_code,countyName,brands,date,proption_naics_national)

    BrandNaicsPostal_prop = brand_state_naics_date

    #### 2.6  Generating Instrument Variable  #######
    BrandNaicsPostal_prop = BrandNaicsPostal_prop %>% 
                                group_by(naics_code,countyName,date) %>%   
                                    summarise(BrandPostalProp = mean(proption_naics_national))
                                    ## ^^Weighted mean at county-day-naics level 
    data = left_join(data,BrandNaicsPostal_prop,by = c("naics_code" = "naics_code" , "countyName" = "countyName", "date" = "date"))


    #### 2.7  Generating Main Indepedent Variable  #####
    naics_postal= data %>% filter(big_brands) %>% 
                            group_by(naics_code,date,countyName) %>% 
                                summarise(proption_BigBrands_naics_postal_open = sum(open)/n())

    data = left_join(data,naics_postal)
    rm(naics_postal)

    fwrite(data, "filedata/preRegData_CountyLevel_state.csv")
    rm(data)



    data_nb = data %>% filter(BrandNational==0)
    data_nb = data_nb %>% filter(!is.na(BrandPostalProp))

    data_nb['newfactor'] = paste(data_nb$date,data_nb$naics_code)
    data_nb$newfactor = ifelse(is.na(data_nb$naics_code),NA,data_nb$newfactor)

    data_nb['countyDate'] = paste(data_nb$countyName,data_nb$date)

    fwrite(data_nb,"filedata/data_nb_CountyLevel_state.csv")




## Without Controls
model1 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open |0|0|countyName,.,keepX=FALSE)
print(paste("Done 1/5"))

## With Controls
model2 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip + postshelter|0|0| countyName,.)
print(paste("Done 2/5"))

## With Controls + FE Naics + FE Postal
model3 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip  + postshelter | naics_code + countyName | 0 |countyName,.)
print(paste("Done 3/5"))

## With Controls + FE Naics + FE Postal + FE Date
model4 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | naics_code + countyName + date | 0 |countyName,.)

## With Controls + FE Naics*Date + FE Postal
model5 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | newfactor + countyName + date | 0 |countyName,.)
print(paste("Done 5/5"))

#screenreg(list(model1,model2,model3,model4,model5))

screenreg(list(model1,model2,model3,model4,model5),caption = "Regression",caption.above = T,custom.header=list("Open"=1:5),digits=3,reorder.coef=c(2,3,4,5,1),custom.gof.rows=list("Fixed Effect NAICS"=c("No","No","Yes","Yes","No"),"Fixed Effect County" = c("No","No","Yes","Yes","Yes"), "Fixed Effect Date"=c("No","No","No","Yes","No"),"Fixed Effect NAICS XX Date"=c("No","No","No","No","Yes"),"Fixed Effect County XX Date"=c("No","No","No","No","No")))


fs2 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip | naics_code + countyName + date | 0 | countyName,.)
iv2 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip | naics_code + countyName + date | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)

print("2/3")

### Controls; With FE ####
fs3 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip| newfactor + countyName + date| 0 | countyName,.)
iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + countyName + date | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)

fs4 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip| newfactor + countyDate| 0 | countyName,.)
iv4 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + countyDate + date | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)

screenreg(list(fs2,iv2,fs3,iv3),digits=3,caption = "Regression",caption.above = T,custom.header = list("Model 6" = 1:2, "Model 7" = 3:4),custom.model.names = c("First Stage","IV","First Stage","IV"),reorder.coef=c(4,1,2,3),custom.gof.rows=list("Fixed Effect NAICS"=c("Yes","Yes","No","No"),"Fixed Effect County" = c("Yes","Yes","Yes","Yes"), "Fixed Effect Date"=c("Yes","Yes","Yes","Yes"),"Fixed Effect NAICS XX Date"=c("No","No","Yes","Yes"),"Fixed Effect County XX Date"=c("No","No","No","No")),table=F)

stargazer(fs4, iv4, type = "text",add.lines = list(c("Fixed Effects: County x Date","Yes","Yes"),c("Fixed Effects NAICS x Date","Yes","Yes")))

print("3/3")


### Zip level 

fs4 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip| newfactor + countyDate| 0 | countyName,.)
iv4 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + countyDate + date | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)


stargazer(fs4, iv4, type = "text",add.lines = list(c("Fixed Effects: County x Date","Yes","Yes"),c("Fixed Effects NAICS x Date","Yes","Yes")))

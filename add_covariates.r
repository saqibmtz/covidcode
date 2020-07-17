## @desc -  Adding useful covriates to data
## @return - A list containing Data object with covriates and a list of identified Big Brands 
add_covriates = function(data,pincodes){
    data['BrandLocal'] = ifelse(data$brands=="",1,0)
    data['BrandNational'] = 1 - data$BrandLocal
    data['brands'] = ifelse(data$brands=="","LOCAL_BRAND",data$brands)

    data$County_Shelter_In_Place_Policy = ymd(data$County_Shelter_In_Place_Policy)
    data$County_Shelter_In_Place_Policy = ifelse(is.na(data$County_Shelter_In_Place_Policy),ymd("2099-01-01" ),data$County_Shelter_In_Place_Policy)
    data['preshelter'] = ifelse(data$date < data$County_Shelter_In_Place_Policy,TRUE,FALSE)
    data['postshelter'] = ifelse(data$date >= data$County_Shelter_In_Place_Policy,TRUE,FALSE)
    data['open'] = !data$closed_or_not

    data = data %>% group_by(safegraph_place_id) %>% mutate(Feb_Avg = mean(feb_daywise_avg))


    #Adding Big Brands
    data_date = data %>% filter(date == begin_date)
    big_brands = get_brand_distribution(data_date,threshold = 50,pincodes,state_threshold = 25,T)
    data['big_brands'] = ifelse(data$brands %in% big_brands$brands, T, F)

    data = data %>% group_by(naics_code,postal_code) %>% mutate(BrandStores = sum(BrandNational)/46,LocalStores = sum(BrandNational==0)/46, BigBrandStores = sum(big_brands)/46)
    bigBrandsInSample = data %>% group_by(naics_code,postal_code) %>% filter(big_brands) %>% summarise(uniqueBigBrands = length(unique(brands)))
    data = left_join(data,bigBrandsInSample)


    ## Adding Loyalty Variable    
    data = data %>% mutate(loyal = ifelse(loyalty==0,T,F))
    
    ## Adding proportion devices at home at zip level and CBG level 
    data = data %>% group_by(postal_code,date) %>% mutate(prop_home_device_zip = sum(completely_home_device_count)/sum(device_count))
    data = data %>% mutate(prop_home_device = completely_home_device_count/device_count)

    return(list(data,big_brands))
}

## @descr - Helper function to identify BigBrands, given a thershold number of stores and state presence
## @param - data_date: An instance of of cross-section of data
##        - threshold: Minimum Number of Stores 
##        - pincodes: data on CBGFIPS to Zips and States
##        - state_threshold: Minimum number of state prescence to qualify as Big Brand
##        - BrandList: Whether the output should be a list of Big Brands or Distribution (used for analysis)     
get_brand_distribution = function(data_date,threshold,pincodes, state_threshold=0, BrandList = F){
  
  big_brands = data_date %>% filter(BrandNational==1) %>% group_by(brands) %>% summarise(count = n()) %>% filter(count>threshold)
  
  data_bb = data_date %>% mutate(BigBrand = ifelse(brands %in% big_brands$brands,T,F))
  
  data_bb = data_bb %>% filter(BigBrand) %>% distinct(safegraph_place_id,.keep_all = T)
  data_bb = data_bb %>% group_by(brands,postal_code) %>% summarise(total_in_pins = n())
  
  data_bb = left_join(data_bb,pincodes,by = c("postal_code" = "zip")) %>% filter(!is.na(state_name))
  #data_bb$state_name = replace_na(data_bb$state_name,"Unknown") 
  #data_bb$state_id = replace_na(data_bb$state_id,"Unknown")
  data_bb = data_bb %>% group_by(brands,state_name,state_id) %>% mutate(total_in_states=n())
  
  #data_bb_states = data_bb %>% group_by(brands,state_name,state_id) %>% summarise(total=n())
  data_bb = data_bb %>% group_by(brands,state_name) %>% mutate(totalState = n())
  
  data_pins = data_bb %>% group_by(brands) %>% distinct(postal_code) %>% summarise(pin_presence = n())
  data_states = data_bb %>% group_by(brands) %>% distinct(state_name) %>% summarise(state_presence = n())
  data_stores = data_bb %>% group_by(brands) %>% summarise(total_stores = n())
  rm(data_bb)
  
  if(BrandList){
    data_out = data_states %>% filter(state_presence>=state_threshold)
    return(data_out)
  }
  else{
    data_out = left_join(data_pins,data_states)
    data_out = left_join(data_out,data_stores)
    data_out["threshold"] = threshold
    return(data_out)
  }
}

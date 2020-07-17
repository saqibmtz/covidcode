### 
# @desc - Get all brand stores and add proportion open nationally (excluding the brand stores in the same state)  for each brand
# @param - dataset
# @return - Brand Store-Day-ProportionOpen Dictionary like object

generate_brand_day_state_dictionary = function(data,pincodes){
    data_bb = data %>% filter(big_brands)
    data_bb = left_join(data_bb,pincodes,by = c("postal_code" = "zip")) %>% filter(!is.na(state_name))
    
    brand_state_naics_date = data_bb %>% group_by(naics_code,state_name,date,brands) %>% mutate(total_state = n(),state_open=sum(open))
    brand_national_naics_date = data_bb %>% group_by(naics_code,date,brands) %>% summarise(total_national = n(),national_open = sum(open))

    brand_state_naics_date = left_join(brand_state_naics_date,brand_national_naics_date)
    
    brand_state_naics_date = brand_state_naics_date %>% 
                                mutate(proption_naics_national = (national_open-state_open)/(total_national-total_state))
                                
    brand_state_naics_date = brand_state_naics_date %>% select(naics_code,postal_code,brands,date,proption_naics_national)

    fwrite(brand_state_naics_date,"filedata/PostalBrandDictState.csv")
    return(brand_state_naics_date)
}
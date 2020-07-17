

### 
# @desc - Get all brand stores and add proportion open nationally for each brand
# @param - dataset
# @return - Brand Store-Day-ProportionOpen Dictionary like object
generate_brand_day_dictionary = function(data){
    PostalBrandDict = data %>% filter(BrandNational==1) %>% select(naics_code,postal_code,brands,date)
    BrandPropotionOpen= data %>% filter(BrandNational==1) %>% group_by(naics_code,date,brands) %>% summarise(proption_naics_national = sum(open)/n())

    PostalBrandDict = left_join(PostalBrandDict,BrandPropotionOpen)
    fwrite(PostalBrandDict,"PostalBrandDict.csv")

    print("Created Dictionary")

    return(PostalBrandDict)
    #PostalBrandDict = fread("PostalBrandDict.csv")
}


generate_brand_day_state_dictionary = function(data,pincodes){
    data_bb = data %>% filter(big_brands)
    data_bb = left_join(data_bb,pincodes,by = c("postal_code" = "zip")) %>% filter(!is.na(state_name))
    
    brand_state_naics_date = data_bb %>% group_by(naics_code,state_name,date,brands) %>% mutate(total_state = n(),state_open=sum(open))
    brand_national_naics_date = data_bb %>% group_by(naics_code,date,brands) %>% summarise(total_national = n(),national_open = sum(open))

    brand_state_naics_date = left_join(brand_state_naics_date,brand_national_naics_date)
    
    brand_state_naics_date = brand_state_naics_date %>% mutate(proption_naics_national = (national_open-state_open)/(total_national-total_state))
    brand_state_naics_date = brand_state_naics_date %>% select(naics_code,postal_code,brands,date,proption_naics_national)

    #fwrite(brand_state_naics_date,"PostalBrandDictState.csv")
    return(brand_state_naics_date)
}

### Read Master and process ###
# Select useful columns, subset for date range #
## Create National and PostalCode opening rates and IV
process_data = function(){

    pincodes = read.csv("uszips.csv")
    pincodes = pincodes %>% select(zip,state_name,state_id) 

    naics_codes = fread("Naics_2017.csv") %>% select(Seq,naics_code,naics_name) 
    naics_codes$naics_code = as.integer(naics_codes$naics_code)

    data = fread("newdata/master_06_new.csv") %>% as.data.frame()
    #data = fread("master_06.csv") %>% as.data.frame()
    print("Read Data")

    names(data)[names(data) == "poi_cbg" ] = "CBGFIPS"

    ## Loyalty has 2 variables with same name
    data = data[,-25] %>% select(safegraph_place_id,date,brands,postal_code,naics_code,County_Shelter_In_Place_Policy,feb_daywise_avg,visits_by_date,closed_or_not,completely_home_device_count,device_count,CBGFIPS,pct_visits_same_cbg,pct_same_tract,loyalty,total_brands_visited_x)
    data$date = ymd(data$date)


    #Subset for date
    data = data %>% filter(date>=begin_date & date<=end_date)
    
    ## Subsetting for NAICS Essential and conversion to 3-digit NAICS ##  
    data = subset_naics(data)
    
    #Creating new Columns and identifying Big Brands
    list_returned = add_covriates(data,pincodes)
    data = list_returned[[1]]
    big_brands = list_returned[[2]]
    rm(list_returned)
    print("Added Covariates")
    

    #### Creating Dictionary for Brand-Date-Postal Openings #####
    PostalBrandDict = generate_brand_day_state_dictionary(data,pincodes)
    BrandNaicsPostal_prop = PostalBrandDict %>% filter(brands %in% big_brands$brands)
    rm(PostalBrandDict)


    ####   Generating Instrument Variable  #######
    BrandNaicsPostal_prop = BrandNaicsPostal_prop %>% group_by(naics_code,postal_code,date) %>% summarise(BrandPostalProp = mean(proption_naics_national))
    ## ^^Weighted mean at postal-day-naics level 

    data = left_join(data,BrandNaicsPostal_prop,by = c("naics_code" = "naics_code" , "postal_code" = "postal_code", "date" = "date"))


    ####  Generating Main Indepedent Variable  #####
    naics_postal= data %>% filter(big_brands) %>% group_by(naics_code,date,postal_code) %>% summarise(proption_BigBrands_naics_postal_open = sum(open)/n())
    data = left_join(data,naics_postal)
    rm(naics_postal)


    # Adding Postal-Naics Brand Store Open and National Naics Brand Stores Open
    #naics_national= data %>% filter(big_brands) %>% group_by(naics_code,date) %>% summarise(proption_BigBrands_naics_national_open = sum(open)/n())
    #data = left_join(data, naics_national) 
    #rm(naics_national)


    fwrite(data, "preRegData_state.csv")
  
}


## @descr - Subsetting for local stores 
## @param - Dataset
## @return - Dataset containing only Local Establishments along 
generate_nb = function(filein = "preRegData_state.csv"){
    data = fread(filein)
    print("Read File")

    brandsTot = length(unique(data$brands[data$big_brands]))
    print(paste("Unique Brands: ",brandsTot))

    big_brands_all = unique(data$brands[data$big_brands]) %>% as.data.frame()
    fwrite(big_brands_all,"bigBrandsList.csv")

    data_nb = data %>% filter(BrandNational==0)
    rm(data)

    data_nb = data_nb %>% filter(!is.na(BrandPostalProp))

    data_nb['newfactor'] = paste(data_nb$date,data_nb$naics_code)
    data_nb$newfactor = ifelse(is.na(data_nb$naics_code),NA,data_nb$newfactor)

    fwrite(data_nb,"data_nb_state.csv")

}
## Processed

## @descr - Generate Table 1a and Table 2b
generate_summary_table = function(){
    data_nb = fread("data_nb.csv") %>% as.data.frame()
  
    data_nb$date = ymd(data_nb$date)
    
    data_day = data_nb %>% filter(date == begin_date)
    
    distc = data_day %>% group_by(postal_code, naics_code) %>% summarise(brands = mean(BigBrandStores))
    ## Aggregating at postal-naics level, value of BigBrandsStores is same for all local est. in postal-naics pair = mean(BigBrandStores)

    print(paste("Total Community Est ",nrow(data_day)))
    print(paste("Total Brand Stores", sum(distc$brands)))

    cross_section_columns_non_char = c("Feb_Avg" ,"pct_visits_same_cbg","loyal")
    table2 <- stargazer(data_day[,cross_section_columns_non_char],summary.stat = c("n","median","mean","sd","min","max"),header = F,out = "tables/table1a.tex",title = "Summary Statistics")


    nb_non_char = c("open","visits_by_date" ,"prop_home_device_zip" ,"proption_BigBrands_naics_postal_open","BrandPostalProp","postshelter" ) 
    table4 <- stargazer(data_nb[,nb_non_char],summary.stat = c("n","median","mean","sd","min","max"),header = F,out = "tables/table1b.tex",title = "Summary Statistics")

    #stargazer(data_nb[,nb_non_char], type = "text", summary.stat = c("n","median","mean","sd","min","max"),header = F,title = "Summary Statistics")

}


## @descr - Helper function to edit Tex Files Generated
fix_names = function(filein){
    textin = readLines(filein)
    textin = gsub("open","Open",textin)
    textin = gsub("visits_by_date","Daily Visits",textin)
    textin = gsub("BrandPostalProp","National Chain Opening Exposure",textin)
    textin = gsub("proption_BigBrands_naics_postal_Open","Prop. Branch Est. Open",textin)
    textin = gsub("Feb_Avg","Avg. February Traffic",textin)
    textin = gsub("prop_home_device_zip","Prop. Devices At Home",textin)
    textin = gsub("postshelterTRUE","Shelter\\ In\\ Place",textin)

    textin = gsub("proption\\\\_BigBrands\\\\_naics\\\\_postal\\\\_Open","Prop.\\ Branch\\ Est.\\ Open",textin)
    textin = gsub("\\`","",textin)
    textin = gsub("\\(fit\\)","",textin)
    textin = gsub("Feb\\\\_Avg","Avg.\\ February\\ Traffic",textin)
    textin = gsub("prop\\\\_home\\\\_device\\\\_zip","Prop.\\ Devices\\ At\\ Home",textin)
    textin = gsub("pct\\\\_visits\\\\_same\\\\_cbg","Proportion\\ Local\\ Customers", textin)
    textin = gsub("visits\\\\_by\\\\_date","Daily\\ Visits" ,textin)
    textin = gsub("Num. obs.","Observations" ,textin)
    textin = gsub("Num. groups\\:" ,"Number of Groups\\:",textin)
    textin = gsub("newfactor " ,"Date-NAICS",textin)
    textin = gsub("postal\\\\_code" ,"PostalCode",textin)
    textin = gsub("naics\\\\_code" ,"NAICS",textin)
    textin = gsub("R\\$\\^2\\$" ,"R2",textin)
    textin = gsub("\\(full model\\)" ,"",textin)
    textin = gsub("XX" ,"\\$\\\\times\\$",textin)
    textin = gsub("pct\\\\_same\\\\_tract","Prop. Local Customers",textin)    

    textin = textin[-grep(pattern = "proj", x = textin)]

    writeLines(textin,filein)
}

## @descr - Helper function to edit Tex Files Generated
fix_names_summary = function(filein){
    textin = readLines(filein)
    textin = gsub("open","Open",textin)
    textin = gsub("visits_by_date","Daily Visits",textin)
    textin = gsub("BrandPostalProp","National Chain Opening Exposure",textin)
    textin = gsub("proption_BigBrands_naics_postal_Open","Prop. Branch Est. Open",textin)
    textin = gsub("Feb_Avg","Avg. February Traffic",textin)
    textin = gsub("prop_home_device_zip","Prop. Devices At Home",textin)
    textin = gsub("postshelterTRUE","Shelter\\ In\\ Place",textin)

    textin = gsub("proption\\\\_BigBrands\\\\_naics\\\\_postal\\\\_Open","Prop.\\ Branch\\ Est.\\ Open",textin)
    textin = gsub("Feb\\\\_Avg","Avg.\\ February\\ Traffic",textin)
    textin = gsub("prop\\\\_home\\\\_device\\\\_zip","Prop.\\ Devices\\ At\\ Home",textin)
    textin = gsub("pct\\\\_visits\\\\_same\\\\_cbg","Proportion\\ Local\\ Customers", textin)
    textin = gsub("visits\\\\_by\\\\_date","Daily\\ Visits" ,textin)
    textin = gsub("Num. obs.","Observations" ,textin)
    textin = gsub("Num. groups\\:" ,"Number of Groups\\:",textin)
    textin = gsub("newfactor " ,"Date-NAICS",textin)
    textin = gsub("postal\\\\_code" ,"PostalCode",textin)
    textin = gsub("naics\\\\_code" ,"NAICS",textin)
    textin = gsub("R\\$\\^2\\$" ,"R2",textin)
    textin = gsub("\\(full model\\)" ,"",textin)
    textin = gsub("XX" ,"\\$\\\\times\\$",textin)
    textin = gsub("pct\\\\_same\\\\_tract","Prop. Local Customers",textin)
    
    writeLines(textin,filein)
}


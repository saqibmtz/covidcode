## Helper function to identify BigBrands, given a thershold number of stores and state presence
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
  data['open'] = !data$closed_or_not
  naics_essential = data %>% group_by(naics_code) %>%summarise(percent_closed = sum(open==FALSE)/n())
  naics_essential  = naics_essential  %>% filter(percent_closed<=naics_essentail_cutoff) 
  data = data %>% filter(!(naics_code %in% naics_essential$naics_code))
  
  data$naics_code = floor(data$naics_code/(10^naics_digit))
  

  #Creating new Columns
  data['BrandLocal'] = ifelse(data$brands=="",1,0)
  data['BrandNational'] = 1 - data$BrandLocal
  data['brands'] = ifelse(data$brands=="","LOCAL_BRAND",data$brands)
  
  data$County_Shelter_In_Place_Policy = ymd(data$County_Shelter_In_Place_Policy)
  data$County_Shelter_In_Place_Policy = ifelse(is.na(data$County_Shelter_In_Place_Policy),ymd("2099-01-01" ),data$County_Shelter_In_Place_Policy)
  data['preshelter'] = ifelse(data$date < data$County_Shelter_In_Place_Policy,TRUE,FALSE)
  data['postshelter'] = ifelse(data$date >= data$County_Shelter_In_Place_Policy,TRUE,FALSE)
  
  
  print("Added Covariates")
  #fwrite(data,"March_April_daily_with_visitorCount.csv")
  
  
  #### Creating Dictionary for Brand-Date-CBG Openings #####
  PostalBrandDict = data %>% filter(BrandNational==1) %>% select(naics_code,postal_code,brands,date)
  
  BrandPropotionOpen= data %>% filter(BrandNational==1) %>% group_by(naics_code,date,brands) %>% summarise(proption_naics_national = sum(open)/n())
  
  
  PostalBrandDict = left_join(PostalBrandDict,BrandPropotionOpen)
  fwrite(PostalBrandDict,"PostalBrandDict.csv")
  
  print("Created Dictionary")
  
  
  ##Creating Big Brands ##
  data_date = data %>% filter(date == begin_date)
  
  big_brands = get_brand_distribution(data_date,threshold = 50,pincodes,state_threshold = 25,T)
  data['big_brands'] = ifelse(data$brands %in% big_brands$brands, T, F)
  
  #PostalBrandDict = fread("PostalBrandDict.csv")
  
  PostalBrandDict =  PostalBrandDict %>% filter(brands %in% big_brands$brands)
  BrandNaicsPostal_prop = PostalBrandDict
  rm(PostalBrandDict)
  


  ####   Generating Instrument Variable  #######
  BrandNaicsPostal_prop = BrandNaicsPostal_prop %>% group_by(naics_code,postal_code,date) %>% summarise(BrandPostalProp = mean(proption_naics_national))
  ## ^^Weighted mean at postal-day-naics level 
  

  data = left_join(data,BrandNaicsPostal_prop,by = c("naics_code" = "naics_code" , "postal_code" = "postal_code", "date" = "date"))
  
  data = data %>% group_by(naics_code,postal_code) %>% mutate(BrandStores = sum(BrandNational)/46,LocalStores = sum(BrandNational==0)/46, BigBrandStores = sum(big_brands)/46)

  bigBrandsInSample = data %>% group_by(naics_code,postal_code) %>% filter(big_brands) %>% summarise(uniqueBigBrands = length(unique(brands)))

  data = left_join(data,bigBrandsInSample)
  
  #saveRDS(data," temp.RDS")
  
  
  # Adding Postal-Naics Brand Store Open and National Naics Brand Stores Open
  naics_national= data %>% filter(big_brands) %>% group_by(naics_code,date) %>% summarise(proption_BigBrands_naics_national_open = sum(open)/n())
  
  data = left_join(data, naics_national) 
  rm(naics_national)
  
  ####  Generating Main Indepedent Variable  #####
  naics_postal= data %>% filter(big_brands) %>% group_by(naics_code,date,postal_code) %>% summarise(proption_BigBrands_naics_postal_open = sum(open)/n())
  data = left_join(data,naics_postal)
  rm(naics_postal)
  
  data = data %>% group_by(safegraph_place_id) %>% mutate(Feb_Avg = mean(feb_daywise_avg))
  

  ## Adding Loyalty Variable
  competitors = data_date %>% group_by(naics_code,postal_code) %>% summarise(totl=n()) %>% filter(totl>1)
  competitors = competitors %>% mutate(HaveCompetition=T)
  data = left_join(data,competitors)

  data = data %>% mutate(loyal = ifelse(loyalty==0,T,F))

  data = data %>% group_by(postal_code,date) %>% mutate(prop_home_device_zip = sum(completely_home_device_count)/sum(device_count))
  data = data %>% mutate(prop_home_device = completely_home_device_count/device_count)

  data['newfactor'] = paste(data$date,data$naics_code)
  data$newfactor = ifelse(is.na(data$naics_code),NA,data$newfactor)

  fwrite(data, "preRegData.csv")
}


## Subsetting for local stores and creating NAICS-Date interaction
generate_nb = function(filein = "preRegData.csv"){
    data = fread(filein)
    print("Read File")

    brandsTot = length(unique(data$brands[data$big_brands]))
    print(paste("Unique Brands: ",brandsTot))

    data_nb = data %>% filter(BrandNational==0)
    rm(data)

    data_nb = data_nb %>% filter(!is.na(BrandPostalProp))

    fwrite(data_nb,"data_nb.csv")

}
## Processed


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

fix_names = function(filein){
    textin = readLines(filein)
    textin = gsub("open","Open",textin)
    textin = gsub("visits_by_date","Daily Visits",textin)
    textin = gsub("BrandPostalProp","National Chain Opening Exposure",textin)
    textin = gsub("proption_BigBrands_naics_postal_Open","Prop. Branch Est. Open",textin)
    textin = gsub("Feb_Avg","Avg. February Traffic",textin)
    textin = gsub("prop_home_device_zip","Prop. Devices At Home",textin)
    textin = gsub("postshelter","Shelter\\ In\\ Place",textin)

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

fix_names_summary = function(filein){
    textin = readLines(filein)
    textin = gsub("open","Open",textin)
    textin = gsub("visits_by_date","Daily Visits",textin)
    textin = gsub("BrandPostalProp","National Chain Opening Exposure",textin)
    textin = gsub("proption_BigBrands_naics_postal_Open","Prop. Branch Est. Open",textin)
    textin = gsub("Feb_Avg","Avg. February Traffic",textin)
    textin = gsub("prop_home_device_zip","Prop. Devices At Home",textin)

    textin = gsub("proption\\\\_BigBrands\\\\_naics\\\\_postal\\\\_Open","Prop.\\ Branch\\ Est.\\ Open",textin)
    textin = gsub("Feb\\\\_Avg","Avg.\\ February\\ Traffic",textin)
    textin = gsub("prop\\\\_home\\\\_device","Prop.\\ Devices\\ At\\ Home",textin)
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


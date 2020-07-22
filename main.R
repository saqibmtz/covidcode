# Set working directory to shared/saqib/covid/covid on BEAR
# setwd("/shared/saqib/covid/covid")

## !!!Two variables with "loyalty" name in the dataset. Dropping one in process_data. Check with new data ##

library(data.table)
library(lubridate)
library(dplyr)
library(stargazer)
library(lfe)
library(stringr)
library(skimr)
library(fastDummies)
library(broom)
library(dotwhisker)
library(texreg)
library(gridExtra)

## 1. Defining Global Variables ##
begin_date = ymd("2020-03-01")
end_date = ymd("2020-04-15")
naics_digit = 3 ## NAICS level used to aggregate places
naics_essentail_cutoff = .30   #Percent closed threshold to identify essential services


#######################################
######## 2. Data Processing ###########
#######################################

    ### 2.1 Reading Raw Data ################
    pincodes = read.csv("rawdata/uszips.csv") %>% select(zip,state_name,state_id) 
    naics_codes = fread("rawdata/Naics_2017.csv") %>% select(Seq,naics_code,naics_name) 
    naics_codes$naics_code = as.integer(naics_codes$naics_code)

    data = fread("filedata/master_06_new.csv") %>% as.data.frame()



    ### 2.2  Subsetting the data to dates and including useful covariates ##########
    names(data)[names(data) == "poi_cbg" ] = "CBGFIPS"
    data = data[,-25] %>% select(safegraph_place_id,date,brands,postal_code,
                            naics_code,County_Shelter_In_Place_Policy,feb_daywise_avg,visits_by_date,closed_or_not,
                            completely_home_device_count,device_count,CBGFIPS,pct_visits_same_cbg,pct_same_tract,
                            loyalty,total_brands_visited_x)

    data$date = ymd(data$date)
    data = data %>% filter(date>=begin_date & date<=end_date)



    ### 2.3 Subsetting to exclude essential categories ##########
    source(file = "code/subset_naics.r")
    data = subset_naics(data)



    ### 2.4 Creating new Columns and identifying Big Brands
    source(file = "code/add_covariates.r")
    
    list_returned = add_covriates(data,pincodes)
    data = list_returned[[1]]
    big_brands = list_returned[[2]]
    rm(list_returned)
    #fwrite(data,"temp/postcovr.csv")
    

    ### 2.5 Creating Dictionary for Brand-Date-Postal Openings #####
    source(file = "code/generate_brand_day_state_dictionary.r")
    BrandNaicsPostal_prop = generate_brand_day_state_dictionary(data,pincodes)
    

    #### 2.6  Generating Instrument Variable  #######
    BrandNaicsPostal_prop = BrandNaicsPostal_prop %>% 
                                group_by(naics_code,postal_code,date) %>%   
                                    summarise(BrandPostalProp = mean(proption_naics_national))
                                    ## ^^Weighted mean at postal-day-naics level 
    data = left_join(data,BrandNaicsPostal_prop,by = c("naics_code" = "naics_code" , "postal_code" = "postal_code", "date" = "date"))


    #### 2.7  Generating Main Indepedent Variable  #####
    naics_postal= data %>% filter(big_brands) %>% 
                            group_by(naics_code,date,postal_code) %>% 
                                summarise(proption_BigBrands_naics_postal_open = sum(open)/n())

    data = left_join(data,naics_postal)
    rm(naics_postal)

    fwrite(data, "filedata/preRegData_state.csv")
    rm(data)


    #### 2.8 Subsetting to include Local Establishments Only and creating Date x NAICS interaction
    source(file = "code/generate_nb.r")
    generate_nb()


####################################
######### 3. Results ###############
####################################


    ### 3.1 Summary Statistics #####
    source(file = "code/generate_summary_table.r")
    generate_summary_table()

    ### 3.2 Table 2 (Main OLS and IV)  ########
    source(file = "code/main_IV_Reg.R")

    ### 3.3 Table 3 (Moderation)  #####
    source(file="code/moderation_Reg.R")
    

    ### 3.4 Fixing Variable Names ####
    source(file = "code/fix_names.r")
    fix_names_summary("tables/table1a.tex")
    fix_names_summary("tables/table1b.tex")
    fix_names("tables/table2a.tex")
    fix_names("tables/table2b.tex")
    fix_names("tables/table3.tex")


## Defining Cosine similarity for arrays
cosine = function(x,y){
    return(sum(x*y)/(sum(x^2)^0.5 *sum(y^2)^0.5) )
}


#### #### 

customerCBGs = fread("filedata/febpatternscbg.csv")
customerCBGs = customerCBGs %>% filter(visitor_types == "H") %>% select(-visitor_types)
data = fread("filedata/preRegData_state.csv") 
data$date = ymd(data$date)
data = data %>% filter(date == begin_date) %>% select(safegraph_place_id,postal_code,naics_code,BrandLocal, BrandNational,big_brands) 

data_bb = data %>% filter(big_brands)
data_nb = data %>% filter(BrandNational == 0)
rm(data)

## Seperating into big brands and communty etb. 
#### inner joins to get the intersection of the two!
customerCBGs_bigbrands = data_bb %>% inner_join(customerCBGs,.)
customerCBGs_nonbrands = data_nb %>% mutate(non_brands = T) %>% inner_join(customerCBGs,.)

customerCBGs_bigbrands = customerCBGs_bigbrands %>% group_by(naics_code,postal_code,visitor_cbg) %>% summarise(MeanVisitors_brand = mean(visitor_counts,na.rm=T))

#### Diagnostic
#customerCBGs_bigbrands %>% filter(naics_code == 423, postal_code == 6106) %>% as.data.frame()
#### Normalise before cosine(?)
#customerCBGs_bigbrands = customerCBGs_bigbrands %>% group_by(naics_code,postal_code) %>% mutate(totalVisitors = normalize(totalVisitors,method = "range", range = c(0,1)))


brand_zip_naics = data_bb %>% distinct(naics_code,postal_code)
community_zip_naics = data_nb %>% distinct(naics_code,postal_code)
both_zip_naics = inner_join(brand_zip_naics,community_zip_naics)

customerCBGs_bigbrands = inner_join(customerCBGs_bigbrands,both_zip_naics)
customerCBGs_nonbrands = inner_join(customerCBGs_nonbrands,both_zip_naics)


## Group_by left_join is not an easy option in R. Merging is slow. Instead using a global merge and fixing for missing data.
## Creating a list of all possible NAICS x Zip X CBG combinations observed in the data and populating that 


brand_zip_naics_cbg = customerCBGs_bigbrands %>% distinct(naics_code,postal_code,visitor_cbg) %>% as.data.frame()
community_zip_naics_cbg = customerCBGs_nonbrands %>% distinct(naics_code,postal_code,visitor_cbg)
both_zip_naics_cbg = rbind(brand_zip_naics_cbg,community_zip_naics_cbg) %>% distinct(naics_code,postal_code,visitor_cbg)

community_zip_naics_locations = data_nb %>% distinct(safegraph_place_id,naics_code,postal_code)
community_zip_naics_cbg_locations = left_join(community_zip_naics_locations,both_zip_naics_cbg) %>% mutate(loc = "Master")

community_data = left_join(community_zip_naics_cbg_locations, customerCBGs_nonbrands)

community_data = left_join(community_data, customerCBGs_bigbrands, by = c("naics_code", "postal_code", "visitor_cbg"), suffix = c("_Community","_Brand"))


## Filling NAs with zeros
community_data = community_data %>% mutate(visitor_counts = replace_na(visitor_counts,0), MeanVisitors_brand = replace_na(MeanVisitors_brand,0))
community_data = community_data %>% group_by(safegraph_place_id) %>% mutate(zero_community = ifelse(sum(visitor_counts)==0,T,F), zero_brand = ifelse(sum(MeanVisitors_brand)==0,T,F))

community_data = community_data %>% filter(!zero_community & !zero_brand)


## Computing Cosine similarity
community_cosine = community_data %>% group_by(safegraph_place_id,naics_code,postal_code) %>% summarise(cosine_similarity = cosine(visitor_counts,MeanVisitors_brand))
community_cosine = community_cosine %>% group_by(naics_code,postal_code) %>% mutate(medianVal = median(cosine_similarity), totalEst = n())

community_cosine_dissimilar = community_cosine %>% filter(cosine_similarity < medianVal) %>% as.data.frame() %>% select(safegraph_place_id,naics_code,postal_code)

fwrite(community_cosine_dissimilar,"filedata/cosineDissimilar.csv")


####

data_nb = fread("filedata/data_nb_state.csv") %>% distinct(safegraph_place_id) %>% nrow()
data_nb$date = ymd(data_nb$date)

data_nb = inner_join(data_nb,community_cosine_dissimilar)
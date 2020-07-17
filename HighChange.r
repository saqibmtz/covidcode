
### Identifying High-Change brands and looking at their impact ### 
data_nb = fread("data_nb_state.csv") %>% as.data.frame()  
data_nb$date = ymd(data_nb$date)


data = fread("preRegData_state.csv")%>% filter(naics_code == 713)
data_bb = data %>% filter(big_brands)

data_bb$date = ymd(data_bb$date)
pincodes = read.csv("uszips.csv")
pincodes = pincodes %>% select(zip,state_name,state_id) 
names(pincodes)[names(pincodes)=="zip"] = "postal_code"

data_bb = left_join(data_bb,pincodes)

bb_daily_open = data_bb %>% group_by(brands,date,naics_code) %>% summarise(prop_open_national = sum(open)/n()) %>% filter(wday(date)!=1 & wday(date)!=7)
bb_daily_open = bb_daily_open %>% group_by(brands,naics_code) %>% mutate(prop_change_national = prop_open_national - lag(prop_open_national,order_by=date)) %>% drop_na()


gym_daily_open = ggplot(bb_daily_open) + geom_line(aes(y = prop_open_national, x = date, group = brands, color = brands)) + xlab("Date") + ylab("Prop. Open")
ggsave("plots/dnd/gyms_daily.jpg", gym_daily_open,height = 8, width = 15)

## Listings All - Easy Check
bb_daily_open %>% filter(abs(prop_change_national)>0.3) %>% filter(naics_code!=722) %>% arrange(date,naics_code,brands,prop_change_national) %>% as.data.frame()


## 
high_change = bb_daily_open %>% filter(abs(prop_change_national)>0.3) 
high_change = high_change %>% select(brands,date,naics_code) %>% mutate(highChange = T)

data_bb %>% filter(brands %in% high_change$brands) %>% group_by(brands) %>% summarise(totalStores = n(),unique(naics_code))

data_bb = left_join(data_bb,high_change)
data_bb = data_bb %>% group_by(brands,postal_code,date) %>% mutate(thisBrandStores = n()) %>% ungroup()

high_change_zips = data_bb %>% filter(highChange) %>% select(postal_code,date,naics_code,thisBrandStores,highChange)

for(brand in high_change$brands){
    brand = "Autograph Collection Hotels"
    brand = "Sky Zone"
    data_brand = data_bb %>% filter(brands == brand)
    brand_prop_open = data_brand %>% group_by(date) %>% filter(wday(date)!=1 & wday(date)!=7) %>% summarise(Prop_Brand_Open = sum(open)/n())

    other_brand_prop_open = data_bb %>% filter(naics_code == high_change$naics_code[high_change$brands==brand]) %>% filter(wday(date)!=1 & wday(date)!=7) %>% filter(brands!= brand) %>% group_by(brands,date) %>% summarise(Prop_Brand_Open = sum(open)/n())

    plot_brands = ggplot() + geom_smooth(aes(x=other_brand_prop_open$date,y=other_brand_prop_open$Prop_Brand_Open)) + theme(legend.position = "none") + geom_line(aes(x=other_brand_prop_open$date,y=other_brand_prop_open$Prop_Brand_Open,group=other_brand_prop_open$brands),size=0.5, alpha=0.2) + geom_line(aes(x=brand_prop_open$date,y=brand_prop_open$Prop_Brand_Open,color='red')) + geom_vline(xintercept = high_change$date[high_change$brands==brand],linetype='dashed')

  
    ggsave(paste("plots/dnd/",brand,"_otherBrands.png",sep=""),plot_brands)

    brand_zips = data_brand %>% select(postal_code,naics_code,date) %>% group_by(postal_code) %>% mutate(thisBrandStores=n(),treatment=T) %>% ungroup()
    data_nb_brand = data_nb %>% filter(naics_code == brand_zips$naics_code[1])
    data_nb_brand = left_join(data_nb_brand,brand_zips)

    data_nb_brand = data_nb_brand %>% mutate(treatment = ifelse(is.na(treatment),F,T))
    data_nb_brand = data_nb_brand %>% mutate(post=ifelse(date>high_change$date[high_change$brands==brand],T,F))

    nb_prop_open_postal = data_nb_brand %>% group_by(date,treatment) %>% summarise(prop_local_open = sum(open)/n())

    p1 = ggplot(nb_prop_open_postal) + geom_line(aes(x = date, y = prop_local_open,group = treatment,color = treatment)) + geom_vline(xintercept = high_change$date[high_change$brands==brand])
    ggsave(paste("plots/dnd/",brand,".png",sep=""),p1) 

    data_nb_brand$date2 = as.factor(data_nb_brand$date)
    est1 = data_nb_brand %>% felm(open ~ treatment*date2 | postal_code + date,.) 

    p2 = dwplot(tidy(est1)%>% drop_na())
    ggsave(paste("plots/dnd/",brand,"est.png",sep=""),p2)
}

data_nb = left_join(data_nb,high_change_zips)

data_nb$highChange = ifelse(!is.na(data_nb$highChange),T,F)

data_nb["highChangeXProp"] = data_nb$highChange * data_nb$proption_BigBrands_naics_postal_open
data_nb["highXExop"] = data_nb$highChange * data_nb$proption_BigBrands_naics_postal_open

model5 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open*highChange + Feb_Avg + prop_home_device | newfactor + postal_code | 0 |postal_code,.)

iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + highChange |  newfactor + postal_code | (proption_BigBrands_naics_postal_open + highChangeXProp ~ BrandPostalProp + highXExop) | postal_code,.)

## Weighted High Exposure
data_nb$highChange = (data_nb$highChange*data_nb$thisBrandStores)/data_nb$BigBrandStores

data_nb["highChangeXProp"] = data_nb$highChange * data_nb$proption_BigBrands_naics_postal_open
data_nb["highXExop"] = data_nb$highChange * data_nb$proption_BigBrands_naics_postal_open

model5 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open*highChange + Feb_Avg + prop_home_device | newfactor + postal_code | 0 |postal_code,.)

iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + highChange |  newfactor + postal_code | (proption_BigBrands_naics_postal_open + highChangeXProp ~ BrandPostalProp + highXExop) | postal_code,.)


### Change Open Analysis ### 
data_nb = data_nb %>% group_by(safegraph_place_id) %>% filter(wday(date)!=1 | wday(date)!=7) %>% mutate(lagOpen = lag(open,order_by=date),change_prop_Big = proption_BigBrands_naics_postal_open - lag(proption_BigBrands_naics_postal_open, order_by = date),change_BrandPostal = BrandPostalProp - lag(BrandPostalProp,order_by=date))
data_nb = data_nb %>% mutate(changeOpen = open-lagOpen)

model5 <- data_nb %>% felm(changeOpen ~ proption_BigBrands_naics_postal_open*highChange + Feb_Avg + prop_home_device | newfactor + postal_code | 0 |postal_code,.)

iv3 <- data_nb %>% felm(changeOpen ~  Feb_Avg + prop_home_device + highChange |  newfactor + postal_code | (proption_BigBrands_naics_postal_open + highChangeXProp ~ BrandPostalProp + highXExop) | postal_code,.)

### Change on all ###


iv3 <- data_nb %>% felm(changeOpen ~  Feb_Avg + prop_home_device |  newfactor + postal_code | (change_prop_Big  ~ change_BrandPostal) | postal_code,.)


## Seperating into positive and negative change
data_nb = data_nb %>% mutate(PosChange = ifelse(change_prop_Big>0,T,F))
data_nb = data_nb %>% mutate(propXPosChange = PosChange*change_prop_Big, ExposurexPosChange= PosChange*change_BrandPostal)

iv4 <- data_nb %>% felm(changeOpen ~  Feb_Avg + prop_home_device |  newfactor + postal_code | (change_prop_Big  + propXPosChange ~ change_BrandPostal + ExposurexPosChange) | postal_code,.)

### Moderating by ratio of local/brand ###

data_nb = data_nb %>% mutate(BigBrandsRatio = BigBrandStores/LocalStores)

data_nb = data_nb %>% mutate(propXRatio = BigBrandsRatio*proption_BigBrands_naics_postal_open, ExposurexRatio= BrandPostalProp*BigBrandsRatio)

iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + BigBrandsRatio |  newfactor + postal_code | (proption_BigBrands_naics_postal_open + propXRatio   ~ BrandPostalProp + ExposurexRatio) | postal_code,.)
# Ratio Doesnot impact the results as much

data_nb = data_nb %>% mutate(propXRatio = BigBrandsRatio*change_prop_Big, ExposurexRatio= change_BrandPostal*BigBrandsRatio)
iv3 <- data_nb %>% felm(changeOpen ~  Feb_Avg + prop_home_device + BigBrandsRatio |  newfactor + postal_code | (change_prop_Big + propXRatio   ~ BrandPostalProp + change_BrandPostal) | postal_code,.)



## Postal Diversity ~ Different Brand presents in a zip code

postal_diversity = data_bb %>% group_by(naics_code,postal_code) %>% summarise(BigBrandsPresent = length(unique(brands)))
data_nb = left_join(data_nb,postal_diversity)

data_nb = data_nb %>% mutate(propXdiversity = BigBrandsPresent*proption_BigBrands_naics_postal_open, ExposurexDiversity= BrandPostalProp*BigBrandsPresent)

iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + BigBrandsPresent |  newfactor + postal_code | (proption_BigBrands_naics_postal_open + propXdiversity   ~ BrandPostalProp + ExposurexDiversity) | postal_code,.)

stargazer(iv3, type = "text")


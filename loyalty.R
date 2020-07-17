
get_competitor_count = function(filein = "preRegData.csv"){
    data = fread(filein)
    data$date = ymd(data$date)

    begin_date = ymd("2020-03-01")

    data_day = data %>% filter(date==begin_date)

    postal_naics_competitiors = data_day %>% group_by(naics_code,postal_code) %>% summarise(totl=n()) %>% filter(totl>1)

    rm(data)
    return(postal_naics_competitiors)
}

competitors = get_competitor_count()
competitors = competitors %>% mutate(HaveCompetition=T)

figg = competitors %>% group_by(naics_code) %>% summarise(meanComp = mean(totl)) %>% arrange(desc(meanComp))
ggplot(figg) + geom_histogram(aes(x=meanComp-1),binwidth=1) + xlab("Average Number of Competitors")
ggsave("plots/competitors.png")

data_nb = fread("data_nb.csv") %>% filter(!is.na(BrandPostalProp))
data_nb = left_join(data_nb,competitors)

data_nb = data_nb %>% filter(HaveCompetition)
data_nb = data_nb %>% mutate(loyal = ifelse(loyalty==0,T,F))
#loyalty : No. of stores visited in the same NAICS

naics_loyal = data_nb %>% group_by(naics_code) %>% summarise(percent_loyal = sum(loyalty)/n(),totalStores = n()) %>% arrange(desc(percent_loyal))

## Dummy for categorical proportion of local visitors
dummies = dummy_cols(data_nb$loyal) %>% select(-1)
names(dummies) = c("NotLoyal","Loyal")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of local visitors * Branch Est. Open
dummies = dummy_cols(data_nb$loyal) %>% select(-1)
dummies = data_nb$proption_BigBrands_naics_postal_open * dummies
names(dummies) = c("Proportion_NotLoyal","Proportion_Loyal")
data_nb = cbind(data_nb,dummies)

## Dummy for categorical interaction of proportion of local visitors * IV
dummies = dummy_cols(data_nb$loyal) %>% select(-1)
dummies = data_nb$BrandPostalProp * dummies
names(dummies) = c("Expsoure_NotLoyal","Exposure_Loyal")
data_nb = cbind(data_nb,dummies)

## OLS
model1 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device +  proption_BigBrands_naics_postal_open + Loyal + Proportion_Loyal |  newfactor + postal_code | 0 | postal_code,.)


model2<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device + Loyal |  newfactor + postal_code  | ( Proportion_Loyal +  proption_BigBrands_naics_postal_open  ~  Exposure_Loyal + BrandPostalProp) | postal_code,.)

screenreg(list(model1,model2),caption.above = T,custom.model.names = c("OLS","IV"),digits=3,custom.coef.names = c("Feb_Avg","prop_home_device","proption_BigBrands_naics_postal_open","Loyal Customers","Branch Open XX Loyal Customers","Branch Open XX Loyal Customers","proption_BigBrands_naics_postal_open"),,custom.gof.rows=list("Fixed Effect Date-NAICS"=c("Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes")),reorder.gof = c(1,2,8,9,3,4,5,6,7))

texreg(list(model1,model2),file = "tables/loyalty.tex",caption.above = T,custom.model.names = c("OLS","IV"),digits=3,custom.coef.names = c("Feb_Avg","prop_home_device","proption_BigBrands_naics_postal_open","Loyal Customers","Branch Open XX Loyal Customers","Branch Open XX Loyal Customers","proption_BigBrands_naics_postal_open"),,custom.gof.rows=list("Fixed Effect Date-NAICS"=c("Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes")),reorder.gof = c(1,2,8,9,3,4,5,6,7))
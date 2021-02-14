######################################
###### Robustness Results ###########
####################################


## Generates robustness results for Models 7-10

data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)
data_nb$countyName = as.factor(data_nb$countyName)


#####################################
###### 1. Removing Clusters ##########
#######################################

cluster_data = fread("filedata/cluster_004.csv")
## To generate the file run clustering.py ##

data_nb = left_join(data_nb, cluster_data)
print(c("Percentage outside of Cluster ", sum(!data_nb$inCluster)/dim(data_nb)[1]))
data_nb2 = data_nb %>% filter(!inCluster)

ols_cluster <- data_nb2 %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | newfactor + countyDate + postal_code  | 0 |countyName,.)
iv_cluster <- data_nb2 %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + countyDate + postal_code  | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)

rm(data_nb2)


###################################
##### 2. Brand Closure Variance ####
#####################################
# Restricting to brands that have low daily variance in opening/closing 
source(file = "code/VarianceAnalysis.r")

data_nb = fread("temp/data_nb_state_lowVar.csv")
ols_low_var <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | newfactor + countyDate + postal_code  | 0 |countyName,.)
iv_low_var <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + countyDate + postal_code  | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)




#####################################
####### 3. Dis-similar customers #####
#######################################

#Restricting to establishments that have dis-similar customers from brand establishments
source(file = "code/customerSimilarity.r")

community_cosine_dissimilar = fread("filedata/cosineDissimilar.csv")
data_nb = fread("filedata/data_nb_state.csv")
###data_nb$date = ymd(data_nb$date)

data_nb = inner_join(data_nb,community_cosine_dissimilar)

ols_dissimilar <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | newfactor + countyDate + postal_code  | 0 |countyName,.)
iv_disimilar <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + countyDate + postal_code  | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)


#####################################
####### 4. Customers Disimilarity#####
#######################################

#Restricting to establishments that have dis-similar customers from brand establishments

non_border_establishments = fread("filedata/included_sg.csv")
names(non_border_establishments) = c("Serial","safegraph_place_id")
data_nb = fread("filedata/data_nb_state.csv")
data_nb = left_join(non_border_establishments,data_nb)
###data_nb$date = ymd(data_nb$date)

ols_non_border <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | newfactor + countyDate + postal_code  | 0 |countyName,.)
iv_non_border <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + countyDate + postal_code  | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)




###### Display Results  #######

screenreg(list(ols_cluster,iv_cluster,ols_low_var,iv_low_var,ols_dissimilar,iv_disimilar,ols_non_border,iv_non_border),digits = 3, caption = "Regression",  caption.above = T,custom.header = list("Model 7"=1:2,"Model 8"=3:4, "Model 9" = 5:6,"Model 10" = 7:8), custom.model.names = c("OLS","IV","OLS","IV","OLS","IV","OLS","IV"),custom.coef.names = c("Prop. Branch Est. Open","Feb_Avg","prop_home_device_zip","Prop. Branch Est. Open"),custom.gof.rows=list("Fixed Effect NAICS XX Date"=c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"),"Fixed Effect County XX Date" = c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"),"Fixed Effect Zip" = c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),table=F)

texreg(list(ols_cluster,iv_cluster,ols_low_var,iv_low_var,ols_dissimilar,iv_disimilar,ols_non_border,iv_non_border), file = "tables/table4.tex", digits = 3, caption = "Regression",  caption.above = T,custom.header = list("Model 7"=1:2,"Model 8"=3:4, "Model 9" = 5:6,"Model 10" = 7:8), custom.model.names = c("OLS","IV","OLS","IV","OLS","IV","OLS","IV"),custom.coef.names = c("Prop. Branch Est. Open","Feb_Avg","prop_home_device_zip","Prop. Branch Est. Open"),custom.gof.rows=list("Fixed Effect NAICS XX Date"=c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"),"Fixed Effect County XX Date" = c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"),"Fixed Effect Zip" = c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),table=F)
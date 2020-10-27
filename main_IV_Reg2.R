### Regression  ###

data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)


#cluster_data = fread("filedata/cluster_004.csv")
#data_nb = left_join(data_nb, cluster_data)
#print(c("Percentage outside of Cluster ", sum(!data_nb$inCluster)/dim(data_nb)[1]))
#data_nb = data_nb %>% filter(!inCluster)

data_nb$countyName = as.factor(data_nb$countyName)



#########################
#### 1. OLS MDOELS   ####
##########################


print(paste("Running Regression"))

## Without Controls
model1 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open |0|0|countyName,.,keepX=FALSE)
print(paste("Done 1/5"))

## With Controls
model2 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip + postshelter|0|0| countyName,.)
print(paste("Done 2/5"))

## With Controls + FE Naics + FE County
model3 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip  + postshelter | naics_code + countyName | 0 |countyName,.)
print(paste("Done 3/5"))

## With Controls + FE Naics + FE County + FE Date
model4 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | naics_code + countyName + date | 0 |countyName,.)

## With Controls + FE Naics*Date + FE County * Date
model5 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | newfactor + countyDate | 0 |countyName,.)
print(paste("Done 5/5"))

#screenreg(list(model1,model2,model3,model4,model5))

screenreg(list(model1,model2,model3,model4,model5),caption = "Regression",caption.above = T,custom.header=list("Open"=1:5),digits=3,reorder.coef=c(2,3,4,5,1),custom.gof.rows=list("Fixed Effect NAICS"=c("No","No","Yes","Yes","No"),"Fixed Effect County" = c("No","No","Yes","Yes","No"), "Fixed Effect Date"=c("No","No","No","Yes","No"),"Fixed Effect NAICS XX Date"=c("No","No","No","No","Yes"),"Fixed Effect County XX Date"=c("No","No","No","No","Yes")))


texreg(list(model1,model2,model3,model4,model5),file = "tables/table2a.tex", caption = "Regression",caption.above = T,custom.header=list("Open"=1:5),digits=3,reorder.coef=c(2,3,4,5,1),custom.gof.rows=list("Fixed Effect NAICS"=c("No","No","Yes","Yes","No"),"Fixed Effect County" = c("No","No","Yes","Yes","No"), "Fixed Effect Date"=c("No","No","No","Yes","No"),"Fixed Effect NAICS XX Date"=c("No","No","No","No","Yes"),"Fixed Effect County XX Date"=c("No","No","No","No","Yes")),table=F)

rm(model1,model2,model3,model4,model5)



#######################
#### 2. IV MODELS #####
#######################

print("Running IV Models")

### No Controls  ### 

#fs1 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp,.)
#iv1 <- data_nb %>% felm(open ~ 1 |  0 | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp),.)

print("1/3")

### Controls; Without FE ####

fs2 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip | naics_code + countyName + date | 0 | countyName,.)
iv2 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip | naics_code + countyName + date | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)

print("2/3")

### Controls; With FE ####
fs3 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip| newfactor + countyDate | 0 | countyName,.)
iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + countyDate | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | countyName,.)


print("3/3")


### Saving 4 Models, excluding No controls, No FE.

screenreg(list(fs2,iv2,fs3,iv3),digits=3,caption = "Regression",caption.above = T,custom.header = list("Model 6" = 1:2, "Model 7" = 3:4),custom.model.names = c("First Stage","IV","First Stage","IV"),reorder.coef=c(4,1,2,3),custom.gof.rows=list("Fixed Effect NAICS"=c("Yes","Yes","No","No"),"Fixed Effect County" = c("Yes","Yes","No","No"), "Fixed Effect Date"=c("Yes","Yes","No","No"),"Fixed Effect NAICS XX Date"=c("No","No","Yes","Yes"),"Fixed Effect County XX Date"=c("No","No","Yes","Yes")),table=F)

texreg(list(fs2,iv2,fs3,iv3),digits=3,file = "tables/table2b.tex",caption = "Regression",caption.above = T,custom.header = list("Model 6" = 1:2, "Model 7" = 3:4),custom.model.names = c("First Stage","IV","First Stage","IV"),reorder.coef=c(4,1,2,3),custom.gof.rows=list("Fixed Effect NAICS"=c("Yes","Yes","No","No"),"Fixed Effect County" = c("Yes","Yes","No","No"), "Fixed Effect Date"=c("Yes","Yes","No","No"),"Fixed Effect NAICS XX Date"=c("No","No","Yes","Yes"),"Fixed Effect County XX Date"=c("No","No","Yes","Yes")),table=F)

rm(fs3,fs2,iv2,iv3)
### Regression  ###

data_nb = fread("filedata/data_nb_state.csv")
data_nb$date = ymd(data_nb$date)


#########################
#### 1. OLS MDOELS   ####
##########################


print(paste("Running Regression"))

## Without Controls
model1 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open,.,keepX=FALSE)
print(paste("Done 1/5"))

## With Controls
model2 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip + postshelter,.)
print(paste("Done 2/5"))

## With Controls + FE Naics + FE Postal
model3 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip  + postshelter | naics_code + postal_code | 0 |postal_code,.)
print(paste("Done 3/5"))

## With Controls + FE Naics + FE Postal + FE Date
model4 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | naics_code + postal_code + date | 0 |postal_code,.)

## With Controls + FE Naics*Date + FE Postal
model5 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | newfactor + postal_code + countyDate | 0 |postal_code,.)
print(paste("Done 5/5"))

#screenreg(list(model1,model2,model3,model4,model5))

screenreg(list(model1,model2,model3,model4,model5),caption = "Regression",caption.above = T,custom.header=list("Open"=1:5),digits=3,reorder.coef=c(2,3,4,5,1),custom.gof.rows=list("Fixed Effect NAICS"=c("No","No","Yes","Yes","No"),"Fixed Effect PostalCode" = c("No","No","Yes","Yes","Yes"), "Fixed Effect Date"=c("No","No","No","Yes","No"),"Fixed Effect NAICS XX Date"=c("No","No","No","No","Yes"),"Fixed Effect County XX Date"=c("No","No","No","No","Yes")))


texreg(list(model1,model2,model3,model4,model5),file = "tables/table2a.tex", caption = "Regression",caption.above = T,custom.header=list("Open"=1:5),digits=3,reorder.coef=c(2,3,4,5,1),custom.gof.rows=list("Fixed Effect NAICS"=c("No","No","Yes","Yes","No"),"Fixed Effect PostalCode" = c("No","No","Yes","Yes","Yes"), "Fixed Effect Date"=c("No","No","No","Yes","No"),"Fixed Effect NAICS XX Date"=c("No","No","No","No","Yes"),"Fixed Effect County XX Date"=c("No","No","No","No","Yes")),table=F)

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

fs2 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip | naics_code + postal_code + date | 0 | postal_code,.)
iv2 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip | naics_code + postal_code + date | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp),.)

print("2/3")

### Controls; With FE ####
fs3 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip| newfactor + postal_code + countyDate| 0 | postal_code,.)
iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + postal_code + countyDate| (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | postal_code,.)


print("3/3")


### Saving 4 Models, excluding No controls, No FE.

screenreg(list(fs2,iv2,fs3,iv3),digits=3,caption = "Regression",caption.above = T,custom.header = list("Model 6" = 1:2, "Model 7" = 3:4),custom.model.names = c("First Stage","IV","First Stage","IV"),reorder.coef=c(4,1,2,3),custom.gof.rows=list("Fixed Effect NAICS"=c("Yes","Yes","No","No"),"Fixed Effect PostalCode" = c("Yes","Yes","Yes","Yes"), "Fixed Effect Date"=c("Yes","Yes","No","No"),"Fixed Effect NAICS XX Date"=c("No","No","Yes","Yes"),"Fixed Effect County XX Date"=c("No","No","Yes","Yes")),table=F)

texreg(list(fs2,iv2,fs3,iv3),digits=3,file = "tables/table2b.tex",caption = "Regression",caption.above = T,custom.header = list("Model 6" = 1:2, "Model 7" = 3:4),custom.model.names = c("First Stage","IV","First Stage","IV"),reorder.coef=c(4,1,2,3),custom.gof.rows=list("Fixed Effect NAICS"=c("Yes","Yes","No","No"),"Fixed Effect PostalCode" = c("Yes","Yes","Yes","Yes"), "Fixed Effect Date"=c("Yes","Yes","No","No"),"Fixed Effect NAICS XX Date"=c("No","No","Yes","Yes"),"Fixed Effect County XX Date"=c("No","No","Yes","Yes")),table=F)

rm(fs3,fs2,iv2,iv3)
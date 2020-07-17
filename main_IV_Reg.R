
begin_date = ymd("2020-03-01")
end_date = ymd("2020-04-15")

### Regression  ###

data_nb = fread("data_nb.csv")
data_nb$date = ymd(data_nb$date)



# OLS MDOELS   #
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
model5 <- data_nb %>% felm(open ~ proption_BigBrands_naics_postal_open + Feb_Avg + prop_home_device_zip | newfactor + postal_code | 0 |postal_code,.)
print(paste("Done 5/5"))

#screenreg(list(model1,model2,model3,model4,model5))

screenreg(list(model1,model2,model3,model4,model5),caption = "Regression",caption.above = T,custom.header=list("Open"=1:5),digits=3,reorder.coef=c(2,3,4,5,1),custom.gof.rows=list("Fixed Effect NAICS"=c("No","No","Yes","Yes","No"),"Fixed Effect PostalCode" = c("No","No","Yes","Yes","Yes"), "Fixed Effect Date"=c("No","No","No","Yes","No"),"Fixed Effect Date XX NAICS"=c("No","No","No","No","Yes")),reorder.gof = c(1,2,3,4,10,11,12,13,5,6,7,8,9))


texreg(list(model1,model2,model3,model4,model5),file = "tables/table2a.tex",caption = "Regression",caption.above = T,custom.header=list("Local Est. Open"=1:5),digits=3,reorder.coef=c(2,3,4,5,1),custom.gof.rows=list("Fixed Effect NAICS"=c("No","No","Yes","Yes","No"),"Fixed Effect PostalCode" = c("No","No","Yes","Yes","Yes"), "Fixed Effect Date"=c("No","No","No","Yes","No"),"Fixed Effect Date XX NAICS"=c("No","No","No","No","Yes")),reorder.gof = c(1,2,3,4,10,11,12,13,5,6,7,8,9),table=F)

rm(model1,model2,model3,model4,model5)



## IV MODELS ##

print("Running IV Models")

### No Controls  ### 

fs1 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp,.)
iv1 <- data_nb %>% felm(open ~ 1 |  0 | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp),.)

print("1/3")

### Controls; Without FE ####

fs2 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip + postshelter,.)
iv2 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip + postshelter| 0 | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp),.)

print("2/3")

### Controls; With FE ####
fs3 <- data_nb %>% felm(proption_BigBrands_naics_postal_open  ~ BrandPostalProp + Feb_Avg + prop_home_device_zip| newfactor + postal_code | 0 | postal_code,.)
iv3 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip |  newfactor + postal_code | (proption_BigBrands_naics_postal_open  ~ BrandPostalProp) | postal_code,.)


print("3/3")

## Saving 6 models

#screenreg(list(fs1,iv1,fs2,iv2,fs3,iv3),digits=3,caption = "Regression",caption.above = T,custom.model.names = c("FS","IV","FS","IV","FS","IV"),reorder.coef = c(3,4,5,2,1),custom.gof.rows=list("Fixed Effect Date-NAICS"=c("No","No","No","No","Yes","Yes"),"Fixed Effect PostalCode" = c("No","No","No","No","Yes","Yes")),reorder.gof = c(1,2,8,9,3,4,5,6,7))

#texreg(list(fs1,iv1,fs2,iv2,fs3,iv3),digits=3,file= "tables/table2b.tex", caption = "Regression",caption.above = T,custom.model.names = c("Model 6","Model 7","Model 8","Model 9","Model 10","Model 11"),reorder.coef = c(3,4,5,2,1),custom.gof.rows=list("Fixed Effect Date-NAICS"=c("No","No","No","No","Yes","Yes"),"Fixed Effect PostalCode" = c("No","No","No","No","Yes","Yes")),reorder.gof = c(1,2,8,9,3,4,5,6,7),table=F)
#stars=c(0.05)



### Saving 4 Models, excluding No controls, No FE.

screenreg(list(fs2,iv2,fs3,iv3),digits=3,caption = "Regression",caption.above = T,custom.header = list("Model 6" = 1:2, "Model 7" = 3:4),custom.model.names = c("First Stage","IV","First Stage","IV"),reorder.coef=c(6,2,3,4,5,1),custom.gof.rows = list("Fixed Effect Date-NAICS"=c("No","No","Yes","Yes"),"Fixed Effect PostalCode" = c("No","No","Yes","Yes")), reorder.gof = c(1,2,8,9,3,4,5,6,7),table=F)

texreg(list(fs2,iv2,fs3,iv3),digits=3,file = "tables/table2b.tex",caption = "Regression",caption.above = T,custom.header = list("Model 6" = 1:2, "Model 7" = 3:4),custom.model.names = c("First Stage","IV","First Stage","IV"),reorder.coef=c(6,2,3,4,5,1),custom.gof.rows = list("Fixed Effect Date-NAICS"=c("No","No","Yes","Yes"),"Fixed Effect PostalCode" = c("No","No","Yes","Yes")), reorder.gof = c(1,2,8,9,3,4,5,6,7),table=F,custom.note = "\\item %stars. Note")

rm(fs1,fs2,fs3,iv1,iv2,iv3)
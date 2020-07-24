## Function to generate coefficients for local customers interaction
get_hetro_coeff_3splits <- function(data_in, percentile_based = F){


    print(paste("Missing PCT Visits:",round(sum(is.na(data_nb["pct_same_tract"]))/nrow(data_nb)*100,2),"%"))
    data_nb = data_in %>% filter(!is.na(pct_same_tract))
    data_nb['interact'] = data_nb["pct_same_tract"]*data_nb$proption_BigBrands_naics_postal_open
    data_nb['interact_expsoure'] = data_nb["pct_same_tract"]*data_nb$BrandPostalProp
 

    ## First definition of cutoff Percentile Based
    if(percentile_based){
        data_nb['cutoff'] = ntile(data_nb["pct_same_tract"],3)
        t=data_nb %>% group_by(cutoff) %>% summarise(minimum = min(pct_same_tract),maximum = max(pct_same_tract))
        print(t)

        data_nb['cutoff'] = as.factor(data_nb$cutoff) 
    }
    ## Second definition Absolute cuttoffs bins of 1/3rd
    else{
        data_nb['cutoff'] = cut(data_nb[["pct_same_tract"]]*100,breaks=c(0,33,66,100),labels = c("1","2","3"),include.lowest = T)
        t=data_nb %>% group_by(cutoff) %>% summarise(count = n())
        print(t)
    }

    ## Dummy for categorical proportion of local visitors
    dummies = dummy_cols(data_nb$cutoff) %>% select(-1)
    names(dummies) = c("Visitors_Low","Visitors_Mid","Visitors_High")
    data_nb = cbind(data_nb,dummies)

    ## Dummy for categorical interaction of proportion of local visitors * Branch Est. Open
    dummies = dummy_cols(data_nb$cutoff) %>% select(-1)
    dummies = data_nb$proption_BigBrands_naics_postal_open * dummies
    names(dummies) = c("Proportion_Low","Proportion_Mid","Proportion_High")
    data_nb = cbind(data_nb,dummies)

    ## Dummy for categorical interaction of proportion of local visitors * IV
    dummies = dummy_cols(data_nb$cutoff) %>% select(-1)
    dummies = data_nb$BrandPostalProp * dummies
    names(dummies) = c("Expsoure_Low","Exposure_Mid","Exposure_High")
    data_nb = cbind(data_nb,dummies)

    print("Created Dummies, Running reg")


    #Model1 OLS Continuous * Categorical
    model1 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip +  proption_BigBrands_naics_postal_open + Visitors_Mid + Visitors_High  + Proportion_Mid + Proportion_High|  newfactor + postal_code | 0 | postal_code,.)

    print(paste("Done 1/4"))

    #Model2 OLS Conitnuous * Continuous
    model2 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip +  proption_BigBrands_naics_postal_open + pct_same_tract + interact |  newfactor + postal_code | 0 | postal_code,.)

    print(paste("Done 2/4"))

    #Model3 IV Continuous * Categorical
    model3<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip + Visitors_Mid + Visitors_High|  newfactor + postal_code  | ( Proportion_Mid + Proportion_High +  proption_BigBrands_naics_postal_open  ~  Exposure_Mid + Exposure_High + BrandPostalProp) | postal_code,.)

    print(paste("Done 3/4"))

    #Model4 IV Continuous * Continuous
    model4<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip + pct_same_tract |  newfactor + postal_code  | (proption_BigBrands_naics_postal_open + interact  ~   BrandPostalProp + interact_expsoure) | postal_code,.)
    #stargazer(model5, type = "text")

    return(list(model1,model3,model2,model4))
    
}


## Function to generate coefficients for loyalty interaction
get_loyalty_coeff = function(data_nb){
    ## Dummy for categorical proportion of loyal visitors
    dummies = dummy_cols(data_nb$loyal) %>% select(-1)
    names(dummies) = c("NotLoyal","Loyal")
    data_nb = cbind(data_nb,dummies)

    ## Dummy for categorical interaction of proportion of loyal visitors * Branch Est. Open
    dummies = dummy_cols(data_nb$loyal) %>% select(-1)
    dummies = data_nb$proption_BigBrands_naics_postal_open * dummies
    names(dummies) = c("Proportion_NotLoyal","Proportion_Loyal")
    data_nb = cbind(data_nb,dummies)

    ## Dummy for categorical interaction of proportion of loyal visitors * IV
    dummies = dummy_cols(data_nb$loyal) %>% select(-1)
    dummies = data_nb$BrandPostalProp * dummies
    names(dummies) = c("Expsoure_NotLoyal","Exposure_Loyal")
    data_nb = cbind(data_nb,dummies)

    print("Created Dummies for Loyal, Running Reg")

    ## OLS
    model1 <- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip +  proption_BigBrands_naics_postal_open + Loyal + Proportion_Loyal |  newfactor + postal_code | 0 | postal_code,.)


    model2<- data_nb %>% felm(open ~  Feb_Avg + prop_home_device_zip + Loyal |  newfactor + postal_code  | ( Proportion_Loyal +  proption_BigBrands_naics_postal_open  ~  Exposure_Loyal + BrandPostalProp) | postal_code,.)

    return(list(model1,model2))

}




data_nb = fread("filedata/data_nb_state.csv") %>% as.data.frame()
data_nb$date = ymd(data_nb$date)

models_local = get_hetro_coeff_3splits(data_nb)


models_loyal = get_loyalty_coeff(data_nb)

screenreg(list(models_local[[1]],models_local[[2]],models_loyal[[1]],models_loyal[[2]]),digits=3, caption = "Regression",caption.above = T, custom.header = list("Model 8"=1:2,"Model 9"=3:4), custom.model.names = c("OLS","IV","OLS","IV"), custom.coef.names = c("Feb_Avg","prop_home_device_zip","Prop. Branch Est. Open","Local Visitors (Med)","Local Visitors (High)","Prop. Branch Est. Open XX Local Visitors (Med)","Prop. Branch Est. Open XX Local Visitors (High)","Prop. Branch Est. Open XX Local Visitors (Med)","Prop. Branch Est. Open XX Local Visitors (High)","Prop. Branch Est. Open","Loyal Customers","Prop. Branch Est. Open XX Loyal Customers","Prop. Branch Est. Open XX Loyal Customers"),reorder.coef=c(3,6,7,9,4,5,8,1,2),custom.gof.rows=list("Fixed Effect Date-NAICS"=c("Yes","Yes","Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes","Yes","Yes")),include.fstatistic = T,table=F)

texreg(list(models_local[[1]],models_local[[2]],models_loyal[[1]],models_loyal[[2]]), file = "tables/table3.tex",digits=3, caption = "Regression",caption.above = T, custom.header = list("Model 8"=1:2,"Model 9"=3:4), custom.model.names = c("OLS","IV","OLS","IV"), custom.coef.names = c("Feb_Avg","prop_home_device_zip","Prop. Branch Est. Open","Local Visitors (Med)","Local Visitors (High)","Prop. Branch Est. Open XX Local Visitors (Med)","Prop. Branch Est. Open XX Local Visitors (High)","Prop. Branch Est. Open XX Local Visitors (Med)","Prop. Branch Est. Open XX Local Visitors (High)","Prop. Branch Est. Open","Loyal Customers","Prop. Branch Est. Open XX Loyal Customers","Prop. Branch Est. Open XX Loyal Customers"),reorder.coef=c(3,6,7,9,4,5,8,1,2),custom.gof.rows=list("Fixed Effect Date-NAICS"=c("Yes","Yes","Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes","Yes","Yes")),include.fstatistic = T,table=F,custom.note = paste("\\item %stars. Note"))


#3 Splits
#screenreg(models,caption = "Moderation",caption.above = T,custom.header=list("Local Visitors(Categorical)"=1:2,"Local Visitors(Continuous)"=3:4),custom.model.names = c("OLS","IV","OLS","IV"),digits=3,custom.coef.names = c("Feb_Avg","prop_home_device_zip","proption_BigBrands_naics_postal_open","Local Visitors (Moderate)", "Local Visitors (High)","Branch Open XX Local Visitors (Moderate)","Branch Open XX Local Visitors (High)", "Branch Open XX Local Visitors (Moderate)","Branch Open XX Local Visitors (High)","pct_same_tract","Branch Open XX Local Visitors","proption_BigBrands_naics_postal_open","Branch Open XX Local Visitors"),custom.gof.rows=list("Fixed Effect Date-NAICS"=c("Yes","Yes","Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes","Yes","Yes")),,reorder.gof = c(1,2,8,9,3,4,5,6,7))

#texreg(models,file = "tables/hetreo_3_withBase.tex",caption = "Moderation",caption.above = T,custom.header=list("Local Visitors(Categorical)"=1:2,"Local Visitors(Continuous)"=3:4),custom.model.names = c("OLS","IV","OLS","IV"),digits=3,custom.coef.names = c("Feb_Avg","prop_home_device_zip","proption_BigBrands_naics_postal_open","Local Visitors (Moderate)", "Local Visitors (High)","Branch Open XX Local Visitors (Moderate)","Branch Open XX Local Visitors (High)", "Branch Open XX Local Visitors (Moderate)","Branch Open XX Local Visitors (High)","pct_same_tract","Branch Open XX Local Visitors","proption_BigBrands_naics_postal_open","Branch Open XX Local Visitors"),custom.gof.rows=list("Fixed Effect Date-NAICS"=c("Yes","Yes","Yes","Yes"),"Fixed Effect PostalCode" = c("Yes","Yes","Yes","Yes")),,reorder.gof = c(1,2,8,9,3,4,5,6,7))

rm(models_local, models_loyal)
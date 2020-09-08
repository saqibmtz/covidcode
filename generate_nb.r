## @descr - Subsetting for local stores 
## @param - Dataset
## @return - Dataset containing only Local Establishments along 
generate_nb = function(filein = "filedata/preRegData_state.csv"){
    data = fread(filein)
    print("Read File")

    brandsTot = length(unique(data$brands[data$big_brands]))
    print(paste("Unique Brands: ",brandsTot))

    big_brands_all = unique(data$brands[data$big_brands]) %>% as.data.frame()
    #fwrite(big_brands_all,"bigBrandsList.csv")

    data_nb = data %>% filter(BrandNational==0)

    data = data %>% group_by(naics_code,postal_code) %>% mutate(haveLocal = ifelse(sum(BrandNational==0)>0,T,F))
    data %>% filter(big_brands) %>% filter(haveLocal) %>% nrow()
    rm(data)

    data_nb = data_nb %>% filter(!is.na(BrandPostalProp))

    data_nb['newfactor'] = paste(data_nb$date,data_nb$naics_code)
    data_nb$newfactor = ifelse(is.na(data_nb$naics_code),NA,data_nb$newfactor)

    data_nb['countyDate'] = paste(data_nb$countyName,data_nb$date)

    fwrite(data_nb,"filedata/data_nb_state.csv")

}
## @descr - Generate Table 1a and Table 2b
generate_summary_table = function(){
    data_nb = fread("filedata/data_nb_state.csv") %>% as.data.frame()
  
    data_nb$date = ymd(data_nb$date)
    
    data_day = data_nb %>% filter(date == end_date)
    
    distc = data_day %>% group_by(postal_code, naics_code) %>% summarise(brands = mean(BigBrandStores))
    ## Aggregating at postal-naics level, value of BigBrandsStores is same for all local est. in postal-naics pair = mean(BigBrandStores)

    print(paste("Total Community Est ",nrow(data_day)))
    print(paste("Total Brand Stores", sum(distc$brands)))

    cross_section_columns_non_char = c("Feb_Avg" ,"pct_visits_same_cbg","loyal")
    table2 <- stargazer(data_day[,cross_section_columns_non_char],summary.stat = c("n","median","mean","sd","min","max"),header = F,out = "tables/table1a.tex",title = "Summary Statistics")


    nb_non_char = c("open","visits_by_date" ,"prop_home_device_zip" ,"postshelter","proption_BigBrands_naics_postal_open","BrandPostalProp" ) 
    table4 <- stargazer(data_nb[,nb_non_char],summary.stat = c("n","median","mean","sd","min","max"),header = F,out = "tables/table1b.tex",title = "Summary Statistics")

    #stargazer(data_nb[,nb_non_char], type = "text", summary.stat = c("n","median","mean","sd","min","max"),header = F,title = "Summary Statistics")

}
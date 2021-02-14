## @desc - Find NAICS essential categories and subset to exclude them 
##         (Essential categories if >70% stores stay open)
## @return - Dataset dropping essential category businesses 
##
subset_naics =  function(data){
    naics_essential = data %>% group_by(naics_code) %>%summarise(percent_closed = sum(closed_or_not)/n())
    naics_essential  = naics_essential  %>% filter(percent_closed<=naics_essentail_cutoff) 
    data = data %>% filter(!(naics_code %in% naics_essential$naics_code))

    data$naics_code_6digit = data$naics_code
    data$naics_code = floor(data$naics_code/(10^naics_digit))

    return(data)

}

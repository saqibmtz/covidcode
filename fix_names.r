## @descr - Helper function to edit Tex Files Generated
fix_names = function(filein){
    textin = readLines(filein)
    textin = gsub("open","Open",textin)
    textin = gsub("visits_by_date","Daily Visits",textin)
    textin = gsub("BrandPostalProp","National Chain Opening Exposure",textin)
    textin = gsub("proption_BigBrands_naics_postal_Open","Prop. Branch Est. Open",textin)
    textin = gsub("Feb_Avg","Avg. February Traffic",textin)
    textin = gsub("prop_home_device_zip","Prop. Devices At Home",textin)
    textin = gsub("postshelterTRUE","Shelter\\ In\\ Place",textin)
    textin = gsub("date","Date",textin)

    textin = gsub("proption\\\\_BigBrands\\\\_naics\\\\_postal\\\\_Open","Prop.\\ Branch\\ Est.\\ Open",textin)
    textin = gsub("\\`","",textin)
    textin = gsub("\\(fit\\)","",textin)
    textin = gsub("Feb\\\\_Avg","Avg.\\ February\\ Traffic",textin)
    textin = gsub("prop\\\\_home\\\\_device\\\\_zip","Prop.\\ Devices\\ At\\ Home",textin)
    textin = gsub("pct\\\\_visits\\\\_same\\\\_cbg","Proportion\\ Local\\ Customers", textin)
    textin = gsub("visits\\\\_by\\\\_date","Daily\\ Visits" ,textin)
    textin = gsub("Num. obs.","Observations" ,textin)
    textin = gsub("Num. groups\\:" ,"Number of Groups\\:",textin)
    textin = gsub("newfactor " ,"Date-NAICS",textin)
    textin = gsub("postal\\\\_code" ,"PostalCode",textin)
    textin = gsub("naics\\\\_code" ,"NAICS",textin)
    textin = gsub("R\\$\\^2\\$" ,"R2",textin)
    textin = gsub("\\(full model\\)" ,"",textin)
    textin = gsub("XX" ,"\\$\\\\times\\$",textin)
    textin = gsub("pct\\\\_same\\\\_tract","Prop. Local Customers",textin)    

    textin = textin[-grep(pattern = "proj", x = textin)]
    textin = textin[-grep(pattern = "p-value", x = textin)]

    writeLines(textin,filein)
}

## @descr - Helper function to edit Tex Files Generated
fix_names_summary = function(filein){
    textin = readLines(filein)
    textin = gsub("open","Open",textin)
    textin = gsub("visits_by_date","Daily Visits",textin)
    textin = gsub("BrandPostalProp","National Chain Opening Exposure",textin)
    textin = gsub("proption_BigBrands_naics_postal_Open","Prop. Branch Est. Open",textin)
    textin = gsub("Feb_Avg","Avg. February Traffic",textin)
    textin = gsub("prop_home_device_zip","Prop. Devices At Home",textin)
    textin = gsub("postshelterTRUE","Shelter\\ In\\ Place",textin)
    textin = gsub("loyal","Prop.\\ Loyal\\ Customers",textin)

    textin = gsub("proption\\\\_BigBrands\\\\_naics\\\\_postal\\\\_Open","Prop.\\ Branch\\ Est.\\ Open",textin)
    textin = gsub("Feb\\\\_Avg","Avg.\\ February\\ Traffic",textin)
    textin = gsub("prop\\\\_home\\\\_device\\\\_zip","Prop.\\ Devices\\ At\\ Home",textin)
    textin = gsub("pct\\\\_visits\\\\_same\\\\_cbg","Prop.\\ Local\\ Customers", textin)
    textin = gsub("visits\\\\_by\\\\_date","Daily\\ Visits" ,textin)
    textin = gsub("Num. obs.","Observations" ,textin)
    textin = gsub("Num. groups\\:" ,"Number of Groups\\:",textin)
    textin = gsub("newfactor " ,"Date-NAICS",textin)
    textin = gsub("postal\\\\_code" ,"PostalCode",textin)
    textin = gsub("naics\\\\_code" ,"NAICS",textin)
    textin = gsub("R\\$\\^2\\$" ,"R2",textin)
    textin = gsub("\\(full model\\)" ,"",textin)
    textin = gsub("XX" ,"\\$\\\\times\\$",textin)
    textin = gsub("pct\\\\_same\\\\_tract","Prop. Local Customers",textin)
    
    writeLines(textin,filein)
}


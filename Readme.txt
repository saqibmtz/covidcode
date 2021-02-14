To generate result tables, add master_06.csv to filedata/ and run main.R


***************
File Structure
***************

covid
    |--rawdata
    |        |
    |        |--uszips.csv (crosswalk between counties and zips)
    |        |--Naics_2017.csv (crosswalk between Naics code and Names of Naics industry)
    |        |--safegraphcensus219.csv (census data. Used in figure B1)
    |        |--safegraphcensus219_VariableNamesFixed.csv (fixed variable names for census data)
    |
    |--filedata
    |          |--master_06.csv (SafeGraph data file) 
    |          |--preRegData_state.csv (Contains processed data for all stores - brand + community + other)
    |          |--cluster_004.csv    (Classification of stores as belonging to a cluster or not)
    |          |--data_nb   (Subset of preRegData.csv. Only includes community stores. Used to generate main table)
    |          |--PostalBrandDictState.csv (Brand-state specific proportion stores open nationally (excluding the focal state))
    |          |--febpatternscbg.csv (Patterns of customers in February)
    |--code 
    |       |-- main.R (Main code files. Calls other code files and functions. Generates Tables in the Paper)
    |       |-- RobustnessTable.r (Generates robustness results in appendix)
    |       |-- eventStudy.R (Generates the event study plot in appendix)
    |       |-- FiguresIV.r (Generates Figure B1 - plots of census covariates with the IV)
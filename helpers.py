#Import Libraries
import os
import pandas as pd
import numpy as np
from datetime import datetime
from multiprocessing import Pool
import gzip
import shutil
from zipfile import ZipFile
import requests
from functools import partial
import censusdata as cd

#### Functions ####

## Function to get a list of all files in a directory:
def get_files(dire):
    files=[]
    directory=os.fsencode(dire)
    for dire_ in os.listdir(directory):
      f_name=os.fsdecode(dire_)
      files.append(dire+f_name)
    return files

## Function to extract .gz files
def unzip(f_name):
    try:
        with gzip.open(f_name, 'rb') as f_in:
            with open(f_name[:-3], 'wb') as f_out:
                shutil.copyfileobj(f_in, f_out)
        os.remove(f_name)
    except:
        print(f_name)
        
## Function to extract .zip files
def extract_zip(dest,root='../rawdata/monthly/'):
    with ZipFile(root+dest, 'r') as zipObj:   
        zipObj.extractall(root+dest[0:-4])
## Function to Read and concat multiples CSVs with a set of required fields
def process_file(fields,fields):
    df_ind=[pd.read_csv(f)[fields] for f in files]
    return df_ind

### Data Extraction ###
def extract():
    ## Monthly Data
    zips= get_files('../../rawdata/monthly/')
    with Pool() as pool:
        pool.map(extract_zip,zips)
def monthlyavg():    
    ## Taking out averages for each month 
    csv_folders= get_files('../../rawdata/monthly/')
    with Pool() as pool:
        csvs=[]
        for folder in csv_folders:
            csvs.append([folder+"/patterns-part1.csv",folder+"/patterns-part2.csv",folder+"/patterns-part3.csv"])
        func=partial(process_file,['safegraph_place_id','date_range_start','raw_visit_counts','raw_visitor_counts'])
        df_monthly=pool.map(func,csvs)
    df_monthly_temp=pd.concat(df_monthly)
    df_monthly_temp.reset_index(drop=True,inplace=True)
    df_monthly_temp['date_range_start']=pd.to_datetime(df_monthly_temp['date_range_start'],unit='s').apply(lambda x:x.strftime('%B,%Y'))
    df_monthly_temp.to_csv('../filedata/visits_data/monthly_avg_visits_data.csv')
def febdaywise():
    ## Feb Daywise Avg ##
    df_feb_p1= pd.read_csv("../../rawdata/monthly/Feb20-AllPatterns-PATTERNS-2020_02-2020-03-2/patterns-part1.csv")
    df_feb_p2= pd.read_csv("../../rawdata/monthly/Feb20-AllPatterns-PATTERNS-2020_02-2020-03-2/patterns-part2.csv")
    df_feb_p3= pd.read_csv("../../rawdata/monthly/Feb20-AllPatterns-PATTERNS-2020_02-2020-03-2/patterns-part3.csv")
    df_feb= pd.concat([df_feb_p1,df_feb_p2,df_feb_p3])
    df_feb= df_feb[['safegraph_place_id', 'visits_by_day' ]]
    df_feb.to_csv("../../rawdata/monthly/Feb20-AllPatterns-PATTERNS-2020_02-2020-03-2/pattern-feb-essential.csv")
    # Calculating Feburary Daywise Average
    df_feb['feb_daywise_avg']=df_feb['visits_by_day']\
    .apply(lambda x: x.strip('][').split(','))\
    .apply(lambda x:[int(val) for val in x])\
    .apply(lambda x: list((np.array(x[0:7])+np.array(x[7:14])+np.array(x[14:21])+np.array(x[21:28])+np.array(x[28:]+[0,0,0,0,0,0]))/np.array([5,4,4,4,4,4,4])))
    df_feb['feb_daywise_avg']=df_feb['feb_daywise_avg'].apply(lambda x:x[1:7]+x[0:1])
    n_weeks=8  # No of weeks we need to replicate
    df_feb['feb_daywise_avg']=df_feb['feb_daywise_avg'].apply(lambda x: x[0:7]*n_weeks)
    df_feb.drop('visits_by_day',axis=1).to_csv('../filedata/febdaywiseavg.csv')
    
## Social Distancing metrics ####
   
def load_file(x):
    a= pd.read_csv(x)
    a['date']=a['date_range_start'].apply(lambda x:pd.to_datetime(x).date())
    a['fips_county']=a['origin_census_block_group'].apply(lambda x: str(x)[:5])
    a=a[['origin_census_block_group', 'date',
       'device_count', 'completely_home_device_count'
       ,'part_time_work_behavior_devices', 'full_time_work_behavior_devices','destination_cbgs',
        'fips_county']]
    return a
def social_distancing_metrics():
    ## Extracting
   
    date_range=pd.date_range(start='3/01/2020', end='4/25/2020')
    date_range=["../rawdata/social-distancing-metrics/2020/"+str(date.month).zfill(2) +"/"+str(date.day).zfill(2)+"/2020-"+ str(date.month).zfill(2)+"-"+str(date.day).zfill(2)+ "-social-distancing.csv" for date in date_range]
    with Pool() as pool:
        pool.map(unzip,date_range)
    df_datebase=[]
    with Pool() as pool:
        df_database=pool.map(load_file,date_range)
    df_compiled=pd.concat(df_database)
    df_compiled.to_csv('../filedata/socialdistancingmetrics/cbgwise.csv',index=False)
    
## Related Brands Feb. #####
def read_files(file):
        return pd.read_csv(file)[['safegraph_place_id','date_range_start','postal_code','brands','related_same_day_brand']]'
def related_brands():
    
  
    df_concat=[]
    with Pool() as pool:
        df_concat=pool.map(read_files,file_list)
    df_concat=pd.concat(df_concat,ignore_index=True)
    df_concat=files.reset_index(drop=True)
    df_concat['related_same_day_brand']=df_concat['related_same_day_brand'].apply(lambda x: list(eval(x).items()))
    df_concat=df_concat.explode('related_same_day_brand')
    df_concat['related_brand']=df_concat['related_same_day_brand'].str[0]
    df_concat['related_brand_pct']=df_concat['related_same_day_brand'].str[1]
    df_concat.drop('related_same_day_brand',axis=1,inplace=True)\
    df_concat.to_csv('../filedata/master_dataset/relatedbrandsfeb.csv')

#### Creating outside inside distribution ####

def pct_visits(x):
        if x['visitor_home_cbgs']:
            if x['poi_cbg'] in x['visitor_home_cbgs'].keys():
                return (x['visitor_home_cbgs'][x['poi_cbg']]/sum(x['visitor_home_cbgs'].values()))
            else:
                return 0
        else:
            return None
def outside_inside():
    files=pd.concat([pd.read_csv('../rawdata/monthly/feb20/patterns-part1.csv')[['safegraph_place_id','visitor_home_cbgs']],pd.read_csv('../rawdata/monthly/feb20/patterns-part2.csv')[['safegraph_place_id','visitor_home_cbgs']],pd.read_csv('../rawdata/monthly/feb20/patterns-part3.csv')])[['safegraph_place_id','visitor_home_cbgs']]
    w1=pd.read_csv('../rawdata/weekly_march/v2/main-file/2020-03-02-weekly-patterns.csv')
    w1=w1[['safegraph_place_id','poi_cbg']]
    files= pd.merge(w1,files,on='safegraph_place_id')
    files['visitor_home_cbgs']=files['visitor_home_cbgs'].apply(eval)
    files['poi_cbg']=files['poi_cbg'].apply(lambda x:str((int(x))))
    files['poi_cbg']=files['poi_cbg'].apply(lambda x:x[:-2])
    
    files['pct_visits_same_cbg']=files.apply(pct_visits,axis=1)
    files=files.drop(['poi_cbg','visitor_home_cbgs'],axis=1)
    files.to_csv('../filedata/master_dataset/samecbgvisits_feb.csv')
    #Same Tract
    files=files[files['poi_cbg'].notna()].reset_index(drop=True)
    files['poi_tract']=files['poi_cbg'].apply(lambda x:str((int(x))).zfill(12)[:-1])
    files['visitor_home_cbgs']=files['visitor_home_cbgs'].apply(lambda x: np.array(x.items()))
    files['visitor_home_cbgs']=files['visitor_home_cbgs'].apply(lambda x: list(x))
    files=files.explode('visitor_home_cbgs')
    files[['visitor_cbg','visitor_count']]=pd.DataFrame(files['visitor_home_cbgs'].tolist(), index=files.index)  
    files=files[files['visitor_cbg'].notna()].reset_index(drop=True)
    files['visitor_tract']=files['visitor_cbg'].apply(lambda x: x[:-1])
    files['same_tract']=files['poi_tract']==files['visitor_tract']
    f=files.groupby('safegraph_place_id').apply(lambda x: x['visitor_count'][x['same_tract']].sum()/x['visitor_count'].sum())
    f.reset_index().rename({0:'pct_same_tract'},axis=1).to_csv('../filedata/master_dataset/sametractvisits_feb.csv')

    ### 


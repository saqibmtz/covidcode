# Import Libraries
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
##### Main file #####
# Concatenating Weekly files
def read_files(file):
    return pd.read_csv(file)[['safegraph_place_id','date_range_start','visits_by_day']]
def closing_to_bool(x):
    b=np.array([False]*56)
    b[x[0]]=True
    return list(b)
def compile_master():
    df_merged=[]
    with Pool() as pool:
        df_merged=pool.map(read_files,file_list)
    n_weeks=len(df_merged)
    df_merged=pd.concat(df_merged,ignore_index=True)
    df_merged['visits_by_day']=df_merged['visits_by_day'].apply(str_to_list)
    df_merged= df_merged.sort_values(by='date_range_start')
    df_merged=df_merged.groupby('safegraph_place_id').apply(lambda x:[element for list_ in x for element in list_]).reset_index()
    # Merging with feb daywise avg 
    df_feb=pd.read_csv('../processed_data/febdaywiseavg.csv')
    df_merged=pd.merge(df_merged,df_feb,on='safegraph_place_id')
    df_merged['feb_daywise_avg']=df_merged['feb_daywise_avg'].apply(lambda x: x[0:7]*n_weeks)
    # Calculating Ratio, Delta and closing dates
    df_merged['coeff']=df_merged['visits_by_date'].apply(np.array)/df_merged['feb_daywise_avg'].apply(np.array)
    w=[1/4]*4
    df_merged['coeff_rolling']=df_merged['coeff'].apply(lambda x: list(np.convolve(np.array(x),w[::-1],"same")))
    z=[1/3,0,0,-1/3]
    df_merged['delta']=df_merged['coeff_rolling'].apply(np.array).apply(lambda x: np.convolve(x,z,'same'))
    ## Algorithm
    df_merged['closing_dates']=df_merged.apply(lambda x:np.where(((np.array(x['delta'])<-0.1)&(0.13<np.array(x['coeff_rolling']))&(0.2>np.array(x['coeff_rolling']))) & (np.array(x['visits_by_date'])<50) | ((np.array(x['coeff_rolling'])<0.13))| (np.array(x['visits_by_date'])<7)),axis=1)

    a['closing_dates']=a['closing_dates'].apply(closing_to_bool)
    a.drop(['coeff_rolling','delta'],axis=1,inplace=True)
    # Adding other constant fields
    w1=pd.read_csv('../../rawdata/weekly_march/v2/main-file/2020-03-02-weekly-patterns.csv')
    w1=w1[['safegraph_place_id','poi_cbg','region', 'brands','postal_code', 'naics_code']]
    df_merged=pd.merge(df_merged,w1, on='safegraph_place_id')
    # Adding County SIP dates
    ## Adding County name
    county= pd.read_csv('../processed_data/closure_data/countystateclosure.csv')
    df_merged['FIPS']=df_merged['poi_cbg'].apply(str).apply(lambda x:x[0:5])

    a=pd.merge(df_merged,county[['FIPS','County','County_Shelter_In_Place_Policy']],on='FIPS',how='left')
    a=a.rename({'county':'countyName'},axis=1).drop('FIPS',axis=1)
    ## exploding the lists i.e. closing_dates,visits_by _day and feb_daywise_avg
    a=a.set_index(['safegraph_place_id', 'region', 'brands','postal_code', 'naics_code','countyName', 'poi_cbg', 'County_Shelter_In_Place_Policy']).apply(pd.Series.explode).reset_index()
    ### Adding date column ###
    start_date='03/01/2020'
    end_date='04/25/2020'
    a['date']=pd.Series(list(pd.date_range(start_date,end_date))*(a.shape[0]/56))

    ### ###
    ### Adding Loyalty ###

    ## Loyalty Calculations
    df_concat= pd.read_csv('../processed_data/master_dataset/relatedbrandsfeb.csv')
    brands_naics=a[['brands','naics_code']].drop_duplicates()
    safegraph_naics=a[['safegraph_place_id','naics_code']].drop_duplicates().reset_index()
    brands_naics=brands_naics[brands_naics['brands'].notna()]
    df_concat=pd.merge(df_concat,safegraph_naics,on='safegraph_place_id',how='right')
    df_concat=pd.merge(df_concat,brands_naics,left_on='related_brand',right_on='brands',suffixes=['store','visited'],how='left')
    df_concat=df_concat.drop(['Unnamed: 0','index','brandsvisited'],axis=1)
    df_concat=df_concat[df_concat['related_brand_pct'].notna()].reset_index(drop=True)
    df_concat['sameindustryvisit']=df_concat['naics_codestore']==df_concat['naics_codevisited']
    pct_same_industry=df_concat.groupby('safegraph_place_id').apply(lambda x: x['sameindustryvisit'].sum())
    pct_visited=df_concat.groupby('safegraph_place_id').apply(lambda x: x['sameindustryvisit'].shape[0])
    pct_same_industry=pct_same_industry.reset_index().rename({0:'loyalty'},axis=1)
    pct_visited=pct_visited.reset_index().rename({0:'total_brands_visited'},axis=1)
    a=pd.merge(a,pct_same_industry,on='safegraph_place_id',how='left')
    a=pd.merge(a,pct_visited,on='safegraph_place_id',how='left')

    ### Adding Social Distancing Metrics ###

    ## Adding to dataset
    cbgwise =pd.read_csv('../processed_data/socialdistancingmetrics/cbgwise.csv')
    cbgwise.drop(['full_time_work_behavior_devices'],axis=1,inplace=True)
    a=pd.merge(a,cbgwise,on=['poi_cbg','date'],how='left',validate="m:1")

    a.to_csv('master_06.csv')
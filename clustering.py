## Code to clusters stores 
## Returns: file with a variable for each store - whether it belongs to a cluster or not.


import numpy as np
import pandas as pd
import os
import seaborn as sns
import matplotlib.pyplot as plt
from scipy import stats
import datetime
from dateutil import parser

import matplotlib.pyplot as plt

from sklearn.cluster import DBSCAN
from sklearn import metrics
from sklearn.datasets import make_blobs
from sklearn.preprocessing import StandardScaler
import seaborn as sns

## Generating a cross section of data
## Getting information on CBGFIPS, Region, State for clustering

data_folder = "../filedata"
file_name = "master_06.csv"

file_in = os.path.join(data_folder,file_name)
data = pd.read_csv(file_in)
data2 = data.drop_duplicates(["safegraph_place_id"])
data.to_csv("../filedata/master_dataset/master_cross_section.csv")

## Reading Lat/Long Info
file_name = "poilocations.csv"
file_in = os.path.join(data_folder,file_name)
data = pd.read_csv(file_in)

data = data.set_index("safegraph_place_id").join(data2.set_index("safegraph_place_id"),lsuffix = "_coord",rsuffix = "_master",how = 'left')
data.to_csv("../filedata/location_complete.csv")

localStores = pd.read_csv("../filedata/localStores.csv")
localStores['inReg'] = True
localStores.head()


### Clustering code  ###

db = DBSCAN(0.0015, min_samples=5).fit(data[["longitude","latitude"]])
labels = db.labels_
print(sum(labels==-1)/len(data))

0.0015*60
#len(db.labels_), sum(db.labels_ == -1)
n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
n_noise_ = list(labels).count(-1)

print('Estimated number of clusters: %d' % n_clusters_)
print('Estimated number of noise points: %d' % n_noise_)
data['inCluster'] = ~(labels==-1) 
data.to_csv("../filedata/cluster_004.csv")
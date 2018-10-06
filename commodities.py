import os
import csv
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

path_str = "/Users/rasaka/Desktop/dataanalysis/whathodata/commodities/comm"
#files = (cereals_df,dairy_df,fish_df,fruits_df,liveanimals_df,meat,vegs)
path = os.fsencode(path_str)

def csv_to_df(file):
    df = pd.read_csv(file)
    df = filter_df(df)
    df = sum_df(df)
    return df

def filter_df(df):
    df = df[df.Flow=='Export']
    df = df[['Trade (USD)' , 'Year' , 'Country or Area']]
    df = df.rename(columns={'Country or Area':'Country'})
    
    return df

def rename_trade(df,name):
    df1=df.rename(columns={'Trade (USD)':name})
    return df1

def sum_df(df):
    df1 = df.groupby(['Country','Year']).sum().reset_index()
    return df1

merge_df = lambda df1, df2 : pd.merge(df1,df2, on=['Country','Year'],how='outer')


mydfs = {}

for file in os.listdir(path_str):
    if file.endswith(".csv"):
        name=os.path.splitext(file)[0]
        mydfs[name]=rename_trade(csv_to_df(os.path.join(path_str,file)),name)
        #print(name)
        
path_ghgas = "/Users/rasaka/Desktop/dataanalysis/whathodata/commodities/greenhousegases.csv"
ghgas_df = pd.read_csv(path_ghgas)
ghgas_df = ghgas_df.rename(columns={'Country or Area':'Country', 'Value':'GHGas'})
#print(list(ghgas_df))

'''
df = merge_df(mydfs['cereals'],mydfs['dairy'])
print(df.head())
'''

all_df = pd.DataFrame(columns=['Country','Year'])
for key in mydfs:
    all_df = merge_df(all_df,mydfs[key])

all_df = merge_df(all_df,ghgas_df)
#print(all_df.head())
                  
corr = all_df.corr()   

sns.heatmap(corr)

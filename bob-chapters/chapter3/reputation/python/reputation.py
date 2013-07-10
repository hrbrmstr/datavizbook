#!/usr/bin/python
#
# reputation.py
#
# sample analysis script for AlienVault IP Reputation Database data
#

# URL for the AlienVault IP Reputation Database (OSSIM format)
# storing the URL in a variable makes it easier to modify later
# if it changes

import urllib
import os.path
import pandas as pd

avURL = "http://reputation.alienvault.com/reputation.data"

# relative path for the downloaded data
avRep = "data/reputation.data"

# using an if-wrapped test with urllib.urlretrieve() vs direct read
# via panads avoids having to re-download a 16MB file every time we
# run the script

if not os.path.isfile(avRep):
    urllib.urlretrieve(avURL, filename=avRep)

# read in the data into a pandas data frame
av = pd.read_csv(avRep,sep="#")

# take a quick look at the dat
#print(av)

# assign more readable column names to make it easier to work with the data 
# IP | reliability | risk | type | country | locale | coords | x
av.columns = ["IP","Reliability","Risk","Type","Country",
              "Locale","Coords","x"]
#print av

# summary_col(col)
# 
# helper function to mimic R's "summary()" function
#
def summary_col(col):
    factor = pd.Categorical.from_array(col)
    return pd.value_counts(factor,sort=True).reindex(factor.levels)

print summary_col(av['Reliability'])
print summary_col(av['Risk'])
print summary_col(av['Type'])
print summary_col(av['Country'])

# f_reliability = pd.Categorical.from_array(av['Reliability'])
# print pd.value_counts(f_reliability)

# f_risk = pd.Categorical.from_array(av['Risk'])
# print pd.value_counts(f_risk)

# f_type = pd.Categorical.from_array(av['Type'])
# print pd.value_counts(f_type)

# f_country = pd.Categorical.from_array(av['Country'])
# print pd.value_counts(f_country)


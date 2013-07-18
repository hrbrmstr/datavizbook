#!/usr/bin/python
#
# ch3.py
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

os.chdir("/Users/n0179200/Dropbox/datavizbook/bob-chapters/chapter3/ch3")
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
print(av)

# assign more readable column names to make it easier to work with the data 
# IP | reliability | risk | type | country | locale | coords | x
av.columns = ["IP","Reliability","Risk","Type","Country",
              "Locale","Coords","x"]
#print av

av.head(10)

av['Reliability'].describe()
av['Risk'].describe()

# factor_col(col)
# 
# helper function to mimic R's "summary()" function
# for pandas "columns" (which are really just Python
# arrays)
#
def factor_col(col):
    factor = pd.Categorical.from_array(col)
    return pd.value_counts(factor,sort=True).reindex(factor.levels)

print factor_col(av['Reliability'])
print factor_col(av['Risk'])
print factor_col(av['Type'])
print factor_col(av['Country'])

barcol = "#762A83"

# We want the country counts sorted
country_ct = pd.value_counts(av['Country'])
country_ct[:20].plot(kind='bar', rot=0, color=barcol, title="Summary By Country");
factor_col(av['Reliability']).plot(kind='bar', rot=0, color=barcol, title="Summary By Reliability")
factor_col(av['Risk']).plot(kind='bar', rot=0, color=barcol, title="Summary By Risk")


# see percentages of top 10 'bad' countries
top10 = pd.value_counts(av['Country'])[0:9] 
top10.astype(float) / len(av['Country'])

# compute contingency table for Risk/Reliability
pd.crosstab(av['Risk'], av['Reliability'])

# graphical view of contingency table
pd.crosstab(av['Reliability'], av['Risk']).plot(kind='bar')


data = { 'rsk': randint(1, 7, 260000), 
         'rel': randint(1, 10, 260000) }
tmp_df = pd.DataFrame(data, columns=['rsk', 'rel'])

pd.crosstab(tmp_df['rel'], tmp_df['rsk']).plot(kind='bar')

# "medium" reliability and "medim" (and above) risk
len(av[(av['Reliability']>4) & (av['Risk']>3)])

# what makes these nodes "risky"?
risky = av[(av['Reliability']>4) & (av['Risk']>3)]
print pd.value_counts(risky['Type'])

# make each entry an array
tmp = risky['Type'].str.split(';')

# expand the series
tmp = tmp.apply(lambda x: pd.Series(x)).unstack()

# join expanded series with original daa frame
r2 = risky.join(pd.DataFrame(s.reset_index(level=0, drop=True)));

# remove the NaN's
risky = r2[pd.notnull(r2[0])]

pd.value_counts(risky[0])



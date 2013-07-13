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

os.chdir("/Users/n0179200/Dropbox/datavizbook/bob-chapters/chapter3/reputation")
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

from scipy.stats import mode
mode(av['Reliability'])
mode(av['Risk'])

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

# "medium" reliability and "medim" (and above) risk
len(av[(av['Reliability']>4) & (av['Risk']>3)])

# what makes these nodes "risky"?
risky = av[(av['Reliability']>4) & (av['Risk']>3)]
print pd.value_counts(risky['Type'])

# retrieve IANA prefix list
ianaURL = "http://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.csv"
ianaData = "data/ipv4-address-space.csv"

if not os.path.isfile(ianaData):
    urllib.urlretrieve(ianaURL, filename=ianaData)

iana = pd.read_csv(ianaData)

iana # examine it

# clean up the iana prefix
iana['Prefix'] = iana['Prefix'].map(lambda x: str(int(x.rstrip("8").rstrip("/"))))
iana['Prefix'] # examine it

# extract just the prefix from the AlienVault list
avPrefix = [ octet[0] for octet in av['IP'].str.split('.') ]

av['Designation'] = [ iana[(iana['Prefix'] == prefix)].Designation 
                      for prefix in avPrefix ]

pd.value_counts(av['Designation'])



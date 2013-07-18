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
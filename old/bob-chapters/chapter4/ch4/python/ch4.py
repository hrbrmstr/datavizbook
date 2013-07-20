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

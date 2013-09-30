#!/usr/bin/python

import csv

netflow_files = [ "../data/vast/week1/nf/nf-chunk1.csv",
                    "../data/vast/week1/nf/nf-chunk2.csv",
                    "../data/vast/week1/nf/nf-chunk3.csv" ]

interval = 60*5 # 60 seconds * 5 minutes: five minutes
interval = 60*60 # 60 seconds * 60 minutes: 1 hour

out = {}

for netflow_file in netflow_files:
    print "Parsing", netflow_file
    netflow = csv.DictReader(open(netflow_file))
    for row in netflow:
        curtime = int(float(row['TimeSeconds']))
        # which 5 minute interval are we talking
        # int will take floor
        series = curtime / interval   
        ip1 = row['firstSeenSrcIp']
        ip2 = row['firstSeenDestIp']
        bytes1 = int(row['firstSeenSrcPayloadBytes'])
        bytes2 = int(row['firstSeenDestPayloadBytes'])
        if not '.0.' in ip1:
            if not out.has_key(ip1):
                out[ip1] = {}
            if not out[ip1].has_key(series):
                out[ip1][series] = {}
                out[ip1][series]['sent'] = 0
                out[ip1][series]['recv'] = 0
            out[ip1][series]['sent'] += bytes1
            out[ip1][series]['recv'] += bytes2
        if not '.0.' in ip2:
            if not out.has_key(ip2):
                out[ip2] = {}
            if not out[ip2].has_key(series):
                out[ip2][series] = {}
                out[ip2][series]['sent'] = 0
                out[ip2][series]['recv'] = 0
            out[ip2][series]['sent'] += bytes2
            out[ip2][series]['recv'] += bytes1

# if I had more time, we should do this ALL in python
c = csv.writer(open("other-1hr-ts.csv", "wb"))

c.writerow([ 'ip', 'ts', 'sent', 'recv' ])
for ip in out.keys():
    for ts in sorted(out[ip], key=out[ip].get):
        c.writerow([ ip, ts, out[ip][ts]['sent'], out[ip][ts]['recv'] ])

#!/usr/bin/python

from datetime import datetime

today = datetime.now()
bookdue = datetime(today.year,2,17)
if (bookdue - today).days > 1:
    print "%d days until book is released." % (bookdue - today).days
else:
    print "The data is here!!"

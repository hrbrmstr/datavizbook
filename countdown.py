#!/usr/bin/python

from datetime import datetime

today = datetime.now()
bookdue = datetime(today.year,11,12)
if (bookdue - today).days > 1:
    print "%d days until book is due." % (bookdue - today).days
else:
    print "You are done, right?!"

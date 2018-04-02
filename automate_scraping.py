import scrape as scrape
import metadata as meta
import os
import datetime
import sys

# edit these three variables
user = 'nytimes' # Twitter account to scrape
if len(sys.argv) > 1:
    user = sys.argv[1].lower()
years = [2006 + x for x in range(13)] # years to scrape

def getTwoMonthIntervals(year):
    if year % 4 == 0:
        two_month_intervals = [
        (datetime.datetime(year, 1,  1), datetime.datetime(year, 2,  29)),
        (datetime.datetime(year, 3,  1), datetime.datetime(year, 4,  30)),
        (datetime.datetime(year, 5,  1), datetime.datetime(year, 6,  30)),
        (datetime.datetime(year, 7,  1), datetime.datetime(year, 8,  31)),
        (datetime.datetime(year, 9,  1), datetime.datetime(year, 10, 31)),
        (datetime.datetime(year, 11, 1), datetime.datetime(year, 12, 31))]
    else:
        two_month_intervals = [
        (datetime.datetime(year, 1,  1), datetime.datetime(year, 2,  28)),
        (datetime.datetime(year, 3,  1), datetime.datetime(year, 4,  30)),
        (datetime.datetime(year, 5,  1), datetime.datetime(year, 6,  30)),
        (datetime.datetime(year, 7,  1), datetime.datetime(year, 8,  31)),
        (datetime.datetime(year, 9,  1), datetime.datetime(year, 10, 31)),
        (datetime.datetime(year, 11, 1), datetime.datetime(year, 12, 31))]
    return two_month_intervals

if __name__ == "__main__":
    for year in years:
        i=0
        intervals = getTwoMonthIntervals(year)
        for interval in intervals:
            if not os.path.isfile("./data/{}_{}_{}.csv".format(user,i, year)):
                start, end = interval
                scrape.run(user, start, end)
                meta.run(user, "{}_{}".format(i, year))
                os.remove('./{}_all_ids.json'.format(user))
            else:
                print(user+"_{}_{}.csv already exists".format(i, year))
            i += 1

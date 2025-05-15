"""
Script for generation a dataset of sunset times in San Francisco
from Janury 1st, 2007 to December 31st, 2015
"""
import csv
from datetime import date, timedelta
from astral import LocationInfo
from astral.sun import sun
import pytz

# Setting the city information
city = LocationInfo("San Francisco", "USA", "America/Los_Angeles", 37.7749, -122.4194)

# Set the date range
start_date = date(2007, 1, 1)
end_date = date(2015, 12, 31)

# Prepare the CSV file
with open('san_francisco_sunrise_sunset_2007_2015.csv', 'w', newline='') as csvfile:
    fieldnames = ['Date', 'Sunrise', 'Sunset']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()

    current_date = start_date
    while current_date <= end_date:
        # Get the sunrise and sunset times
        s = sun(city.observer, date=current_date, tzinfo=pytz.timezone(city.timezone))
        sunrise_time = s['sunrise']
        sunset_time = s['sunset']

        writer.writerow({
            'Date': current_date.strftime('%Y-%m-%d'),
            'Sunrise': sunrise_time.strftime('%H:%M:%S'),
            'Sunset': sunset_time.strftime('%H:%M:%S')
        })

        current_date += timedelta(days=1)

print("CSV file 'san_francisco_sunrise_sunset_2007_2015.csv' has been created.")
# Read in data
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

# Print file
system("cat dates.json")

# Call packages
library(jsonlite)

library(tidyverse)

# Convert JSON to list
mylist <- fromJSON('dates.json')

# Convert list to a data frame
mydf <- bind_rows(mylist$result[-1])

# What type of object is mydf?
print(class(mydf))

# What type of object is mydf$date?
print(class(mydf$date))

# List first 10 rows of mydf
print(head(mydf, 10))

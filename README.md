# Scripts to examine reported data and generated estimates in the global TB database

*(Mainly using faceted visualisations so we can scan across all countries looking for anomolies)*

## use

1. Make sure you have your version of `get_environment.r` set up with a database connection string and where outpute files will be saved.

2. Then, in R, to produce notification graphs as a PDF, `> source("review_notifications.r")`. Likewise, source `review_IPT.r` for IPT graphs and `review_TBHIV_ART.r` for HIV-positive TB and ART graphs.


## Output

Each script generates a single PDF file containing the same graph, faceted for each country. I found 16 countries per PDF page is about right for legibility.
# EDAV_Final_Project
EDAV Course Project of 311 Data

## Analysis of Data Quality

```{r}
library(tidyverse)
library(ggplot2)

setwd("/Users/dc/Dropbox/A_Courses/Exploratory_Data_Analysis/EDAV_Project")
visna_data <- read_csv("visna.csv")

library(extracat)
visna(visna_data, sort = "b")

data_mi <- read_csv("mi.csv")
library(mi)
x <- missing_data.frame(data_mi)
image(x)
```

## Exploratory Data Analysis


All those records for which any of the missing date values are excluded from the master 2.3 million dataset.  

With the cleaned version of the dataset built by removing missing values, NAs etc.,  we have explored performance of each agency as a whole and perormance for each complaint type pertinent to the agency. To measure performance, we have created a factor called "efficiiency factor". It is defined as the ratio of the time in which the complaint has been due to the time in which the complaint has been resolved. A factor above 1 of this efficiency factor implies, complaints are being resolved within the stipulated time. To calculate this, we have first measured `hrs_diff_due` and `hrs_diff_closed` as below:

`hrs_diff_due` = `Due.Date` - `Created.Date`

`hrs_diff_closed` = `Closed.Date` - `Created.Date`

`efficiency_factor` = `hrs_diff_due`/`hrs_diff_closed`

Building on tidy concept, 3 dataframes have been built 

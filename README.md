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


* First, containing every agency's average overall efficieny
```{r}
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
setwd("/Users/dc/Dropbox/A_Courses/Exploratory_Data_Analysis/EDAV_Project")
#bar plot of agency efficiency 
efficiency_agency <- read_csv("efficiency_agency.csv")
library(knitr)
kable(efficiency_agency[1:5,], caption = "Agency - Average Efficiency")
```
* Second, containing every agency's monthly average efficiency

```{r}
efficiency_agency_month <- read_csv("efficiency_agency_month.csv")
kable(efficiency_agency_month[ceiling(runif(10, 0, 100)),], caption = "Agency - Monthly Average Efficiency")
```

* Third, complaint type of each agency's monthly average efficiency. 


```{r}
efficiency_agency_complaint_month <- read_csv("efficiency_agency_complaint_month.csv")
kable(efficiency_agency_complaint_month[ceiling(runif(10, 0, 100)),], caption = "Complaint Type - Monthly Average Efficiency")
```


With the cleaned and tidy dataframes, we've plotted column chart of average efficiency for each agency. Below the code and plot are shown.

```{r, fig.align="center"}

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
setwd("/Users/dc/Dropbox/A_Courses/Exploratory_Data_Analysis/EDAV_Project")
#bar plot of agency efficiency 
efficiency_agency <- read_csv("efficiency_agency.csv")
ggplot(efficiency_agency, aes(Agency, mean_efficiency)) + geom_col() + scale_y_log10() +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +labs(x = "Agency", y = "Average Efficiency")+ggtitle("Average Efficiency (log scale)")
```

We can see that `Department of Health and Mental Hygiene` agency has highest efficiency and `Economic Development Corporation` has the least efficiency.

Further, we have plotted line charts of all agencies monthly average efficiency variation. Due to high variation of values, we have ecmplyed log scale. From the chart plotted below, the variation looks quite random and uninferentiable. So, we excluded `DOHMH` and plotted another plot.

```{r, fig.align="center"}
#line chart efficiency of agency over time
efficiency_agency_month <- read_csv("efficiency_agency_month.csv")

ggplot(efficiency_agency_month, aes(created_month, mean_efficiency, color = Agency)) + geom_line() +
  ggtitle("Efficiency for each Agency") +
  labs (x = "Month", y = "Efficiency - log scale") +
  scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
    theme_grey(16) +
  theme(legend.title = element_blank()) + scale_y_log10()
```

After excluding `DOHMH`

```{r, fig.align="center"}
#excluding DOHMH
efficiency_agency_month_modified <- efficiency_agency_month %>% filter(Agency != c("DOHMH"))

ggplot(efficiency_agency_month_modified, aes(created_month, mean_efficiency, color = Agency)) + geom_line() +
  ggtitle("Efficiency for each Agency") +
  labs (x = "Month", y = "Efficiency") +
  scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
theme_grey(16) +
  theme(legend.title = element_blank())
```


After excluding `DOHMH`,`DOT`,`DOB`,`DPR`
```{r, fig.align="center"}
#Excluding DOHMH,"DOT","DOB","DPR"

efficiency_agency_month_modified <- efficiency_agency_month %>% filter(!Agency %in% c("DOHMH","DOT","DOB","DPR"))

ggplot(efficiency_agency_month_modified, aes(created_month, mean_efficiency, color = Agency)) + geom_line() +
  ggtitle("Efficiency for each Agency") +
  labs (x = "Month", y = "Efficiency") +
  scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
theme_grey(16) +
  theme(legend.title = element_blank())
```

Only Showing `DOT`,`DOB`,`DPR`:

```{r, fig.align="center"}
#Only show "DOT","DOB","DPR"

efficiency_agency_month_modified <- efficiency_agency_month %>% filter(Agency %in% c("DOT","DOB","DPR"))

ggplot(efficiency_agency_month_modified, aes(created_month, mean_efficiency, color = Agency)) + geom_line() +
  ggtitle("Efficiency for each Agency") +
  labs (x = "Month", y = "Efficiency") +
  scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
theme_grey(16) +
  theme(legend.title = element_blank())

```



After throrough exploration of above plots, it seems more granular analysis of `Complaint.Type` of each `Agency` might throw more analysis on the variation of efficiency. So, we have plotted each agency's Complaint Type vs Monthly efficiency factor in the below plots.  

As the efficiency factors vary a lot, we had employed a log_scale for y-axis which showed clear overall view for some charts and not for some. 

```{r, fig.width=10,fig.height=60}
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
efficiency_agency_complaint_month <- read_csv("efficiency_agency_complaint_month.csv")
agency_list = c("DCA",
               "DFTA",
               "DOB",
               "DOE",
               "DOHMH",
               "DOITT",
               "DOT",
               "DPR",
               "DSNY",
               "EDC",
               "FDNY",
               "NYPD",
               "TLC")
pltList <- vector("list", length(agency_list))
for(s in 1:length(agency_list))
{
  efficiency_agency_complaint_month_filtered <- efficiency_agency_complaint_month %>% 
    filter(Agency == agency_list[s])
  pltList[[s]] <- ggplot(efficiency_agency_complaint_month_filtered, 
                               aes(created_month, mean_efficiency, color = Complaint.Type)) + 
                          geom_line() + ggtitle(paste("Efficiency of Agency:",agency_list[s], "for each Complaint Type")) + labs (x = "Month", y = "Efficiency") +
        scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
                          theme_grey(12) +
                          theme(legend.title = element_blank(),legend.position="bottom")+
    guides(col = guide_legend(nrow = 5)) + scale_y_log10()
}
myList <- list()

# Now the new experiments
for(i in 1:length(agency_list)){
  myList[[length(myList)+1]] <- pltList[[i]]
}

#myList
cowplot::plot_grid(plotlist = myList, ncol = 1)
```


Many agencies such as `DCA`, `DOB`, `DOE`, `DOITT`, `DSNY`, and `EDC` have only complaint type in their coverage. Wheras `NYPD`,`DOT`, and `FDNY` have highest complaint types. One thing to notice here is that, all these complaints are of non-emergency. Especially, agencies `NYPD` and `FDNY` have to cater to both emergency and non-emergency services and at the same time maintaining overall good efficiency factors. 

These charts further directed us to explore the reasons behind such high efficiency factors during certain months for some agency comaplaint types. 

Using the above cleaned data, in the same fashion how we grouped efficiency factors, we've grouped total number of records to understand number of complaints each agency is receiving. 

Again 3 dataframes are created with each one's description as below:

* First, containing every agency's total count of complaint records
```{r}
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
setwd("/Users/dc/Dropbox/A_Courses/Exploratory_Data_Analysis/EDAV_Project")
#bar plot of agency efficiency 
count_agency.csv <- read_csv("count_agency.csv")
library(knitr)
kable(count_agency.csv[1:5,], caption = "Agency - Count")
```

* Second, containing every agency's monthly count of complaint records

```{r}
count_agency_month <- read_csv("count_agency_month.csv")
kable(count_agency_month[ceiling(runif(10, 0, 100)),], caption = "Agency - Monthly Count")
```

* Third, complaint type of each agency's monthly count of complaint records


```{r}
count_agency_complaint_month <- read_csv("count_agency_complaint_month.csv")
kable(count_agency_complaint_month[ceiling(runif(10, 0, 100)),], caption = "Complaint Type - Monthly Count")
```

Similar to average efficiency factor, we have analysed total complaint records at increasing granular level. Below set of plots show the same:

```{r, fig.align="center"}
#bar plot of agency count 
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
count_agency <- read_csv("count_agency.csv")
ggplot(count_agency, aes(Agency, mean_count)) + geom_col() + theme_grey(12)+
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
      labs(x = "Agency", y = "Count (log scale)") + scale_y_log10() +
  ggtitle("Total Count of Agency Complaints")
```


```{r, fig.align="center"}
#line chart count of agency over time
count_agency_month <- read_csv("count_agency_month.csv")

ggplot(count_agency_month, aes(created_month, mean_count, color = Agency)) + geom_line() +
  ggtitle("Count for each Agency") +
  labs (x = "Month", y = "Count - log scale") +
  scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
    theme_grey(16) +
  theme(legend.title = element_blank()) + scale_y_log10()
```

As hypothesized, `NYPD`, `DOT` and `FDNY` are in the top quartile of total complaints received. 

Below set of charts show distribution of each agency's complaint type aggreagted over each month.

```{r, fig.width=10, fig.height=60}
#agency complaint types count
count_agency_complaint_month <- read_csv("count_agency_complaint_month.csv")
agency_list2 = c("DCA",
                "DFTA",
                "DHS",
                "DOB",
                "DOE",
                "DOHMH",
                "DOITT",
                "DOT",
                "DPR",
                "DSNY",
                "EDC",
                "FDNY",
                "NYPD",
                "TLC")

pltList2 <- vector("list", length(agency_list2))
for(s in 1:length(agency_list2))
{
  count_agency_complaint_month_filtered <- count_agency_complaint_month %>% 
    filter(Agency == agency_list2[s])
  pltList2[[s]] <- ggplot(count_agency_complaint_month_filtered, 
                         aes(created_month, mean_count, color = Complaint.Type)) + 
    geom_line() + ggtitle(paste("Complaint Count of Agency:",agency_list2[s], "for each Complaint Type")) + labs (x = "Month", y = "Count") +
    scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
    theme_grey(12) +
    theme(legend.title = element_blank(),legend.position="bottom")+
    guides(col = guide_legend(nrow = 5))
}

myList2 <- list()

# Now the new experiments
for(i in 1:length(agency_list2)){
  myList2[[length(myList2)+1]] <- pltList2[[i]]
}

#myList
cowplot::plot_grid(plotlist = myList2, ncol = 1)
```

On prelimnary analysis of above charts, we see that there is some correlation between number of complaints received to efficiency factor. So, to further explore I built 5 individual functions with each fucntion and its actions are described below:

* This fucntion takes 3 inputs, an agency vector, a complaint_type vector with the list of complaint types that needs to be excluded under consideration from agency and rows for plotting legend. Given these input parameters, this function plots a line chart showing `log (efficiency_factor)` of agency with excluded complaint type over time

```{r}
#plots log efficiency of certain agency with given complaint_type excluded
effi_plot <- function(agency, complaint_type,rows){
efficiency_agency_complaint_month_filtered <- efficiency_agency_complaint_month %>% 
  filter(Agency %in% agency) %>% filter(!Complaint.Type %in% complaint_type)
ggplot(efficiency_agency_complaint_month_filtered, 
                             aes(created_month, mean_efficiency, color = Complaint.Type)) + 
                        geom_line() + ggtitle(paste("Efficiency of Agency:",agency[1], "for each Complaint Type")) + labs (x = "Month", y = "Efficiency (log scale)") +
      scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
                        theme_grey(12) +
                        theme(legend.title = element_blank(),legend.position="bottom")+
  guides(col = guide_legend(nrow = rows)) + scale_y_log10()
}
```



* This fucntion takes 3 inputs, an agency vector, a complaint_type vector with the list of complaint types that needs to be excluded under consideration from agency and rows for plotting legend. Given these input parameters, this function plots a line chart showing `log (count)` of agency with excluded complaint type over time

```{r}
#plots log count of certain agency with given complaint_type excluded
count_plot <- function(agency, complaint_type,rows){
count_agency_complaint_month_filtered <- count_agency_complaint_month %>% 
  filter(Agency %in% agency) %>% filter(!Complaint.Type %in% complaint_type)
ggplot(count_agency_complaint_month_filtered, aes(created_month, mean_count, color = Complaint.Type)) + geom_line() + ggtitle(paste("Complaint Count of Agency:",agency[1], "for each Complaint Type")) + labs (x = "Month", y = "Count  (log scale)") +
  scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
  theme_grey(12) +
  theme(legend.title = element_blank(),legend.position="bottom")+
  guides(col = guide_legend(nrow = rows)) + scale_y_log10()
}
```



* This fucntion takes 3 inputs, an agency vector, a complaint_type vector with the list of complaint types that needs to be included under consideration from agency and rows for plotting legend. Given these input parameters, this function plots a line chart showing `log (efficiency_factor)` of agency with included complaint type over time

```{r}
#plots log efficiency of certain agency for given complaint_type only included
effi_compl_plot <- function(agency,complaint_type,rows){
  efficiency_agency_complaint_month_filtered <- efficiency_agency_complaint_month %>% 
 filter(Complaint.Type %in% complaint_type)
ggplot(efficiency_agency_complaint_month_filtered, 
                             aes(created_month, mean_efficiency, color = Complaint.Type)) + 
                        geom_line() + ggtitle(paste("Efficiency of Agency:",agency[1], "for few Complaint Types")) + labs (x = "Month", y = "Efficiency (log scale)") +
      scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
                        theme_grey(12) +
                        theme(legend.title = element_blank(),legend.position="bottom")+
  guides(col = guide_legend(nrow = rows)) + scale_y_log10()
}

```


* This fucntion takes 3 inputs, an agency vector, a complaint_type vector with the list of complaint types that needs to be included under consideration from agency and rows for plotting legend. Given these input parameters, this function plots a line chart showing `log (count)` of agency with included complaint type over time



```{r}
#plots log count of certain agency for given complaint_type only included
count_compl_plot <- function(agency,complaint_type,rows){

count_agency_complaint_month_filtered <- count_agency_complaint_month %>% 
filter(Complaint.Type %in% complaint_type)
ggplot(count_agency_complaint_month_filtered, aes(created_month, mean_count, color = Complaint.Type)) + geom_line() + ggtitle(paste("Complaint Count of Agency:",agency[1], "for few Complaint Types")) + labs (x = "Month", y = "Count (log scale)") +
  scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
  theme_grey(12) +
  theme(legend.title = element_blank(),legend.position="bottom")+
  guides(col = guide_legend(nrow = 5)) + scale_y_log10()
}
```


* This function takes input of agency vector, complaint_type vector and a scaling factor. Output of the fucntion is a plot of 2 lines which utilizes secondary y-axis. On primary y-axis we showed `log(efficiency factor)` of the given complaint type and on the secondary y-axis we showed `total count` of complaint type records for given complaint type. Scaling factor is utlized to scale both the line charts onto the plot


```{r}
#2 line plot with 2 different y-axis
#showing efficiency vs count of given complaint type
count_vs_effi <- function(agency,complaint_type,scaling_factor){
efficiency_agency_complaint_month_filtered <- efficiency_agency_complaint_month %>% 
  filter(Complaint.Type %in% complaint_type)

count_agency_complaint_month_filtered <- count_agency_complaint_month %>% 
  filter(Complaint.Type %in% complaint_type)

ggplot(efficiency_agency_complaint_month_filtered, aes(x = created_month))+
  geom_line(aes(y = log2(efficiency_agency_complaint_month_filtered$mean_efficiency), color = "Mean Efficiency"))+
  geom_line(aes(y = count_agency_complaint_month_filtered$mean_count/scaling_factor, color = "Count"))+
  scale_y_continuous(sec.axis = sec_axis(~.*(scaling_factor), name = "Count"))+
  scale_colour_manual(values = c("red", "blue"))+
  scale_x_continuous( breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), label = c("Jan", "Feb","Mar", "Apr","May",  "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec"))+
  ggtitle(paste("Complaint Count of",agency[1], "for",complaint_type[1], "Complaint"))+
  labs(y = "Efficiency (log values)",
      x = "Month",
      colour = "Index")+
    theme_grey(12)+
  theme(legend.title = element_blank(),legend.position="bottom")+
  guides(color = guide_legend(reverse = TRUE))
}
```

`DOHMH` agency without `Food poisoning` complaint type:

```{r, fig.width=10,fig.height=6}
#DOHMH agency without Food Poisoning complaint type - efficiency
effi_plot(c("DOHMH"),c("Food Poisoning"),5)
```


```{r, fig.width=10,fig.height=6}
#DOHMH agency without Food Poisoning complaint type - count
count_plot(c("DOHMH"),c("Food Poisoning"),5)
```

`DOHMH` agency with `Food poisoning` complaint type:

```{r, fig.width=10,fig.height=6}
#DOHMH Food Poisoning complaint type - efficiency
effi_compl_plot(c("DOHMH"),c("Food Poisoning"),1)

```



```{r, fig.width=10,fig.height=6}
#DOHMH agency Food Poisoning complaint type - count
count_compl_plot(c("DOHMH"),c("Food Poisoning"),1)

```


`DOHMH` had effectively dealt with complaints of `Food Poisning` even though complaints seem to be increasing. 

```{r, fig.align="center"}
#DOHMH: food poisoning efficiency vs count over time
count_vs_effi(c("DOHMH"),c("Food Poisoning"),20)
```


`FDNY` agency without `Public Assembly` complaint type:


```{r, fig.width=10,fig.height=6}
#FDNY agency without Public Assembly complaint type - efficiency
effi_plot(c("FDNY"),c("Public Assembly"),5)
```

```{r, fig.width=10,fig.height=6}
#FDNY agency without Public Assembly complaint type - count
count_plot(c("FDNY"),c("Public Assembly"),5)
```

`FDNY` agency with `Public Assembly` complaint type:

```{r, fig.width=10,fig.height=6}
#FDNY Public Assembly complaint type - efficiency
effi_compl_plot(c("FDNY"),c("Public Assembly"),1)

```

```{r, fig.width=10,fig.height=6}
#FDNY agency Public Assembly complaint type - count
count_compl_plot(c("FDNY"),c("Public Assembly"),1)

```

`FDNY` agency's `Public Assembly` complaint type dual line plot is not showing any significant correaltion between efficiency and count of complaints

```{r, fig.align="center"}
#FDNY: Public Assembly efficiency vs count over time
count_vs_effi(c("FDNY"),c("Public Assembly"),0.8)
```


`FDNY` performance hovering for non-emergency complaint type `Fire Safety Director - F58` with number of number of complaints received. The complaint type is about issuing a certificate of fitness - F58 for any person in the building which has more than 100 occupants.


```{r, fig.align="center"}
#FDNY: Public Assembly efficiency vs count over time
count_vs_effi(c("FDNY"),c("Fire Safety Director - F58"),100)
```


From above 2 plots, we could hypothesize that `FDNY` is equally prioritizing non-emergency services with emergency services. Had we  have data on emergency services, we could make confident statement on performance of both the services of `FDNY`


EDC Noise - Helicopter complaint type - efficiency


```{r, fig.width=10,fig.height=6}
#EDC Noise - Helicopter complaint type - efficiency
effi_compl_plot(c("EDC"),c("Noise - Helicopter"),1)

```

EDC agency Noise - Helicopter complaint type - count

```{r, fig.width=10,fig.height=6}
#EDC agency Noise - Helicopter complaint type - count
count_compl_plot(c("EDC"),c("Noise - Helicopter"),1)

```

Clearly, with increase in complaint types, the performance of `EDC` has fallen. They both show a negative correlation. 

```{r, fig.align="center"}
#EDC: Noise - Helicopter efficiency vs count over time
count_vs_effi(c("EDC"),c("Noise - Helicopter"),80)
```


### GEO HEATMAP

### Let's try to answer following questions
1.To find out the Complaints counts distribution across a day in each Borough.

2.To find out how complaints are distributed spatially across a day?

3.At a given time in a day from where are we getting more complaints?

### procedure 

1.We plotted spatial heatmap of complaints count for each hour of a day on a geography map.

2.Complaints with null values for the any of the following features were not considered for plotting the graph. 

3.Features used to plot this graph:

a)Time of a day at which complaint is filed.

b)Latitude point of the complaint.

c)Longitude point of the complaint 

4.Used ggmap and ggplot2 library to plot this graph and plotted the heatmap for every hour and saved those figures. Created Animated PNG file from the saved graphs.

### Import

```{r, eval= FALSE}
library(ggplot2)
library(ggmap)
sub_heat_data <- read.csv("sub_heat_data.csv", header = TRUE, sep = ",")
gc <- geocode("New York")
map <- get_map(gc, maptype = "roadmap", zoom = 11)
```

### Plot

```{r, eval= FALSE}
for (i in 0:23){
  day_heat <- sub_heat_data[sub_heat_data$hour == i,]
  ggmap(map) + 
    geom_density2d(data = day_heat,
                   aes(x = Longitude, y = Latitude), size = 0.3) + 
    stat_density2d(data = head(day_heat, 50000),
                   aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                   bins = 20, geom = "polygon") +
    scale_fill_gradient(low = "blue", high = "red") +
    scale_alpha(range = c(0, 0.3), guide = FALSE) +
    labs(title=paste("Hour:", i), cex.names=0.75)
  ggsave(filename = paste("start", i, ".png", sep = ""), width = 4, height = 4, dpi = 200)
}
```

![](ezgif.com-apng-maker.png)

### Observations:

1.Most number of the complaints in a day are coming from 5:00 AM to 8:00 AM 

2.Least number of the complaints in a day are coming from 10:00 PM to 2:00 AM

3.Staten Island has very few number of complaints at a given hour in a day compared to Manhattan, Queens, Bronx, and Brooklyn.

4.Bronx has complaints across all hours in a day.

5.Manhattan downtown, Bronx and central Brooklyn has highest number of complaints across all hours in a day.

### Inference:

1.Population density is one of the key factors for less number of complaints in Staten Island and more complaints in Lower 
Manhattan, Central Brooklyn, and Bronx.

2.Let's look at parallel coordinate plot to get more detailed inference of the complaints in a day across Boroughs. 

## Parallel Coordinate Plots

Here we load the needed libraries and read the data.

```{r}
#setwd("~/Downloads/edav project")
data2 = read.csv("MyData.csv")
library(parcoords)
library(GGally)
library(ggplot2)
library(dplyr)
library(plyr)
```

  - We sample the data and create additional columns of month and hour.     
  - Since hour is in 12-hour format with AM/PM tag, we do additional processing to convert it to 24 hour format. Once we get the hour variable, we group the data in 4 groups with 6 hour long time chunks: 12am - 6am, 6am - 12pm, 12pm - 6pm and 6pm - 12am respectively.

```{r}
data = data2[1:10000,]
data$month = substr(as.character(data$Created.Date),1,2)
data$hour = substr(as.character(data$Created.Date),12,13)
data$day_night = substr(as.character(data$Created.Date),21,22)
data$hour2=0
for (i in 1:10000){
  if (data$day_night[i] == "AM"){
    data$hour2[i] = as.numeric(data$hour[i])
  }
  else{
    data$hour2[i] = as.numeric(data$hour[i]) + 12
  }
}
data$period = floor((data$hour2 - 1)/6)
data$TimeOfDay = "none"
for (i in 1:nrow(data)){
  val = data$period[i] 
  if(val == 0){
    data$TimeOfDay[i] = "12am to 6am"
  }
  if(val == 1){
    data$TimeOfDay[i] = "6am to 12pm"
  }
  if(val == 2){
    data$TimeOfDay[i] = "12pm to 6pm"
  }
  if(val == 3){
    data$TimeOfDay[i] = "6pm to 12am"
  }
}
```

- Here we subset the data to only take the fields we are curious about and the following code is repeated for different types of parallel coordinates plot across. 
- Next among different categories of agencies / complaint types, we sort on most reported. 
- We break the frequency of complaints in 5 buckets. Sometimes no data maps to a particular bucket as R breaks it in even buckets and a particular category type increases the maximum value. The more the bucket value, the higher the complaints. 
- Also the reason of not plotting more than 3 variables in parallel coordinate plot is due to cluttered overlapping names with the lines. 
- Lets first check if there is anything we can find out how complaints are generated across different agencies and what time of day.

```{r}
sample = data[,c(6,36)]
c = data.frame(table(sample[1]))
c = c[order(c$Freq, decreasing = TRUE),]
select = as.character(c$Var1[1:30])
sample2 = sample[(as.character(sample$Agency.Name) %in% select),]
sample3 = count(sample2, c("Agency.Name","TimeOfDay"))
sample3$bucket = cut(sample3$freq, breaks = 5, labels = FALSE)
sample3$bucket = as.character(sample3$bucket)
sample3$freq=NULL

parcoords(
  sample3
  , rownames = F # turn off rownames from the data.frame
  , brushMode = "1D-axes-multi"
  , reorderable = T
  , queue = T,
  color = list(
    colorBy = "bucket"
    ,colorScale=htmlwidgets::JS('d3.scale.category10()'))
  , width = 650
)

```

Inference -- We see that the highest number of complaints are between 6am to 12pm time for the Department of Hosuing Preservation and Development, followed by complaints between 6pm to 12am for the NYPD. This ordering makes sense since people genreally report all the housing complaints when the day gets started and feel unsafe at night when they probably travel back to home.

Next we look at the distribution of complaints types across different hour of day.

```{r, echo = FALSE}
sample = data[,c(7,36)]
c = data.frame(table(sample[1]))
c = c[order(c$Freq, decreasing = TRUE),]
select = as.character(c$Var1[1:30])
sample2 = sample[(as.character(sample$Complaint.Type) %in% select),]
sample3 = count(sample2, c("Complaint.Type","TimeOfDay"))
sample3$bucket = cut(sample3$freq, breaks = 5, labels = FALSE)
sample3$bucket = as.character(sample3$bucket)
sample3$freq=NULL

parcoords(
  sample3
  , rownames = F # turn off rownames from the data.frame
  , brushMode = "1D-axes-multi"
  , reorderable = T
  , queue = T,
  color = list(
    colorBy = "bucket"
    ,colorScale=htmlwidgets::JS('d3.scale.category10()'))
  , width = 650
)

```

Inference -- Quite obviously we see the complaints of HEAT/HOT WATER to be highest in the mornings followed by the NOISE - RESIDENTIAL complaints at night.

Next we look at which complaints are most reported across different boroughs.

```{r, echo = FALSE}
sample = data[,c(7,23)]
c = data.frame(table(sample[1]))
c = c[order(c$Freq, decreasing = TRUE),]
select = as.character(c$Var1[1:30])
sample2 = sample[(as.character(sample$Complaint.Type) %in% select),]
sample3 = count(sample2, c("Complaint.Type","Borough"))
sample3$bucket = cut(sample3$freq, breaks = 5, labels = FALSE)
sample3$bucket = as.character(sample3$bucket)
sample3$freq=NULL

parcoords(
  sample3
  , rownames = F # turn off rownames from the data.frame
  , brushMode = "1D-axes-multi"
  , reorderable = T
  , queue = T,
  color = list(
    colorBy = "bucket"
    ,colorScale=htmlwidgets::JS('d3.scale.category10()'))
  , width = 650
)

```

Inference -- We see that most complaints reported are from BROOKLYN and BRONX for HEAT/HOT WATER. This tells us the HEAT/HOT WATER problem is quite prevalent among these boroughs. We also see problems of NOISE-RESIDENTIAL are also high in MANHATTEN and BRONX.

Next we look at which agencies are most busy across different boroughs.

```{r, echo = FALSE}
sample = data[,c(6,23)]
c = data.frame(table(sample[1]))
c = c[order(c$Freq, decreasing = TRUE),]
select = as.character(c$Var1[1:30])
sample2 = sample[(as.character(sample$Agency.Name) %in% select),]
sample3 = count(sample2, c("Agency.Name","Borough"))
sample3$bucket = cut(sample3$freq, breaks = 5, labels = FALSE)
sample3$bucket = as.character(sample3$bucket)
sample3$freq=NULL

parcoords(
  sample3
  , rownames = F # turn off rownames from the data.frame
  , brushMode = "1D-axes-multi"
  , reorderable = T
  , queue = T,
  color = list(
    colorBy = "bucket"
    ,colorScale=htmlwidgets::JS('d3.scale.category10()'))
  , width = 650
)

```

Inference -- We note that NYPD in BROOKLYN and Housing Preservation and Development in BROOKLYN and BRONX are the most busy agencies. They are followed by Department of Transportation in Queens ( due to JFK airport transport probably, as people get near airport they panick about missing flights) and NYPD in BRONX.


## Heatmaps

We employ heat maps to study which particular generla locations report highest number of complaints across different Boroughs. This requires aggregating different types of similar locations like `'Residence', 'RESIDENTIAL BUILDING', 'Residential', 'Residential Building', 'Residential Building/House'` in one. After we have done that, we skip the records where location is not reported and plot heatmap usiing ggplot.

```{r pressure, echo=FALSE}
sample10 = data[1:1200,c(23,9)]
sample10$freq = 1
aggregate_sample = aggregate(sample10$freq, by =list(sample10$Borough, sample10$Location.Type), FUN = sum)
aggregate_sample$Locations = "Not Reported"
aggregate_sample$Group.2 = as.character(aggregate_sample$Group.2)

for (i in 1:nrow(aggregate_sample)){
  if(aggregate_sample$Group.2[i] %in% c('Residence', 'RESIDENTIAL BUILDING', 'Residential', 'Residential Building', 'Residential Building/House')){
    aggregate_sample$Locations[i] = "Residential"
  }
  else if(aggregate_sample$Group.2[i] %in% c('3+ Family Apartment Building', '3+ Family Apt. Building', '3+ Family Mixed Use Building')){
    aggregate_sample$Locations[i] = "3+ Family Apartment"
  }
  else if(aggregate_sample$Group.2[i] %in% c('1-2 Family Dwelling', '1-2 Family Mixed Use Building', '1-3 Family Dwelling')){
    aggregate_sample$Locations[i] = "1-3 Family Apartment"
  }
  else if(aggregate_sample$Group.2[i] %in% c('Street','Street Address','Street and Sidewalk','Street/Curbside', 'Street/Sidewalk')){
    aggregate_sample$Locations[i] = "Streets"
  }
  else if(aggregate_sample$Group.2[i] ==""){
    aggregate_sample$Locations[i] = "Not Reported"
  }
  else{
    aggregate_sample$Locations[i] = as.character(aggregate_sample$Group.2[i])
  }
}

aggregate_sample = aggregate_sample[-which(aggregate_sample$Locations == "Not Reported"),]

aggregate_sample$Locations = factor(aggregate_sample$Locations)
ggplot(aggregate_sample, aes(Group.1, Locations )) +
  geom_tile(aes(fill = x), color = "white") +
  coord_equal() +
  scale_fill_gradient(low = "grey", high = "orange") +
  ylab("Locations") +
  xlab("Boroughs") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "x")
```

Inference -- We see that most of the complaints originate on Streets and Residential Area. Brooklyn and Queens seems to top the chart for that. 

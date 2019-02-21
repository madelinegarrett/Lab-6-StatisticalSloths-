# Lab-6-StatisticalSloths-
---
title: "Lab6_StatisticalSloths"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
COflights <- read_csv("https://files.osf.io/v1/resources/3z5as4pej8/providers/osfstorage/5a8ca28f57103100104584db")
summary(COflights)
```
# Team Section 
Question 1: Determine what factors increase the probability of a delayed arrival and decrease the probability of a delayed arrival.





Question 2: Are more flights delayed because of the carrier, weather, NAS, security issues, or aircraft late in arriving causing delays in departures?

```{r}
COflights %>%
  filter(!is.na(DEP_DELAY)) %>%
  filter(DEP_DELAY>0) %>%
  summarise(total=n())

COflights %>%
  filter(!is.na(CARRIER_DELAY)) %>%
  filter(CARRIER_DELAY>0) %>%
  summarise(carrier=n())

COflights %>%
  filter(!is.na(WEATHER_DELAY)) %>%
  filter(WEATHER_DELAY>0) %>%
  summarise(weather=n())

COflights %>%
  filter(!is.na(NAS_DELAY)) %>%
  filter(NAS_DELAY>0) %>%
  summarise(nas=n())

COflights %>%
  filter(!is.na(SECURITY_DELAY)) %>%
  filter(SECURITY_DELAY>0) %>%
  summarise(security=n())

COflights %>%
  filter(!is.na(LATE_AIRCRAFT_DELAY)) %>%
  filter(LATE_AIRCRAFT_DELAY>0) %>%
  summarise(late_aircraft=n())
```

## Madeline's Individual Section 
Does Leaving Denver on a Specific Carrier's Flight Increase the Probability That Your Flight Will Be Delayed?

Of all the flights that are delayed that leave from Denver
There are 41,376 delayed flights total, of these delayed flights.
* AA: 1,893/41,376 == 0.04575116 -> 4.5%

* AS: 301/41376 == 0.007274749 -> 0.72%

* B6: 364/41376 == 0.00879737 -> 0.87%

* DL: 1339/41376 == 0.03236176 -> 3.2%

* EV: 0/41376 == 0 -> 0%

* F9: 4420/41376 == 0.1068252 -> 10.6%

* NK: 827/41376 == 0.01998743 -> 1.9%

* OO: 7665/41376 == 0.1852523 -> 18.5%

* UA: 9148/41376 == 0.2210944 -> 22.1% 

* VX: 268/42376 ==  0.006324335 -> 0.63%

* WN: 15151/42376 == 0.3575373 -> 35.7%

Madeline's Findings: Of the flights that were delayed, it appeared that a majority of those flights were flights that were United Airlines and Southwest. The least amount of delays were from EV and AS and B6 and VX.  

```{r}
delay_carrier_CO <- COflights %>%
  filter(ORIGIN=="DEN") %>%
  group_by(CARRIER) %>%
  filter(ARR_DELAY >= 15)

ggplot(data=delay_carrier_CO) + 
  geom_bar(mapping = aes(x=CARRIER, fill=as.factor(CARRIER)), alpha = .5) +
  ggtitle("Distribution of Late Arrivals From Carriers") + 
  xlab("Carriers") + 
  ylab("Number of Late Arrivals")

ggplot(data=delay_carrier_CO) + 
  geom_bar(mapping = aes(x=MONTH, fill=as.factor(CARRIER)))+
  ggtitle("Distribution of Late Arrivals From Carriers") + 
  xlab("Carriers") + 
  ylab("Number of Late Arrivals")
```

## Kevin's Individual Section
How does the probability of a departure delay from DEN caused by weather change based on the month?

(Flights delayed by weather in month/Total flights in month)
* January: 164/17030 = 0.96%
* February: 122/15533 = 0.79%
* March: 55/18659 = 0.35%
* April: 63/17704 = 0.36%
* May: 103/19122 = 0.54%
* June: 119/19901 = 0.60%
* July: 128/20643 = 0.62%
* August: 204/20891 = 0.97%
* September: 23/18831 = 0.12%
* October: 38/19181 = 0.20%
* November: 24/17627 = 0.13%
* December: 127/18043 = 0.70%

Findings: The months with the highest probablities of having a weather delay are August, January, February, and December. Seasonally speaking, the winter and summer months have higher probabilities of experiencing delays due to weather, whereas the spring and fall months have relatively low probabilities.

```{r}
delay_month_CO <- COflights %>%
  filter(ORIGIN=="DEN") %>%
  group_by(MONTH) %>%
  filter(WEATHER_DELAY>=15)

ggplot(data=delay_month_CO) + 
  geom_histogram(mapping = aes(x=MONTH, fill=as.factor(MONTH)), binwidth = 1) + 
  ggtitle("Monthly Weather Delays") + 
  xlab("Month") + 
  ylab("# of flights") + 
  scale_fill_discrete(name = "Month", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
```

## Katie's Individual Section
Does the distance between Denver and the origin airport effect arrival delays?
(Flights delayed by 15 minutes or longer that had an origin over ___ miles away / total flights that arrived in Denver from that distance)
250 Miles: 33954/214479 = 0.1583092       
500 Miles: 30796/191562 = 0.1607626
1000 Miles: 10780/64615 = 0.1668343

## Zandy's Individual Section
How does the probabily of a delayed flight from Denver or a delayed flight to Denver change based on the scheduled arrival time of the plane and/pr the scheduled departure time of the plane.


## Team Summary 
* I, Madeline Garrett, worked to determine which airline had the most delayed departures from Denver. I used the count function to find a collection of probabilities and found that the airline with the most delayed departures was Southwest. I used the geom bar function and made two plots that should help to illustrate which airlines had the most delays. I labeled both axis and header and use the alpha function on one of the graphs.

* I, Kevin Luth, determined which months had higher probabilities of a flight being delayed by weather-related causes. I found that August had the highest chance, followed by Janurary, February, and December. I used the geom_histogram function to make a plot displaying the number of delayed flights due to weather by month. I changed the title and labels to make their representation clearer. I also changed the color of each month to make it easier to distinguish between them. Finally, I used the scale_fill_discrete function to change the labels for the legend colors to the month names to make it easier to interpret the graph.

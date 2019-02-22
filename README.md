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
```{r}
count(COflights, DEST == "DEN", MONTH == 1, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH ==1, ARR_DELAY >= 15)
3738/16704
count(COflights, DEST == "DEN", MONTH == 2, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 2, ARR_DELAY >= 15)
2159/15409
count(COflights, DEST == "DEN", MONTH == 3, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 3, ARR_DELAY >= 15)
3023/18394
count(COflights, DEST == "DEN", MONTH == 4, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 4, ARR_DELAY >= 15)
2443/17609
count(COflights, DEST == "DEN", MONTH == 5, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 5, ARR_DELAY >= 15)
3268/19017
count(COflights, DEST == "DEN", MONTH == 6, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 6, ARR_DELAY >= 15)
3491/19796
count(COflights, DEST == "DEN", MONTH == 7, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 7, ARR_DELAY >= 15)
3773/20458
count(COflights, DEST == "DEN", MONTH == 8, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 8, ARR_DELAY >= 15)
3927/20590
count(COflights, DEST == "DEN", MONTH == 9, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 9, ARR_DELAY >= 15)
1945/18660
count(COflights, DEST == "DEN", MONTH == 10, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 10, ARR_DELAY >= 15)
2461/19071
count(COflights, DEST == "DEN", MONTH == 11, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 11, ARR_DELAY >= 15)
1464/17582
count(COflights, DEST == "DEN", MONTH == 12, ARR_DELAY != "NA")
count(COflights, DEST == "DEN", MONTH == 12, ARR_DELAY >= 15)
3261/17990
```
* January: 0.2237787 -> 22.37%
* February: 0.1401129 -> 14.01%
* March: 0.1643471 -> 16.43%
* April: 0.1387359 -> 13.87%
* May: 0.1718462 -> 17.18%
* June: 0.1763488 -> 17.63%
* July: 0.1844266 -> 18.44%
* August: 0.1907237 -> 19.07%
* September: 0.1042337 -> 10.42%
* October: 0.1290441 -> 12.90%
* November: 0.08326698 -> 8.33%
* December: 0.1812674 -> 18.13%




Question 2: Are more flights delayed because of the carrier, weather, NAS, security issues, or aircraft late in arriving causing delays in departures?

```{r}
team2q <- COflights %>%
  mutate(typeC=case_when(CARRIER_DELAY>0~"car")) %>%
  mutate(typeW=case_when(WEATHER_DELAY>0~"wea")) %>%
  mutate(typeN=case_when(NAS_DELAY>0~"nas")) %>%
  mutate(typeS=case_when(SECURITY_DELAY>0~"sec")) %>%
  mutate(typeL=case_when(LATE_AIRCRAFT_DELAY>0~"late")) %>%
  filter(!is.na(typeC) | !is.na(typeW) | !is.na(typeN) | !is.na(typeS) | !is.na(typeL))

ggplot(data=team2q) + 
  geom_bar(mapping=aes(x=typeC, fill=typeC)) + 
  geom_bar(mapping=aes(x=typeW, fill=typeW)) + 
  geom_bar(mapping=aes(x=typeN, fill=typeN)) + 
  geom_bar(mapping=aes(x=typeS, fill=typeS)) + 
  geom_bar(mapping=aes(x=typeL, fill=typeL)) + 
  ggtitle("Number of Delay Types") + 
  xlab("Delay Type") + 
  ylab("# of flights") + 
  scale_fill_discrete(name = "Delay", labels = c("Carrier", "Late Aircraft", "NAS", "Security", "Weather"))

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
Probabilities for Delays by: Carrier Delay, Weather Delay, NAS Delay, Security Delay, and Aircraft Delay
* Carrier Delay = 41209/163855 = 0.2515 = 25.15%
* Weather Delay = 3725/163855 = 0.0227 = 2.27%
* NAS Delay = 44416/163855 = 0.271 = 27.1%
* Security Delay = 135/163855 = 0.000824 = 0.0824%
* Aircraft Delay = 43470/163855 = 0.265 = 26.5%

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
* 250 Miles: 33954/214479 = 0.1583092 -> 15.83%       
* 500 Miles: 30796/191562 = 0.1607626 -> 16.07%
* 1000 Miles: 10780/64615 = 0.1668343 -> 16.63%
* 1500 Miles: 4721/25058 = 0.1884029 -> 18.84%

Katie's Findings: As the distance of the origin airport from Denver increases, the probability that that flight will be delayed by at least 15 minutes increases.delay 

```{r}
ggplot(data = COflights) +
  geom_jitter(mapping = aes(x= DISTANCE, y= ARR_DELAY))+
  ggtitle("Distance from Denver vs Delayed Arrival")+
  xlab("Distance")+
  ylab("Arrival Delay")
```

## Zandy's Individual Section
 Does the probabily of a delayed flight(by departure time) from Denver increase based on the delay of the planes arrival time in Denver.

## Team Summary 
* I, Madeline Garrett, worked to determine which airline had the most delayed departures from Denver. I used the count function to find a collection of probabilities and found that the airline with the most delayed departures was Southwest. I used the geom bar function and made two plots that should help to illustrate which airlines had the most delays. I labeled both axis and header and use the alpha function on one of the graphs.

* I, Kevin Luth, determined which months had higher probabilities of a flight being delayed by weather-related causes. I found that August had the highest chance, followed by Janurary, February, and December. I used the geom_histogram function to make a plot displaying the number of delayed flights due to weather by month. I changed the title and labels to make their representation clearer. I also changed the color of each month to make it easier to distinguish between them. Finally, I used the scale_fill_discrete function to change the labels for the legend colors to the month names to make it easier to interpret the graph.

* I, Katie Stewart, took probabilites to determine if the distance of an airport from Denver would affect airport delays. I found that as the distance from Denver increases so does the arrival delay time. I used the geom_jitter function to show my findings. In addition to the plot, I added a title so it would be easily understood as well as changing the x and y titles.  

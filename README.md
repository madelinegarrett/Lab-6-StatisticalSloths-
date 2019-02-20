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



## Madeline's Individual Section 
Does Leaving Denver on an American Airlines Flight Increase the Probability That Your Flight Will Be Delayed?

```{r}
filter(COflights, !is.na(ARR_TIME), ORIGIN == 'DEN',  ARR_DELAY >= 15)
# There are 41,376 flights that leave from Denver and arrived delayed total for the year 
```

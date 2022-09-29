# MOHD-AQDAS-PORTFOLIO
An aspiring Data Analyst

## "Cyclistic Case Study:Analysing the usage for Customer Growth"
Author: Mohd Aqdas

Date: 2022-09-23

Data Source:- Cyclistic

Cyclistic Given Scenario
In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are tracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.

I will conduct my Analysis based on 6 steps:-
Ask

Prepare

Process

Analyze

Share

Act

## First Step- Ask

### Defining Business Task
There are two types of customers, casual riders and members. Lily Moreno (The director of marketing) came up with a hypothesis: Converting casual riders into members would lead to more profitable growth.

There are three questions which will lead us through the analysis and back the hypothesis:-

How do annual members and casual riders use Cyclistic bikes differently?

Why would casual riders buy Cyclistic annual memberships?

How can Cyclistic use digital media to influence casual riders to become members?

### Key stakeholders
Lily Moreno: The director of marketing and your manager. Moreno is responsible for the development of campaigns and initiatives to promote the bike-share program. These may include email, social media, and other channels.

Cyclistic marketing analytics team: A team of data analysts who are responsible for collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy

Cyclistic executive team: The notoriously detail-oriented executive team will decide whether to approve the recommended marketing program.

## Second step- Prepare
In this step, we will download and store the data in an organized structure. Then we will sort the data for further analysis.
### MY code-
,,,r

#Installing the packages
install.packages('tidyverse')
install.packages('readr')
install.packages('lubridate')
install.packages("dplyr")
install.packages("magrittr")

#Loading the packages
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)

#Adding a name <- Importing the csv(file_location)
Jan2022 <- read_csv("Divvy_Tripdata/2022_01.csv")
Feb2022 <- read_csv("Divvy_Tripdata/2022_02.csv")
Mar2022 <- read_csv("Divvy_Tripdata/2022_03.csv")
Apr2022 <- read_csv("Divvy_Tripdata/2022_04.csv")
May2022 <- read_csv("Divvy_Tripdata/2022_05.csv")
Jun2022 <- read_csv("Divvy_Tripdata/2022_06.csv")
Jul2022 <- read_csv("Divvy_Tripdata/2022_07.csv")
Aug2022 <- read_csv("Divvy_Tripdata/2022_08.csv")

#str(dataset_name)
colnames(Jan2022)
colnames(Feb2022)
colnames(Mar2022)
colnames(Apr2022)
colnames(May2022)
colnames(Jun2022)
colnames(Jul2022)
colnames(Aug2022)

#Creating new dataset name <- binding rows(all_your_datasets)

merged_data <- bind_rows(Jan2022, Feb2022, Mar2022, Apr2022, May2022, Jun2022, Jul2022, Aug2022)

head(merged_data)
,,,,,

## Third Step- Process
In this step, we will clean the dataset and assure its integrity.

Code:-
,,,r
#Cleaning the Dataset

#Fixing Column Types
str(merged_data)

merged_data$started_at <- as.POSIXct(merged_data$started_at, format = "%Y-%m-%d %H:%M:%S")
merged_data$ended_at <- as.POSIXct(merged_data$ended_at, format = "%Y-%m-%d %H:%M:%S")
str(merged_data)


#Removing NA/null values
cleaned_df_1 <- na.omit(merged_data)

#Adding Ride Length Column
cleaned_df_2 <- mutate(cleaned_df_1, ride_length = difftime(ended_at, started_at, units = "mins"))
str(cleaned_df_2)

#To calculate the number of observations with negative ride length
nrow(cleaned_df_2[cleaned_df_2$ride_length < 0,])


## We use the ! to NOT show observations where ride_length < 0.
cleaned_df_3 <- cleaned_df_2[!cleaned_df_2$ride_length < 0,]
head(cleaned_df_3, 5)

#Adding Month and Day and Hour Columns


cleaned_df_4 <- mutate(cleaned_df_3, trip_month = format(as.POSIXct(started_at), "%B-%y"), trip_day = format(as.POSIXct(started_at),"%A"), trip_hour = format(as.POSIXct(started_at),"%H"))
head(cleaned_df_4,3)

#write.csv(cleaned_df_4, file = "cleaned_df_4.csv")
,,, 


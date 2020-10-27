# Overview

This repo contains code and data for forecasting the US 2020 presidential election. It was created by Haili Su and Xingyu Yu. The purpose is to create a report that summarizes the results of a statistical model that we built for forecasting. Some data are unable to be shared publicly. The way to get them is detailed below. The sections of this repo are: inputs, outputs, scripts.

Inputs contain data that are unchanged from their original. We use two datasets: 

- The June 25th, 2020 survey results of Democracy Fund + UCLA Nationscape project are the individual survey level data we use. The data can be obtained through: https://www.voterstudygroup.org/publication/nationscape-data-set ;
- The 2018 American Community Survey results are used for post-stratification. The data is obtained from IPUMS USA (https://usa.ipums.org/usa/index.shtml). We are unable to share the raw ACS dataset with selected variable due to its file size. 

Outputs contain data that are modified from the input data, the report and supporting material.

- In /data folder:
- surevy.csv is the survey data from Nationscape after cleaning and reorganizing;
- cells_post.csv is the cells data frame from the ACS dataset;
- In /paper folder:

Scripts contain R scripts that take inputs and outputs and produce outputs. These are:

- 01_data_cleaning.R
- 02_data_preparation.R





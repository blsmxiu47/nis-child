# Sample R analysis: NIS -- Child Immunizations ((1994*)--2020)

## Summary

The NIS (National Immunization Surveys) are annually conducted phone surveys used to monitor vaccination coverage among children 19–35 months and teens 13–17 years, flu vaccinations for children 6 months–17 years, and COVID-19 vaccination for children and teens in eligible age groups and for adults 18 years and older (CDC, [About the National Immunization Surveys (NIS)](https://www.cdc.gov/vaccines/imz-managers/nis/about.html)).

In this analysis repository we look at the 2020 NIS - Child survey responses. In particular we aim to further investigate and visualize the findings of [this report](https://www.cdc.gov/mmwr/volumes/70/wr/mm7041a1.htm) based on responses to the 2020 NIS - Child, for children born in 2017 and 2018.


## Contents

| Name                                   | Description                                                                       |
| -------------------------------------- | --------------------------------------------------------------------------------- |
| [`README.md`](./README.md)             | A description and guide for this code repository                                  |
| [`00_data_prep`](./00_data_prep)       | Directory containing R code to prepare raw data for analysis and modeling         |
| [`01_analysis`](./01_analysis)         | Directory containing R code performing analysis of data from the CDC's NIS        |
| [`02_models`](./02_models)             | Directory containing R code generating models based on data from the CDC's NIS    |
| [`03_notebooks`](./03_notebooks)       | Directory containing markdown notebooks to demonstrate findings of the analysis   |
| [`04_html_outputs`](./04_html_outputs) | Directory containing outputs generated from the analysis                          |
| [`05_img`](./05_img)                   | Directory containing image files used by/generated from this code base            |


*An additional purpose of this repo will be to investigate trends over time. The NIS go back to 1994, but for now this repo looks at 2020 results pending historical data access and synthesis.

# Digital Addiction: Analysis of How Usage of Social Media Impact Objective Well-Being And Political Opinion

## Overview

## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from [replication package](https://www.openicpsr.org/openicpsr/project/112081/version/V1/view?flag=follow&pageSelected=1&pageSize=10&sortOrder=(?title)&sortAsc=true) from AEA. Some of the data was exceeds the upload size limit of GitHub. To get those data, go to [Getting raw data](#getting-raw-data)
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage

Aspects of the code were written with the help of the auto-complete tool, Chat-GPT4 and the entire chat history is available in inputs/llms/usage.txt.

## Getting raw data
Go to the [replication package](https://www.openicpsr.org/openicpsr/project/112081/version/V1/view?flag=follow&pageSelected=1&pageSize=10&sortOrder=(?title)&sortAsc=true), then go into `confidential/main_experiment/output`. Download `baseline_anonymous.dta` and `sms_anonymous.dta`, then put it into `data/raw_data` in your GitHub repo.

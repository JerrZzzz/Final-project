# Strategic Optimization in Formula 1

We have investigated the relationship between the lap time for the first, second and third qualifying sessions. We focus on the relationship of the 11th fastest driver in first qualifying session and the second qualifying session to predict the elimination time for second qualifying session from 2006 to 2023 in Formula one history. We also focused on estimating the pole position time using driver's previous lap time. We found that there is a corrlation between both second qualifying elimination time and pole position time and driver's previous lap times. These information plays a significant role for helping team win the races and giving more exposures to sponsors which will economically benefit the teams. 

## File Structures

The repo is structured as:

- `data` includes the raw data where we download from Kaggle and the data we cleaned in analyze data file.
- `model` contains fitted models in .rds.
- `paper` contains the papers and the datasheet for understanding raw data, including the Quarto document for both paper and datasheet and reference bibliography file, as well as the PDFs.
- `scripts` contains the R scripts used to simulate data, download data, clean data, test data, modeling data, graphing data and an application to understand the data. 
- `other` contains the literature we used in our paper, the llm usages and sketches where we expect what the data should be. 

## LLM Usages

In this paper, codes are mainly written by generating AI like Chatgpt-4 to help us creating graphs, tables and models. The full chat history can be found in llm folder with an llm.txt.

## Downloading Data & Run Codes

To download raw data from Kaggle, Login to Kaggle with email and click Kaggle on the top left corner, then click your profile photo, then click setting. Finally, click create new token under API category. Put the downloaded folder under the main directory and run the code which will download the zip file and automatically unzip it to the directory. 
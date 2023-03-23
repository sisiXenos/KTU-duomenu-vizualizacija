library(tidyverse)
library(jsonlite)

# JSON
download.file("https://atvira.sodra.lt/imones/downloads/2023/monthly-2023.json.zip", "temp")
unzip("temp")
list.files(getwd())
df <- fromJSON("monthly-2023.json")
file.remove("temp")
file.remove("monthly-2023.json")

# Lab Data
data <- read_csv("../../laboratorinis/data/lab_sodra.csv")

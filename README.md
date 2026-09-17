# Data Analysis Thesis Report

Statistical processing and graph generation scripts in R developed for the analysis of telemetry data and operational metrics of the IoT system.

---

## Repository Content

* **`analysis.R`**: Script focused on the analysis of environmental variables recorded by sensors (temperature, humidity, air quality index, and internal vs. external thermal comparisons using OpenMeteo).

* **`devices.R`**: Script oriented towards infrastructure diagnostics and monitoring (Edge node health, Hub message volume, RAM consumption, and continuous stability/uptime).

---

## Requirements and Installation

To run the scripts, you need **R (>= 4.2)** and the following packages:

```r
install.packages(c(
"ggplot2",
"dplyr",
"readr",
"lubridate",
"scales",
"patchwork",
"plotly",
"htmlwidgets"
))
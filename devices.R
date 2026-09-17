library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(scales)
library(patchwork)

df_me <- read_csv("/home/franco/metric.csv")
df_mo <- read_csv("/home/franco/monitor.csv")
df_me$timestamp <- ymd_hms(df_me$timestamp, tz = "America/Buenos_Aires")
df_mo$timestamp <- ymd_hms(df_mo$timestamp, tz = "America/Buenos_Aires")

df_me <- df_me %>% filter(year(timestamp) == 2026)
df_mo <- df_mo %>% filter(year(timestamp) == 2026)


#///////////////////////
# Analisis del Edge
#///////////////////////
samp = "60 minutes"
days = "5 days"

df_ram_total <- df_me %>% select(timestamp, ram_used_mb)
df_ram_used <- df_me %>% select(timestamp, ram_used_by_service_mb)

df_ram_total <- df_ram_total %>%
  mutate(timestamp_s = floor_date(timestamp, unit = samp)) %>%
  group_by(timestamp_s) %>%
  summarise(
    ram_promedio = mean(ram_used_mb, na.rm = TRUE),
    n_datos        = n(),
    .groups = "drop"
  )

df_ram_used <- df_ram_used %>%
  mutate(timestamp_s = floor_date(timestamp, unit = samp)) %>%
  group_by(timestamp_s) %>%
  summarise(
    ram_promedio = mean(ram_used_by_service_mb, na.rm = TRUE),
    n_datos        = n(),
    .groups = "drop"
  )

# Gráfico 1: RAM Total
p_total <- ggplot(df_ram_total, aes(x = timestamp_s, y = ram_promedio)) +
  geom_line(color = "blue") +
  scale_y_continuous(n.breaks = 8) + # Máximo detalle en la escala 200-400
  theme_minimal() +
  labs(y = "RAM Total") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) # Ocultamos el texto de la fecha aquí

# Gráfico 2: RAM Usada
p_used <- ggplot(df_ram_used, aes(x = timestamp_s, y = ram_promedio)) +
  geom_line(color = "red") +
  scale_y_continuous(n.breaks = 8) + # Máximo detalle en la escala 10-15
  scale_x_datetime(date_labels = "%b %d\n%H:%M", date_breaks = days) +
  theme_minimal() +
  labs(y = "RAM Usada", x = "Timestamp")

# Magia de patchwork: apilar con el operador '/'
p_total / p_used

library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)


#////////////////////
# Comparacion de temperatura interna hora a hora en promedio por dia
#////////////////////
df <- read_csv("/home/franco/measurement.csv")
df$timestamp <- ymd_hms(df$timestamp, tz = "America/Buenos_Aires")
df <- df %>% filter(year(timestamp) == 2026)

net = "sala8"

df_perfil <- df %>% filter (network_id == net)

df_perfil <- df_perfil %>%
  mutate(
    dia_sem   = wday(timestamp, label = TRUE, week_start = 1),
    # hora fraccionaria redondeada a bloques de 30 min
    hora_frac = floor((hour(timestamp) * 60 + minute(timestamp)) / 30) * 30 / 60
  ) %>%
  group_by(dia_sem, hora_frac) %>%
  summarise(temp_promedio = mean(temperature, na.rm = TRUE), .groups = "drop")

ggplot(df_perfil, aes(x = hora_frac, y = temp_promedio, color = dia_sem)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d:00", seq(0, 23, 2))) +
  coord_cartesian(ylim = c(20, 24.5)) +         
  scale_y_continuous(breaks = seq(20, 24.5, 0.5)) +
  scale_color_viridis_d(option = "turbo") +   # o "hue", "manual", etc.
  labs(x = "Hora del día", y = "Temperatura (°C)", color = "Día") +
  theme_minimal()



#////////////////////
# Comparacion de humedad interna hora a hora en promedio por dia
#////////////////////
df <- read_csv("/home/franco/measurement.csv")
df$timestamp <- ymd_hms(df$timestamp, tz = "America/Buenos_Aires")
df <- df %>% filter(year(timestamp) == 2026)

net = "sala8"

df_perfil <- df %>% filter (network_id == net)

df_perfil <- df %>%
  mutate(
    dia_sem   = wday(timestamp, label = TRUE, week_start = 1),
    # hora fraccionaria redondeada a bloques de 30 min
    hora_frac = floor((hour(timestamp) * 60 + minute(timestamp)) / 30) * 30 / 60
  ) %>%
  group_by(dia_sem, hora_frac) %>%
  summarise(hum_promedio = mean(humidity, na.rm = TRUE), .groups = "drop")

ggplot(df_perfil, aes(x = hora_frac, y = hum_promedio, color = dia_sem)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d:00", seq(0, 23, 2))) +
  coord_cartesian(ylim = c(24, 35)) +         
  scale_y_continuous(breaks = seq(24, 35, 2)) +
  scale_color_viridis_d(option = "turbo") +   # o "hue", "manual", etc.
  labs(x = "Hora del día", y = "Humedad (%)", color = "Día") +
  theme_minimal()



#////////////////////
# Comparacion de calidad interna del aire hora a hora en promedio por dia
#////////////////////

#//////////////
# sala 7
df <- read_csv("/home/franco/measurement.csv")
df$timestamp <- ymd_hms(df$timestamp, tz = "America/Buenos_Aires")
df <- df %>% filter(year(timestamp) == 2026)

net = "sala7"

df <- df %>% filter (network_id == net)

df_perfil <- df %>%
  mutate(
    dia_sem   = wday(timestamp, label = TRUE, week_start = 1),
    hora_frac = floor((hour(timestamp) * 60 + minute(timestamp)) / 30) * 30 / 60
  ) %>%
  group_by(dia_sem, hora_frac) %>%
  summarise(air = mean(air_quality, na.rm = TRUE), .groups = "drop")

ggplot(df_perfil, aes(x = hora_frac, y = air, color = dia_sem)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d:00", seq(0, 23, 2))) +
  coord_cartesian(ylim = c(84, 97.5)) +         
  scale_y_continuous(breaks = seq(84, 97.5, 2)) +
  scale_color_viridis_d(option = "turbo") +   
  labs(x = "Hora del día", y = "Calidad del aire (0-100)", color = "Día") +
  theme_minimal()


#//////////////
# sala 8
df <- read_csv("/home/franco/measurement.csv")
df$timestamp <- ymd_hms(df$timestamp, tz = "America/Buenos_Aires")
df <- df %>% filter(year(timestamp) == 2026)

net = "sala8"

df <- df %>% filter (network_id == net)

df_perfil <- df %>%
  mutate(
    dia_sem   = wday(timestamp, label = TRUE, week_start = 1),
    hora_frac = floor((hour(timestamp) * 60 + minute(timestamp)) / 30) * 30 / 60
  ) %>%
  group_by(dia_sem, hora_frac) %>%
  summarise(air = mean(air_quality, na.rm = TRUE), .groups = "drop")

ggplot(df_perfil, aes(x = hora_frac, y = air, color = dia_sem)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d:00", seq(0, 23, 2))) +
  coord_cartesian(ylim = c(90.5, 100)) +         
  scale_y_continuous(breaks = seq(90.5, 100, 1)) +
  scale_color_viridis_d(option = "turbo") +   
  labs(x = "Hora del día", y = "Calidad del aire (0-100)", color = "Día") +
  theme_minimal()






#////////////////////
# Temperatura mensual: promedio interno vs promedio externo
#////////////////////
m = 09
days = "2 days"

df <- read_csv("/home/franco/measurement.csv")
df_w <- read_csv("/home/franco/weather.csv")
df$timestamp <- ymd_hms(df$timestamp, tz = "America/Buenos_Aires")
df_w$timestamp <- ymd_hms(df_w$timestamp, tz = "America/Buenos_Aires")
df <- df %>% filter(year(timestamp) == 2026)

df_m <- df %>% filter(month(timestamp) == m)
df_m_w <- df_w %>% filter(month(timestamp) == m)
df_m_n <- df_m %>% filter (network_id == net)

df_m_n_s <- df_m_n %>%
  mutate(timestamp_s = floor_date(timestamp, unit = samp)) %>%
  group_by(timestamp_s) %>%
  summarise(
    temp_promedio = mean(temperature, na.rm = TRUE),
    n_datos        = n(), 
    .groups = "drop"
  )

df_m_s_w <- df_m_w %>%
  mutate(timestamp_s_w = floor_date(timestamp, unit = samp)) %>%
  group_by(timestamp_s_w) %>%
  summarise(
    temp_promedio = mean(temperature, na.rm = TRUE),
    n_datos        = n(),
    .groups = "drop"
  )

ggplot() +
  geom_line(data = df_m_n_s, aes(x = timestamp_s, y = temp_promedio), color = "blue") +
  geom_line(data = df_m_s_w, aes(x = timestamp_s_w, y = temp_promedio), color = "red") +
  coord_cartesian(ylim = c(0, 25.5)) +         
  scale_y_continuous(breaks = seq(0, 25.5, 1)) +
  scale_x_datetime(date_labels = "%b %d\n%H:%M", date_breaks = days) +
  theme_minimal()



#////////////////////
# Comparacion de temperatura interna entre meses
#////////////////////
m = 08
m2 = 09

df <- read_csv("/home/franco/measurement.csv")
df$timestamp <- ymd_hms(df$timestamp, tz = "America/Buenos_Aires")
df <- df %>% filter(year(timestamp) == 2026)

df_m <- df %>% filter(month(timestamp) == m)
df_m_n <- df_m %>% filter(network_id == net)

df_m_n_s <- df_m_n %>%
  mutate(timestamp_s = floor_date(timestamp, unit = samp)) %>%
  group_by(timestamp_s) %>%
  summarise(
    temp_promedio = mean(temperature, na.rm = TRUE),
    n_datos        = n(),
    .groups = "drop"
  ) %>%
  mutate(dia = day(timestamp_s) + hour(timestamp_s)/24 + minute(timestamp_s)/1440)


# Mes 2
df_m2 <- df %>% filter(month(timestamp) == m2)
df_m2_n <- df_m2 %>% filter(network_id == net)

df_m2_n_s <- df_m2_n %>%
  mutate(timestamp_s = floor_date(timestamp, unit = samp)) %>%
  group_by(timestamp_s) %>%
  summarise(
    temp_promedio = mean(temperature, na.rm = TRUE),
    n_datos        = n(),
    .groups = "drop"
  ) %>%
  mutate(dia = day(timestamp_s) + hour(timestamp_s)/24 + minute(timestamp_s)/1440)

ggplot() +
  geom_line(data = df_m_n_s,
            aes(x = dia, y = temp_promedio, color = "Agosto"),
            linewidth = 0.7) +
  geom_line(data = df_m2_n_s,
            aes(x = dia, y = temp_promedio, color = "Septiembre"),
            linewidth = 0.7) +
  scale_color_manual(values = c("Septiembre" = "blue", "Agosto" = "red")) +
  scale_x_continuous(breaks = 1:30) +
  coord_cartesian(ylim = c(15.5, 26)) +         
  scale_y_continuous(breaks = seq(15.5, 26, 1)) +
  labs(x = "Día del mes", y = "Temperatura (°C)", color = "Mes") +
  theme_minimal()
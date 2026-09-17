library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)


azul_profundo <- "#003785"
celeste       <- "#5DADE2"
gris_claro    <- "#D5DBDB"
gris_oscuro   <- "#5D6D7E"
negro_texto   <- "#17202A"


# Generador de gradiente para los 7 días de la semana (Gris -> Celeste -> Azul Profundo)
paleta_semana <- colorRampPalette(c(gris_claro, celeste, azul_profundo))(7)

# Tema reutilizable para todos los graficos
tema_presentacion <- theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", color = negro_texto, size = 16, margin = margin(b = 8)),
    plot.subtitle    = element_text(color = gris_oscuro, size = 12, margin = margin(b = 15)),
    axis.title.x     = element_text(face = "bold", color = negro_texto, margin = margin(t = 10)),
    axis.title.y     = element_text(face = "bold", color = negro_texto, margin = margin(r = 10)),
    axis.text        = element_text(color = negro_texto, size = 10),
    panel.grid.major = element_line(color = "#EBF5FB", linewidth = 0.5), 
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", color = negro_texto),
    legend.text      = element_text(color = gris_oscuro, size = 11),
    plot.margin      = margin(t = 20, r = 20, b = 20, l = 20)
  )


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
    hora_frac = floor((hour(timestamp) * 60 + minute(timestamp)) / 30) * 30 / 60
  ) %>%
  group_by(dia_sem, hora_frac) %>%
  summarise(temp_promedio = mean(temperature, na.rm = TRUE), .groups = "drop")

ggplot(df_perfil, aes(x = hora_frac, y = temp_promedio, color = dia_sem)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d:00", seq(0, 23, 2))) +
  coord_cartesian(ylim = c(20, 24.5)) +         
  scale_y_continuous(breaks = seq(20, 24.5, 0.5)) +
  scale_color_manual(values = paleta_semana) +   
  labs(
    title = "Perfil Diario de Temperatura Interna",
    subtitle = paste("Variación horaria promedio según el día de la semana - Red:", net),
    x = "Hora del día", 
    y = "Temperatura (°C)", 
    color = "Día"
  ) +
  tema_presentacion



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
    hora_frac = floor((hour(timestamp) * 60 + minute(timestamp)) / 30) * 30 / 60
  ) %>%
  group_by(dia_sem, hora_frac) %>%
  summarise(hum_promedio = mean(humidity, na.rm = TRUE), .groups = "drop")

ggplot(df_perfil, aes(x = hora_frac, y = hum_promedio, color = dia_sem)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d:00", seq(0, 23, 2))) +
  coord_cartesian(ylim = c(24, 35)) +         
  scale_y_continuous(breaks = seq(24, 35, 2)) +
  scale_color_manual(values = paleta_semana) +   
  labs(
    title = "Perfil Diario de Humedad Relativa",
    subtitle = paste("Fluctuación horaria promedio por día de la semana - Red:", net),
    x = "Hora del día", 
    y = "Humedad (%)", 
    color = "Día"
  ) +
  tema_presentacion



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
  geom_line(linewidth = 1.2, alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d:00", seq(0, 23, 2))) +
  coord_cartesian(ylim = c(84, 97.5)) +         
  scale_y_continuous(breaks = seq(84, 97.5, 2)) +
  scale_color_manual(values = paleta_semana) +   
  labs(
    title = "Monitoreo de Calidad de Aire (AQI)",
    subtitle = "Tendencia horaria semanal - Red: sala7",
    x = "Hora del día", 
    y = "Índice de Calidad (0-100)", 
    color = "Día"
  ) +
  tema_presentacion


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
  geom_line(linewidth = 1.2, alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d:00", seq(0, 23, 2))) +
  coord_cartesian(ylim = c(90.5, 100)) +         
  scale_y_continuous(breaks = seq(90.5, 100, 1)) +
  scale_color_manual(values = paleta_semana) +   
  labs(
    title = "Monitoreo de Calidad de Aire (AQI)",
    subtitle = "Tendencia horaria semanal - Red: sala8",
    x = "Hora del día", 
    y = "Índice de Calidad (0-100)", 
    color = "Día"
  ) +
  tema_presentacion




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
  # Se incorpora la estética 'color' dentro de aes() para que genere la leyenda automaticamente
  geom_line(data = df_m_n_s, aes(x = timestamp_s, y = temp_promedio, color = "Interna"), linewidth = 1) +
  geom_line(data = df_m_s_w, aes(x = timestamp_s_w, y = temp_promedio, color = "Externa"), linewidth = 1) +
  coord_cartesian(ylim = c(0, 25.5)) +         
  scale_y_continuous(breaks = seq(0, 25.5, 5)) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = days) +
  scale_color_manual(values = c("Interna" = azul_profundo, "Externa" = gris_oscuro)) +
  labs(
    title = "Contraste Térmico Ambiental",
    subtitle = "Temperatura interna del sistema vs. Condiciones meteorológicas externas",
    x = "Día del Mes",
    y = "Temperatura (°C)",
    color = "Ubicación del Sensor"
  ) +
  tema_presentacion



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
            linewidth = 1, alpha = 0.8) +
  geom_line(data = df_m2_n_s,
            aes(x = dia, y = temp_promedio, color = "Septiembre"),
            linewidth = 1) +
  scale_color_manual(values = c("Septiembre" = azul_profundo, "Agosto" = celeste)) +
  scale_x_continuous(breaks = 1:30) +
  coord_cartesian(ylim = c(15.5, 26)) +         
  scale_y_continuous(breaks = seq(15.5, 26, 2)) +
  labs(
    title = "Evolución Térmica Intermensual",
    subtitle = "Análisis comparativo: Agosto vs. Septiembre",
    x = "Día del mes", 
    y = "Temperatura (°C)", 
    color = "Mes de Registro"
  ) +
  tema_presentacion
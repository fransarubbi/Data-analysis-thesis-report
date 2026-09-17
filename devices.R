library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(scales)
library(patchwork)


#///////////////////////
# 1. Analisis del Edge
#///////////////////////

#------------------------
# 1.1 Tiempo activo
#------------------------
df_me <- read_csv("/home/franco/metric.csv")
df_me$timestamp <- ymd_hms(df_me$timestamp, tz = "America/Buenos_Aires")
df_me <- df_me %>% filter(year(timestamp) == 2026)

device <- df_me %>%
  mutate(active_hours = uptime_seconds / 3600)

ggplot(device, aes(x = timestamp, y = active_hours)) +
  geom_line(color = "#2c3e50", linewidth = 0.5) +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),
    breaks = c(0, 1, 12, 24, 24*7, 24*15, 24*40),
    labels = c("0", "1 h", "12 hs", "1 día", "1 semana", "15 días", "40 días")
  ) +
  scale_x_datetime(date_labels = "%d %b\n%Y", date_breaks = "1 week") +
  labs(
    title = "Evolución de Estabilidad de los Edge (Uptime)",
    x = "Fecha",
    y = "Tiempo Activo Continuo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40", size = 11),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10)
  )



#------------------------
# 1.2 Uso de memoria
#------------------------
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

p_total <- ggplot(df_ram_total, aes(x = timestamp_s, y = ram_promedio)) +
  geom_line(color = "blue") +
  scale_y_continuous(n.breaks = 8) + # Máximo detalle en la escala 200-400
  theme_minimal() +
  labs(y = "RAM Bruta Usada (MB)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) # Ocultamos el texto de la fecha aquí

p_used <- ggplot(df_ram_used, aes(x = timestamp_s, y = ram_promedio)) +
  geom_line(color = "red") +
  scale_y_continuous(n.breaks = 8) + # Máximo detalle en la escala 10-15
  scale_x_datetime(date_labels = "%b %d\n%H:%M", date_breaks = days) +
  theme_minimal() +
  labs(y = "RAM Neta Usada (MB)", x = "Timestamp")

p_total / p_used



#///////////////////////
# 2. Analisis del Hub
#///////////////////////

#----------------------------------------
# 2.1 Distribucion de mensajes monitoreo
#----------------------------------------
# La grafica demuestra que todos los dispositivos han enviado casi la 
# misma cantida de mensajes de monitoreo.Eso significa que las redes 
# tienen buena salud y son estables.
df_mo <- read_csv("/home/franco/monitor.csv")
df_mo$timestamp <- ymd_hms(df_mo$timestamp, tz = "America/Buenos_Aires")
df_mo <- df_mo %>% filter(year(timestamp) == 2026)

sender <- df_mo %>% 
  select(timestamp, sender_user_id)

ggplot(sender, aes(x = sender_user_id)) +
  geom_bar(fill = "cyan", color = "black") +
  coord_cartesian(ylim = c(0, 140000)) +         
  scale_y_continuous(
    breaks = seq(0, 140000, 20000),
    labels = scales::label_number(scale = 1e-3, suffix = "k")
  ) +
  labs(title = "Distribución de los mensajes de monitoreo (Hub)",
       x = "MAC Hub",
       y = "Cantidad de mensajes de monitoreo") +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#------------------------
# 2.2 Tiempo activo
#------------------------
mac = "40:22:D8:6E:8F:E4"
days = "5 days"

df_mo <- read_csv("/home/franco/monitor.csv")
df_mo$timestamp <- ymd_hms(df_mo$timestamp, tz = "America/Buenos_Aires")
df_mo <- df_mo %>% filter(year(timestamp) == 2026)

device <- df_mo %>% filter(sender_user_id == mac)

device <- device %>%
  mutate(active_hours = active_time / 3600)

fecha_update <- as.POSIXct("2026-07-30 18:00:00")
fecha_corte_luz <- as.POSIXct("2026-08-9 00:00:00")
fecha_vuelta_luz <- as.POSIXct("2026-08-10 00:00:00")

ggplot(device, aes(x = timestamp, y = active_hours)) +
  annotate("rect", xmin = min(device$timestamp, na.rm = TRUE), xmax = fecha_update, 
           ymin = 0, ymax = Inf, alpha = 0.08, fill = "red") +
  annotate("text", x = mean(c(min(device$timestamp, na.rm = TRUE), fecha_update)), 
           y = max(device$active_hours, na.rm = TRUE) * 0.7, 
           label = "Fase\nInestable", color = "darkred", size = 4) +
  geom_line(color = "#2c3e50", linewidth = 0.5) +
  annotate("rect", xmin = fecha_corte_luz, xmax = fecha_vuelta_luz, 
           ymin = 0, ymax = Inf, alpha = 0.08, fill = "orange") +
  geom_line(color = "#2c3e50", linewidth = 0.5) +
  annotate("rect", xmin = fecha_corte_luz, xmax = fecha_vuelta_luz, 
           ymin = 0, ymax = Inf, alpha = 0.08, fill = "orange") +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),
    # Elegimos cortes narrativos y los traducimos a etiquetas humanas
    breaks = c(0, 1, 12, 24, 24*7, 24*15, 24*40),
    labels = c("0", "1 h", "12 hs", "1 día", "1 semana", "15 días", "40 días")
  ) +
  scale_x_datetime(date_labels = "%d %b\n%Y", date_breaks = "1 week") +
  labs(
    title = "Evolución de Estabilidad de los Hub (Uptime)",
    subtitle = "Transición de reinicios constantes a un servicio continuo",
    x = "Fecha",
    y = "Tiempo Activo Continuo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40", size = 11),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10)
  )

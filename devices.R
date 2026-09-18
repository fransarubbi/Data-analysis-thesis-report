library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(scales)
library(patchwork)
library(plotly)
library(htmlwidgets)


azul_profundo <- "#003785"
celeste       <- "#5DADE2"
gris_claro    <- "#E5E7E9"
gris_medio    <- "#B2BABB"
gris_oscuro   <- "#5D6D7E"
negro_texto   <- "#17202A"

tema_presentacion <- theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", color = negro_texto, size = 16, margin = margin(b = 8)),
    plot.subtitle    = element_text(color = gris_oscuro, size = 12, margin = margin(b = 15)),
    axis.title.x     = element_text(face = "plain", color = negro_texto, margin = margin(t = 10)),
    axis.title.y     = element_text(face = "plain", color = negro_texto, margin = margin(r = 10)),
    axis.text        = element_text(color = negro_texto, size = 10),
    panel.grid.major = element_line(color = "#EBF5FB", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(face = "plain", color = negro_texto),
    legend.title.position = "top",
    legend.text      = element_text(color = gris_oscuro, size = 11),
    plot.margin      = margin(t = 20, r = 20, b = 20, l = 20)
  )


# ============================================================
# Función auxiliar: convierte un ggplot en un widget plotly
# ============================================================
exportar_html <- function(p, archivo, ancho_leyenda = 190, selfcontained = TRUE) {
  titulo    <- p$labels$title
  p_sin_titulos <- p + labs(title = NULL, subtitle = NULL)
  p_ly <- ggplotly(p_sin_titulos, tooltip = c("x", "y", "colour")) %>%
    config(displayModeBar = FALSE)
  texto_titulo <- paste0(
    "<b style='font-size:18px; color:", negro_texto, "'>", titulo, "</b>"
  )
  p_ly <- p_ly %>%
    layout(
      title = list(text = texto_titulo, x = 0.01, xanchor = "left", y = 0.98, yanchor = "top"),
      legend = list(
        orientation = "v",     # leyenda vertical
        x = 1.02, xanchor = "left",
        y = 0.5,  yanchor = "middle",
        bgcolor = "rgba(255,255,255,0)",
        bordercolor = "rgba(0,0,0,0)"
      ),
      margin = list(t = 70, r = ancho_leyenda, b = 70, l = 70)
    )
  print(p_ly)
  saveWidget(widget = p_ly, file = archivo, selfcontained = selfcontained)
  invisible(p_ly)
}


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
  geom_line(color = azul_profundo, linewidth = 0.8) +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),
    breaks = c(0, 1, 12, 24, 24*7, 24*15, 24*40),
    labels = c("0", "1 h", "12 hs", "1 día", "1 sem", "15 días", "40 días")
  ) +
  scale_x_datetime(date_labels = "%d %b\n%Y", date_breaks = "1 week") +
  labs(
    title = "Estabilidad Operativa: Nodos Edge (Uptime)",
    subtitle = "Registro de tiempo activo continuo y resiliencia del servicio",
    x = "Fecha de Registro",
    y = "Tiempo Activo Continuo"
  ) +
  tema_presentacion



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
  geom_line(color = azul_profundo, linewidth = 1) +
  scale_y_continuous(n.breaks = 6) + 
  labs(y = "RAM Global Usada (MB)") +
  tema_presentacion +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

p_used <- ggplot(df_ram_used, aes(x = timestamp_s, y = ram_promedio)) +
  geom_line(color = celeste, linewidth = 1) +
  scale_y_continuous(n.breaks = 6) + 
  scale_x_datetime(date_labels = "%d %b\n%H:%M", date_breaks = days) +
  labs(y = "RAM del Servicio (MB)", x = "Línea Temporal") +
  tema_presentacion

# Composición visual con patchwork
(p_total / p_used) + 
  plot_annotation(
    title = "Consumo de Memoria RAM: Global vs. Servicio Específico",
    subtitle = "Comparativa de impacto del servicio sobre los recursos del sistema base",
    theme = theme(
      plot.title = element_text(face = "bold", color = negro_texto, size = 16),
      plot.subtitle = element_text(color = gris_oscuro, size = 12)
    )
  )

p1 <- ggplotly(p_total, tooltip = c("x", "y"))
p2 <- ggplotly(p_used, tooltip = c("x", "y"))

grafico_ram_combinado <- subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(
    title = list(
      text = "<b style='font-size:18px; color:#17202A'>Consumo de Memoria RAM: Global vs. Servicio Específico</b>",
      x = 0.01, 
      xanchor = "left", 
      y = 0.98, 
      yanchor = "top"
    ),
    margin = list(t = 70, r = 20, b = 40, l = 70),
    showlegend = FALSE
  ) %>%
  config(displayModeBar = FALSE)

saveWidget(widget = grafico_ram_combinado, file = "grafico_edge_ram.html", selfcontained = TRUE)



#///////////////////////
# 2. Analisis del Hub
#///////////////////////

#----------------------------------------
# 2.1 Distribucion de mensajes monitoreo
#----------------------------------------
# La grafica demuestra que todos los dispositivos han enviado casi la 
# misma cantida de mensajes de monitoreo.Eso significa que las redes 
# tienen buena salud y son estables.
tema_presentacion <- theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", color = negro_texto, size = 16, margin = margin(b = 8)),
    plot.subtitle    = element_text(color = gris_oscuro, size = 12, margin = margin(b = 15)),
    axis.title.x     = element_text(face = "plain", color = negro_texto, margin = margin(t = 10)),
    axis.title.y     = element_text(face = "plain", color = negro_texto, margin = margin(r = 10)),
    axis.text        = element_text(color = negro_texto, size = 10),
    axis.text.x      = element_text(angle = 10, hjust = 1, face = "plain"),  
    panel.grid.major = element_line(color = "#EBF5FB", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(face = "plain", color = negro_texto),
    legend.title.position = "top",
    legend.text      = element_text(color = gris_oscuro, size = 11),
    plot.margin      = margin(t = 20, r = 20, b = 20, l = 20)
  )

df_mo <- read_csv("/home/franco/monitor.csv")
df_mo$timestamp <- ymd_hms(df_mo$timestamp, tz = "America/Buenos_Aires")
df_mo <- df_mo %>% filter(year(timestamp) == 2026)

sender <- df_mo %>% 
  select(timestamp, sender_user_id)

p <- ggplot(sender, aes(x = sender_user_id)) +
  geom_bar(fill = celeste, color = azul_profundo, width = 0.6) +
  coord_cartesian(ylim = c(0, 140000)) +         
  scale_y_continuous(
    breaks = seq(0, 140000, 20000),
    labels = scales::label_number(scale = 1e-3, suffix = "k")
  ) +
  labs(
    title = "Distribución de Carga: Mensajes de Monitoreo (Hub)",
    subtitle = "Volumen de transmisión por identificador físico (MAC Address)",
    x = "Dirección MAC",
    y = "Volumen de Mensajes"
  ) +
  tema_presentacion

exportar_html(p, "grafico_distribucion_monitoreo.html")



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

p <- ggplot(device, aes(x = timestamp, y = active_hours)) +
  # Áreas de contexto histórico usando la paleta de grises
  annotate("rect", xmin = min(device$timestamp, na.rm = TRUE), xmax = fecha_update, 
           ymin = 0, ymax = 1560, alpha = 0.4, fill = gris_claro) +
  annotate("text", x = mean(c(min(device$timestamp, na.rm = TRUE), fecha_update)), 
           y = 30, label = "Fase Inicial\n(Inestable)", color = gris_oscuro, size = 3.5, fontface = "italic") +
  annotate("rect", xmin = fecha_corte_luz, xmax = fecha_vuelta_luz, 
           ymin = 0, ymax = 1560, alpha = 0.3, fill = gris_medio) +
  annotate("text", x = fecha_corte_luz, y = 280, 
           label = " Corte Eléctrico", hjust = -0.1, color = negro_texto, size = 3.5) +
  geom_line(color = azul_profundo, linewidth = 0.8) +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),
    breaks = c(0, 1, 12, 24, 24*7, 24*15, 24*40),
    labels = c("0", "1 h", "12 hs", "1 día", "1 sem", "15 días", "40 días")
  ) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "1 week") +
  labs(
    title = "Estabilidad Operativa Histórica de un Hub Particular",
    subtitle = paste("Transición hacia la disponibilidad continua - Dispositivo:", mac),
    x = "Dia",
    y = "Tiempo Activo Continuo"
  ) +
  tema_presentacion

exportar_html(p, "grafico_uptime_hub.html")

library(tidyverse)

campinas_qualar <- readRDS("dados_QUALAR/campinas_qualar_media.rds")
limeira_qualar <- readRDS("dados_QUALAR/limeira_qualar_media.rds")
paulinia_qualar <- readRDS("dados_QUALAR/paulinia_qualar_media.rds")

#Desconsidera a média de dias com menos de 17 horas de monitoramento e os coloca na mesma categoria de dias sem monitoramento (inválidos)
remove_dias_menos_17h <- function(data){
  data |>
    mutate(
      MEDIA_MP25 = ifelse((HORAS_MONIT_MP25<17), NA, MEDIA_MP25),
      HORAS_MONIT_MP25 = ifelse((HORAS_MONIT_MP25<17), 0, HORAS_MONIT_MP25)
    )
}

campinas_rem_17 <- remove_dias_menos_17h(campinas_qualar) 
limeira_rem_17 <- remove_dias_menos_17h(limeira_qualar)
paulinia_rem_17 <- remove_dias_menos_17h(paulinia_qualar)

#Séries temporais após remover dias com menos de 17 horas de monitoramento
campinas_st17 <- ggplot(campinas_rem_17, aes(x = DATA, y = MEDIA_MP25))+
  geom_line(color = "darkblue") +
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y"
  ) +
  scale_y_continuous(
    limits = c(0, 90), breaks = seq(0, 90, by = 10),
    expand = c(0,0)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "",
    title = "Média Diária de Material Particulado 2,5 (µg/m³) em Campinas (2021 - 2025)",
    subtitle = "Desconsiderando dias com menos de 17 horas de monitoramento"
  )

limeira_st17 <- ggplot(limeira_rem_17, aes(x = DATA, y = MEDIA_MP25))+
  geom_line(color = "darkblue") +
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y"
  ) +
  scale_y_continuous(
    limits = c(0, 90), breaks = seq(0, 90, by = 10),
    expand = c(0,0)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "",
    title = "Média Diária de Material Particulado 2,5 (µg/m³) em Limeira (2021 - 2025)",
    subtitle = "Desconsiderando dias com menos de 17 horas de monitoramento"
  )

paulinia_st17 <- ggplot(paulinia_rem_17, aes(x = DATA, y = MEDIA_MP25))+
  geom_line(color = "darkblue") +
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y"
  ) +
  scale_y_continuous(
    limits = c(0, 90), breaks = seq(0, 90, by = 10),
    expand = c(0,0)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "",
    title = "Média Diária de Material Particulado 2,5 (µg/m³) em Paulínia (2021 - 2025)",
    subtitle = "Desconsiderando dias com menos de 17 horas de monitoramento"
  )

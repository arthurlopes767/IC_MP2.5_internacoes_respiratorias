library(tidyverse)

campinas_qualar <- readRDS("dados_QUALAR/campinas_qualar_media.rds")
limeira_qualar <- readRDS("dados_QUALAR/limeira_qualar_media.rds")
paulinia_qualar <- readRDS("dados_QUALAR/paulinia_qualar_media.rds")


#Contagem de dias com ao menos 17 horas de monitoramento, entre 16 e 12 horas de monitoramento e menos de 12 horas
contagem_de_dias_validos <- function(dados){
  dados_validos_filtrados <- dados |>
    filter(HORAS_MONIT_MP25 >= 17)
  quantidade_validos <- nrow(dados_validos_filtrados)
  print(paste("Dias com pelo menos 17 horas de monitoramento:", quantidade_validos))
  
  dados_analise_filtrados <- dados |>
    filter(HORAS_MONIT_MP25 >= 12 & HORAS_MONIT_MP25 <= 16)
  quantidade_analise <- nrow(dados_analise_filtrados)
  print(paste("Dias com monitoramento entre 12 e 16 horas:", quantidade_analise))
  
  dados_excluidos <- dados |>
    filter(HORAS_MONIT_MP25 < 12)
  quantidade_excluidos <- nrow(dados_excluidos)
  print(paste("Dias com monitoramento menor que 12 horas (excluídos):", quantidade_excluidos))
  
  cat("\n")
}

print("CAMPINAS")
contagem_de_dias_validos(campinas_qualar)

print("LIMEIRA")
contagem_de_dias_validos(limeira_qualar)

print("PAULINÍA")
contagem_de_dias_validos(paulinia_qualar)

#Gráficos de Colunas (x = Data, y = Horas de Monitoramento de MP2,5)
#Campinas
campinas_geral <- ggplot(campinas_qualar, aes(x = DATA, y = HORAS_MONIT_MP25)) +
  geom_col(fill = "darkblue", width = 1) +
  geom_hline(yintercept = 17, linewidth = 1, color = "red")+
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y",
    expand = c(0, 0)
  )+
  scale_y_continuous(breaks = seq(0, 24, by = 1), expand = c(0,0)) +
  labs(title = "Horas de monitoramento por dia - CAMPINAS", x = "Data", y = "Horas de monitoramento") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  ) 

campinas_analise_12 <- ggplot(campinas_qualar, aes(
  x = DATA, 
  y = HORAS_MONIT_MP25, 
  fill = ifelse(
    HORAS_MONIT_MP25 < 12, "< 12h (Descarte)", 
    ifelse(HORAS_MONIT_MP25 < 17, "12h a 16h (Imputar)", ">= 17h (Válido)")
  )
)) +
  geom_col(width = 1) +
  geom_hline(yintercept = 17, linewidth = 1, color = "red") +
  geom_hline(yintercept = 12, linewidth = 1, color = "red") +
  scale_fill_manual(
    name = "Status do Dia",
    values = c(
      ">= 17h (Válido)" = "darkblue", 
      "12h a 16h (Imputar)" = "orange", 
      "< 12h (Descarte)" = "tomato"
    )
  ) +
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(breaks = seq(0, 24, by = 1), expand = c(0,0)) +
  labs(
    title = "Horas de monitoramento por dia - CAMPINAS", 
    x = "Data", 
    y = "Horas de monitoramento"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom" 
  )


#Limeira
limeira_geral <- ggplot(limeira_qualar, aes(x = DATA, y = HORAS_MONIT_MP25)) +
  geom_col(fill = "darkblue", width = 1) +
  geom_hline(yintercept = 17, linewidth = 1, color = "red")+
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y",
    expand = c(0, 0)
  )+
  scale_y_continuous(breaks = seq(0, 24, by = 1), expand = c(0,0)) +
  labs(title = "Horas de monitoramento por dia - Limeira", x = "Data", y = "Horas de monitoramento") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  ) 

limeira_analise_12 <- ggplot(limeira_qualar, aes(
  x = DATA, 
  y = HORAS_MONIT_MP25, 
  fill = ifelse(
    HORAS_MONIT_MP25 < 12, "< 12h (Descarte)", 
    ifelse(HORAS_MONIT_MP25 < 17, "12h a 16h (Imputar)", ">= 17h (Válido)")
  )
)) +
  geom_col(width = 1) +
  geom_hline(yintercept = 17, linewidth = 1, color = "red") +
  geom_hline(yintercept = 12, linewidth = 1, color = "red") +
  scale_fill_manual(
    name = "Status do Dia",
    values = c(
      ">= 17h (Válido)" = "darkblue", 
      "12h a 16h (Imputar)" = "orange", 
      "< 12h (Descarte)" = "tomato"
    )
  ) +
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(breaks = seq(0, 24, by = 1), expand = c(0,0)) +
  labs(
    title = "Horas de monitoramento por dia - LIMEIRA", 
    x = "Data", 
    y = "Horas de monitoramento"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom" 
  )


#Paulínia
paulinia_geral <- ggplot(paulinia_qualar, aes(x = DATA, y = HORAS_MONIT_MP25)) +
  geom_col(fill = "darkblue", width = 1) +
  geom_hline(yintercept = 17, linewidth = 1, color = "red")+
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y",
    expand = c(0, 0)
  )+
  scale_y_continuous(breaks = seq(0, 24, by = 1), expand = c(0,0)) +
  labs(title = "Horas de monitoramento por dia - Paulínia", x = "Data", y = "Horas de monitoramento") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  ) 

paulinia_analise_12 <- ggplot(paulinia_qualar, aes(
  x = DATA, 
  y = HORAS_MONIT_MP25, 
  fill = ifelse(
    HORAS_MONIT_MP25 < 12, "< 12h (Descarte)", 
    ifelse(HORAS_MONIT_MP25 < 17, "12h a 16h (Imputar)", ">= 17h (Válido)")
  )
)) +
  geom_col(width = 1) +
  geom_hline(yintercept = 17, linewidth = 1, color = "red") +
  geom_hline(yintercept = 12, linewidth = 1, color = "red") +
  scale_fill_manual(
    name = "Status do Dia",
    values = c(
      ">= 17h (Válido)" = "darkblue", 
      "12h a 16h (Imputar)" = "orange", 
      "< 12h (Descarte)" = "tomato"
    )
  ) +
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(breaks = seq(0, 24, by = 1), expand = c(0,0)) +
  labs(
    title = "Horas de monitoramento por dia - PAULÍNIA", 
    x = "Data", 
    y = "Horas de monitoramento"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom" 
  )


#Gráficos de Séries Temporais (x = Data, y = Média diária de monitoramento de MP2,5)
campinas_st <- ggplot(campinas_qualar, aes(x = DATA, y = MEDIA_MP25))+
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
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "",
    title = "Média Diária de Material Particulado 2,5 (µg/m³) em Campinas (2021 - 2025)"
  )

limeira_st <- ggplot(limeira_qualar, aes(x = DATA, y = MEDIA_MP25))+
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
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "",
    title = "Média Diária de Material Particulado 2,5 (µg/m³) em Limeira (2021 - 2025)"
  )

paulinia_st <- ggplot(paulinia_qualar, aes(x = DATA, y = MEDIA_MP25))+
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
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "",
    title = "Média Diária de Material Particulado 2,5 (µg/m³) em Paulínia (2021 - 2025)"
  )
  
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
  

#BOXPLOT
campinas_qualar <- readRDS("dados_QUALAR/dados_processados/missForest/mf_campinas.rds")
limeira_qualar <- readRDS("dados_QUALAR/dados_processados/missForest/mf_limeira.rds")
paulinia_qualar <- readRDS("dados_QUALAR/dados_processados/missForest/mf_paulinia.rds")

campinas_qualar_sem_proces <- readRDS("dados_QUALAR/dados_processados/campinas_rem_17.rds")
limeira_qualar_sem_proces <- readRDS("dados_QUALAR/dados_processados/limeira_rem_17.rds")
paulinia_qualar_sem_proces <- readRDS("dados_QUALAR/dados_processados/paulinia_rem_17.rds")

gera_boxplot_qualar <- function(df, atributo, nome_eixo_y = atributo) {
  ggplot(df, aes(x = format(DATA, "%Y"), y = {{ atributo }}, fill = format(DATA, "%Y"))) +
    stat_boxplot(geom = "errorbar", width = 0.25) +
    geom_boxplot(color = "black", outlier.shape = 1, outlier.size = 2) +
    scale_y_continuous(breaks = seq(0, 1000, by = 10)) +
    scale_fill_brewer(palette = "Pastel1") + 
    labs(
      x = "Ano de Medição",
      y = nome_eixo_y
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 11)
    )
}

boxplot_campinas_mp <- gera_boxplot_qualar(campinas_qualar, MEDIA_MP25,"MP2,5 (µg/m³)")
boxplot_campinas_temp <- gera_boxplot_qualar(campinas_qualar, MEDIA_TEMP,"Temperatura (°C)")
boxplot_campinas_ur <- gera_boxplot_qualar(campinas_qualar, MEDIA_UMIDADE,"Umidade Relativa (%)")
boxplot_campinas_mp_si <- gera_boxplot_qualar(campinas_qualar_sem_proces, MEDIA_MP25,"MP2,5 (µg/m³)")
boxplot_campinas_temp_si <- gera_boxplot_qualar(campinas_qualar_sem_proces, MEDIA_TEMP,"Temperatura (°C)")
boxplot_campinas_ur_si <- gera_boxplot_qualar(campinas_qualar_sem_proces, MEDIA_UMIDADE,"Umidade Relativa (%)")

boxplot_limeira_mp <- gera_boxplot_qualar(limeira_qualar, MEDIA_MP25,"MP2,5 (µg/m³)")
boxplot_limeira_temp <- gera_boxplot_qualar(limeira_qualar, MEDIA_TEMP,"Temperatura (°C)")
boxplot_limeira_ur <- gera_boxplot_qualar(limeira_qualar, MEDIA_UMIDADE,"Umidade Relativa (%)")
boxplot_limeira_mp_si <- gera_boxplot_qualar(limeira_qualar_sem_proces, MEDIA_MP25,"MP2,5 (µg/m³)")
boxplot_limeira_temp_si <- gera_boxplot_qualar(limeira_qualar_sem_proces, MEDIA_TEMP,"Temperatura (°C)")
boxplot_limeira_ur_si <- gera_boxplot_qualar(limeira_qualar_sem_proces, MEDIA_UMIDADE,"Umidade Relativa (%)")

boxplot_paulinia_mp <- gera_boxplot_qualar(paulinia_qualar, MEDIA_MP25,"MP2,5 (µg/m³)")
boxplot_paulinia_temp <- gera_boxplot_qualar(paulinia_qualar, MEDIA_TEMP,"Temperatura (°C)")
boxplot_paulinia_ur <- gera_boxplot_qualar(paulinia_qualar, MEDIA_UMIDADE,"Umidade Relativa (%)")
boxplot_paulinia_mp_si <- gera_boxplot_qualar(paulinia_qualar_sem_proces, MEDIA_MP25,"MP2,5 (µg/m³)")
boxplot_paulinia_temp_si <- gera_boxplot_qualar(paulinia_qualar_sem_proces, MEDIA_TEMP,"Temperatura (°C)")
boxplot_paulinia_ur_si <- gera_boxplot_qualar(paulinia_qualar_sem_proces, MEDIA_UMIDADE,"Umidade Relativa (%)")

#Estatísticas
estatisticas <- function(df, cidade, impute){
  cat("Cidade:", cidade, "(Todos os anos)\n")
  
  if (impute == 1) {
    cat("(Dados imputados por missForest)\n")
  }

  media_mp <- mean(df$MEDIA_MP25, na.rm = TRUE)
  median_mp <- median(df$MEDIA_MP25, na.rm = TRUE)
  min_mp <- min(df$MEDIA_MP25, na.rm = TRUE)
  max_mp <- max(df$MEDIA_MP25, na.rm = TRUE)
  dp_mp <- sd(df$MEDIA_MP25, na.rm = TRUE)
  var_mp <- var(df$MEDIA_MP25, na.rm = TRUE)
  
  media_temp <- mean(df$MEDIA_TEMP, na.rm = TRUE)
  median_temp <- median(df$MEDIA_TEMP, na.rm = TRUE)
  min_temp <- min(df$MEDIA_TEMP, na.rm = TRUE)
  max_temp <- max(df$MEDIA_TEMP, na.rm = TRUE)
  dp_temp <- sd(df$MEDIA_TEMP, na.rm = TRUE)
  var_temp <- var(df$MEDIA_TEMP, na.rm = TRUE)
  
  media_ur <- mean(df$MEDIA_UMIDADE, na.rm = TRUE)
  median_ur <- median(df$MEDIA_UMIDADE, na.rm = TRUE)
  min_ur <- min(df$MEDIA_UMIDADE, na.rm = TRUE)
  max_ur <- max(df$MEDIA_UMIDADE, na.rm = TRUE)
  dp_ur <- sd(df$MEDIA_UMIDADE, na.rm = TRUE)
  var_ur <- var(df$MEDIA_UMIDADE, na.rm = TRUE)
  
  cat(
    "\nMaterial Particulado 2,5 (μg/m³)\n",
    "Média:", media_mp, "| Mediana:", median_mp,
    "| Mínimo:", min_mp, "| Máximo:", max_mp,
    "| Variância:", var_mp, "| Desvio Padrão:", dp_mp, "\n"
  )
  
  cat(
    "Temperatura (°C)\n",
    "Média:", media_temp, "| Mediana:", median_temp,
    "| Mínimo:", min_temp, "| Máximo:", max_temp,
    "| Variância:", var_temp, "| Desvio Padrão:", dp_temp, "\n"
  )
  
  cat(
    "Umidade Relativa (%)\n",
    "Média:", media_ur, "| Mediana:", median_ur,
    "| Mínimo:", min_ur, "| Máximo:", max_ur,
    "| Variância:", var_ur, "| Desvio Padrão:", dp_ur, "\n"
  )
  
  ano_min <- as.Date("2021-01-01")
  ano_max <- as.Date("2021-12-31")
  
  while(ano_min <= as.Date("2025-01-01") & ano_max <= as.Date("2025-12-31")){
    df_ano <- df |> 
      filter(DATA >= ano_min, DATA <= ano_max)
    
    media_mp <- mean(df_ano$MEDIA_MP25, na.rm = TRUE)
    median_mp <- median(df_ano$MEDIA_MP25, na.rm = TRUE)
    min_mp <- min(df_ano$MEDIA_MP25, na.rm = TRUE)
    max_mp <- max(df_ano$MEDIA_MP25, na.rm = TRUE)
    dp_mp <- sd(df_ano$MEDIA_MP25, na.rm = TRUE)
    var_mp <- var(df_ano$MEDIA_MP25, na.rm = TRUE)
    
    media_temp <- mean(df_ano$MEDIA_TEMP, na.rm = TRUE)
    median_temp <- median(df_ano$MEDIA_TEMP, na.rm = TRUE)
    min_temp <- min(df_ano$MEDIA_TEMP, na.rm = TRUE)
    max_temp <- max(df_ano$MEDIA_TEMP, na.rm = TRUE)
    dp_temp <- sd(df_ano$MEDIA_TEMP, na.rm = TRUE)
    var_temp <- var(df_ano$MEDIA_TEMP, na.rm = TRUE)
    
    media_ur <- mean(df_ano$MEDIA_UMIDADE, na.rm = TRUE)
    median_ur <- median(df_ano$MEDIA_UMIDADE, na.rm = TRUE)
    min_ur <- min(df_ano$MEDIA_UMIDADE, na.rm = TRUE)
    max_ur <- max(df_ano$MEDIA_UMIDADE, na.rm = TRUE)
    dp_ur <- sd(df_ano$MEDIA_UMIDADE, na.rm = TRUE)
    var_ur <- var(df_ano$MEDIA_UMIDADE, na.rm = TRUE)
    
    ano_atual <- format(ano_min, "%Y")
    
    cat(
      "\n--- Ano:", ano_atual, "---\n",
      "Material Particulado 2,5 (μg/m³)\n",
      "Média:", media_mp, "| Mediana:", median_mp,
      "| Mínimo:", min_mp, "| Máximo:", max_mp,
      "| Variância:", var_mp, "| Desvio Padrão:", dp_mp, "\n"
    )
    
    cat(
      "Temperatura (°C)\n",
      "Média:", media_temp, "| Mediana:", median_temp,
      "| Mínimo:", min_temp, "| Máximo:", max_temp,
      "| Variância:", var_temp, "| Desvio Padrão:", dp_temp, "\n"
    )
    
    cat(
      "Umidade Relativa (%)\n",
      "Média:", media_ur, "| Mediana:", median_ur,
      "| Mínimo:", min_ur, "| Máximo:", max_ur,
      "| Variância:", var_ur, "| Desvio Padrão:", dp_ur, "\n"
    )
    
    ano_min <- ano_min + years(1)
    ano_max <- ano_max + years(1)
  }
}

estatisticas(campinas_qualar, "Campinas", 1)
estatisticas(campinas_qualar_sem_proces, "Campinas", 0)

estatisticas(limeira_qualar, "Limeira", 1)
estatisticas(limeira_qualar_sem_proces, "Limeira", 0)

estatisticas(paulinia_qualar, "Paulínia", 1)
estatisticas(paulinia_qualar_sem_proces, "Paulínia", 0)


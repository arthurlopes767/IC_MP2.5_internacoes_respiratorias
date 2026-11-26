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

#BOXPLOT internações
campinas_internacoes <- readRDS("dados_SIH/Cidades/internacoes_Campinas.rds")
limeira_internacoes <- readRDS("dados_SIH/Cidades/internacoes_Limeira.rds")
paulinia_internacoes <- readRDS("dados_SIH/Cidades/internacoes_Paulinia.rds")

gera_boxplot_inter <- function(df) {
  ggplot(df, aes(x = format(DT_INTER, "%Y"), y = TOTAL_INTER, fill = format(DT_INTER, "%Y"))) +
    stat_boxplot(geom = "errorbar", width = 0.25) +
    geom_boxplot(color = "black", outlier.shape = 1, outlier.size = 2) +
    scale_fill_brewer(palette = "Pastel1") + 
    labs(
      x = "Ano",
      y = "Internações"
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 11)
    )
}

boxplot_inter_camp <- gera_boxplot_inter(campinas_internacoes)
boxplot_inter_lim <- gera_boxplot_inter(limeira_internacoes)
boxplot_inter_paul <- gera_boxplot_inter(paulinia_internacoes)

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
  
  dia_maior_concentracao <- df |>
    slice_max(order_by = MEDIA_MP25, n = 1) |>
    pull(DATA)
  
  contagem_dias_acima_mp_conama <- sum(df$MEDIA_MP25 > 17, na.rm = TRUE)
  contagem_dias_acima_mp_oms <- sum(df$MEDIA_MP25 > 15, na.rm = TRUE)
  
  cat(
    "\nMaterial Particulado 2,5 (μg/m³)",
    "\nMédia:", media_mp, "\nMediana:", median_mp,
    "\nMínimo:", min_mp, "\nMáximo:", max_mp,
    "\nVariância:", var_mp, "\nDesvio Padrão:", dp_mp, "\n\n"
  )
  
  cat(
    "Temperatura (°C)",
    "\nMédia:", media_temp, "\nMediana:", median_temp,
    "\nMínimo:", min_temp, "\nMáximo:", max_temp,
    "\nVariância:", var_temp, "\nDesvio Padrão:", dp_temp, "\n\n"
  )
  
  cat(
    "Umidade Relativa (%)",
    "\nMédia:", media_ur, "\nMediana:", median_ur,
    "\nMínimo:", min_ur, "\nMáximo:", max_ur,
    "\nVariância:", var_ur, "\nDesvio Padrão:", dp_ur, "\n\n"
  )
  
  cat(
    "Dia com maior concentração de MP2,5: ", format(dia_maior_concentracao, "%d/%m/%Y"), 
    "\nDias com concentração acima das diretrizes do CONAMA n°506/2024 (17 µg/m³):", as.character(contagem_dias_acima_mp_conama),
    "\nDias com concentração acima das diretrizes da OMS (15 µg/m³)", as.character(contagem_dias_acima_mp_oms), "\n\n"
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
    
    dia_maior_concentracao_ano <- df_ano |>
      slice_max(order_by = MEDIA_MP25, n = 1) |>
      pull(DATA)
    
    contagem_dias_acima_mp_conama_ano <- sum(df_ano$MEDIA_MP25 > 17, na.rm = TRUE)
    contagem_dias_acima_mp_oms_ano <- sum(df_ano$MEDIA_MP25 > 15, na.rm = TRUE)
    
    ano_atual <- format(ano_min, "%Y")
    
    cat(
      "\n--- Ano:", ano_atual, "---\n",
      "Material Particulado 2,5 (μg/m³)",
      "\nMédia:", media_mp, "\nMediana:", median_mp,
      "\nMínimo:", min_mp, "\nMáximo:", max_mp,
      "\nVariância:", var_mp, "\nDesvio Padrão:", dp_mp, "\n\n"
    )
    
    cat(
      "Temperatura (°C)",
      "\nMédia:", media_temp, "\nMediana:", median_temp,
      "\nMínimo:", min_temp, "\nMáximo:", max_temp,
      "\nVariância:", var_temp, "\nDesvio Padrão:", dp_temp, "\n\n"
    )
    
    cat(
      "Umidade Relativa (%)",
      "\nMédia:", media_ur, "\nMediana:", median_ur,
      "\nMínimo:", min_ur, "\nMáximo:", max_ur,
      "\nVariância:", var_ur, "\nDesvio Padrão:", dp_ur, "\n\n"
    )
    
    cat(
      "Dia com maior concentração de MP2,5: ", format(dia_maior_concentracao_ano, "%d/%m/%Y"), 
      "\nDias com concentração acima das diretrizes do CONAMA n°506/2024 (17 µg/m³):", as.character(contagem_dias_acima_mp_conama_ano),
      "\nDias com concentração acima das diretrizes da OMS (15 µg/m³)", as.character(contagem_dias_acima_mp_oms_ano), "\n\n"
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

#Estatísticas Internações
estatisticas_internacoes <- function(df, cidade){
  cat("Cidade:", cidade, "(Todos os anos)\n")
  
  media_int <- mean(df$TOTAL_INTER, na.rm = TRUE)
  median_int <- median(df$TOTAL_INTER, na.rm = TRUE)
  max_int <- max(df$TOTAL_INTER, na.rm = TRUE)
  dp_int <- sd(df$TOTAL_INTER, na.rm = TRUE)
  var_int <- var(df$TOTAL_INTER, na.rm = TRUE)
  
  media_int_criancas <- mean(df$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
  median_int_criancas <- median(df$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
  max_int_criancas <- max(df$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
  dp_int_criancas <- sd(df$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
  var_int_criancas <- var(df$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
  
  media_int_idosos <- mean(df$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
  median_int_idosos <- median(df$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
  max_int_idosos <- max(df$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
  dp_int_idosos <- sd(df$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
  var_int_idosos <- var(df$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
  
  dia_maior_inter <- df |>
    slice_max(order_by = TOTAL_INTER, n = 1) |>
    pull(DT_INTER)
  
  cat(
    "\nInternações Totais",
    "\nMédia:", media_int, "\nMediana:", median_int,
    "\nMáximo:", max_int,
    "\nVariância:", var_int, "\nDesvio Padrão:", dp_int, "\n\n"
  )
  
  cat(
    "Dia com mais internações: ", format(dia_maior_inter, "%d/%m/%Y"), "\n" 
  )
  
  
  cat(
    "\nInternações de 0 a 5 anos",
    "\nMédia:", media_int_criancas, "\nMediana:", median_int_criancas,
    "\nMáximo:", max_int_criancas,
    "\nVariância:", var_int_criancas, "\nDesvio Padrão:", dp_int_criancas, "\n\n"
  )
  
  cat(
    "\nInternações de idosos (>60 anos)",
    "\nMédia:", media_int_idosos, "\nMediana:", median_int_idosos,
    "\nMáximo:", max_int_idosos,
    "\nVariância:", var_int_idosos, "\nDesvio Padrão:", dp_int_idosos, "\n\n"
  )
  
  ano_min <- as.Date("2021-01-01")
  ano_max <- as.Date("2021-12-31")
  
  while(ano_min <= as.Date("2025-01-01") & ano_max <= as.Date("2025-12-31")){
    df_ano <- df |> 
      filter(DT_INTER >= ano_min, DT_INTER <= ano_max)
    
    media_int <- mean(df_ano$TOTAL_INTER, na.rm = TRUE)
    median_int <- median(df_ano$TOTAL_INTER, na.rm = TRUE)
    max_int <- max(df_ano$TOTAL_INTER, na.rm = TRUE)
    dp_int <- sd(df_ano$TOTAL_INTER, na.rm = TRUE)
    var_int <- var(df_ano$TOTAL_INTER, na.rm = TRUE)
    
    media_int_criancas <- mean(df_ano$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
    median_int_criancas <- median(df_ano$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
    max_int_criancas <- max(df_ano$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
    dp_int_criancas <- sd(df_ano$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
    var_int_criancas <- var(df_ano$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
    
    media_int_idosos <- mean(df_ano$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
    median_int_idosos <- median(df_ano$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
    max_int_idosos <- max(df_ano$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
    dp_int_idosos <- sd(df_ano$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
    var_int_idosos <- var(df_ano$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
    
    dia_maior_inter <- df_ano |>
      slice_max(order_by = TOTAL_INTER, n = 1) |>
      pull(DT_INTER)
    
    ano_atual <- format(ano_min, "%Y")
    
    cat(
      "\n--- Ano:", ano_atual, "---\n",
      "\nInternações Totais",
      "\nMédia:", media_int, "\nMediana:", median_int,
      "\nMáximo:", max_int,
      "\nVariância:", var_int, "\nDesvio Padrão:", dp_int, "\n\n"
    )
    
    cat(
      "\nInternações de 0 a 5 anos",
      "\nMédia:", media_int_criancas, "\nMediana:", median_int_criancas,
      "\nMáximo:", max_int_criancas,
      "\nVariância:", var_int_criancas, "\nDesvio Padrão:", dp_int_criancas, "\n\n"
    )
    
    cat(
      "\nInternações de idosos (>60 anos)",
      "\nMédia:", media_int_idosos, "\nMediana:", median_int_idosos,
      "\nMáximo:", max_int_idosos,
      "\nVariância:", var_int_idosos, "\nDesvio Padrão:", dp_int_idosos, "\n\n"
    )
    
    cat(
      "Dia com mais internações: ", format(dia_maior_inter, "%d/%m/%Y"), "\n\n" 
    )
    
    ano_min <- ano_min + years(1)
    ano_max <- ano_max + years(1)
  }
}

estatisticas_internacoes(campinas_internacoes, "Campinas")
estatisticas_internacoes(limeira_internacoes, "Limeira")
estatisticas_internacoes(paulinia_internacoes, "Paulínia")


#Gráfico de Barras internações
gera_graf_pizza <- function(df, cidade) {
  tot_0_5  <- sum(df$INTER_ZERO_A_CINCO_ANOS, na.rm = TRUE)
  tot_60   <- sum(df$INTER_MAIOR_IGUAL_SESSENTA_ANOS, na.rm = TRUE)
  tot_6_59 <- sum(pmax(df$TOTAL_INTER - df$INTER_ZERO_A_CINCO_ANOS - df$INTER_MAIOR_IGUAL_SESSENTA_ANOS, 0), na.rm = TRUE)
  
  pizza_df <- data.frame(
    Faixa = factor(
      c("0 a 5 anos", "6 a 59 anos", "60 anos ou mais"),
      levels = c("0 a 5 anos", "6 a 59 anos", "60 anos ou mais")
    ),
    Valor = c(tot_0_5, tot_6_59, tot_60)
  )
  
  pizza_df$Pct <- round(100 * pizza_df$Valor / sum(pizza_df$Valor), 1)
  pizza_df$Rotulo <- paste0(pizza_df$Pct, "%\n(", pizza_df$Valor, ")")
  
  subtitulo <- if (cidade != "") paste("Município:", cidade) else NULL
  
  ggplot(pizza_df, aes(x = "", y = Valor, fill = Faixa)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = Rotulo),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 3.2
    ) +
    scale_fill_manual(
      name = "Faixa etária:",
      values = c(
        "0 a 5 anos" = "#d62728",
        "6 a 59 anos" = "#1f77b4",
        "60 anos ou mais" = "#2ca02c"
      )
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    labs(
      title = "Internações por Faixa Etária",
      subtitle = subtitulo
    )
}

pizza_camp <- gera_graf_pizza(campinas_internacoes, "Campinas")
pizza_lim <- gera_graf_pizza(limeira_internacoes, "Limeira")
pizza_paul <- gera_graf_pizza(paulinia_internacoes, "Paulínia")
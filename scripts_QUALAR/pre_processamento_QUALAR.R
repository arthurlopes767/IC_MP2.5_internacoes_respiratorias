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

saveRDS(campinas_rem_17, "dados_QUALAR/dados_processados/campinas_rem_17.rds")
saveRDS(limeira_rem_17, "dados_QUALAR/dados_processados/limeira_rem_17.rds")
saveRDS(paulinia_rem_17, "dados_QUALAR/dados_processados/paulinia_rem_17.rds")

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


#Ranking das 3 maiores lacunas de dias sem monitoramento
ranking_lacunas <- function(data){
  tam_lacuna <- 0
  data_inicio <- NA
  
  historico_lacunas <- data.frame(
    tamanho = numeric(),
    inicio = as.Date(character()),
    fim = as.Date(character())
  )
  
  for(i in 1:nrow(data)){
    if(is.na(data$MEDIA_MP25[i])){
      if(tam_lacuna == 0){
        data_inicio <- data$DATA[i]
      }
      tam_lacuna <- tam_lacuna + 1
    }else{
      #Se o dia for válido, mas existir uma lacuna aberta, quer dizer que o dia anterior a ele é o dia final de uma lacuna
      if(tam_lacuna > 0){ 
        data_fim <- data$DATA[i-1]
        nova_linha <- data.frame(
          tamanho = tam_lacuna, 
          inicio = data_inicio,
          fim = data_fim
        )
        historico_lacunas <- rbind(historico_lacunas, nova_linha)
        tam_lacuna <- 0
      }
    }
  }
  
  #Garante o salvamento se a base terminar no meio de uma lacuna
  if(tam_lacuna > 0){
    data_fim <- data$DATA[nrow(data)]
    nova_linha <- data.frame(
      tamanho = tam_lacuna,
      inicio = data_inicio,
      fim = data_fim
    )
    historico_lacunas <- rbind(historico_lacunas, nova_linha)
  }
  
  historico_lacunas <- historico_lacunas[order(-historico_lacunas$tamanho),]
  
  for(posicao in 1:3){
    tam <- historico_lacunas$tamanho[posicao]
    data_ini <- format(historico_lacunas$inicio[posicao], "%d/%m/%Y")
    data_f <- format(historico_lacunas$fim[posicao], "%d/%m/%Y")
    
    print(paste(posicao, "lacuna: ", tam, "dias ", data_ini, data_f))
  }
  
  cat("\n")
}

print("CAMPINAS")
ranking_lacunas(campinas_rem_17)

print("LIMEIRA")
ranking_lacunas(limeira_rem_17)

print("PAULÍNIA")
ranking_lacunas(paulinia_rem_17)


#Gráficos das 3 maiores lacunas de cada cidade
#Campinas
camp_1 <- campinas_rem_17 |>
  filter(DATA >= "2022-01-01" & DATA <= "2022-05-31") |> 
  
  ggplot(aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(color = "darkblue") +
  scale_x_date(
    date_breaks = "1 month", 
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
    title = "1ª maior lacuna de Campinas"
  )

camp_2 <- campinas_rem_17 |>
  filter(DATA >= "2021-06-01" & DATA <= "2021-11-30") |> 
  
  ggplot(aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(color = "darkblue") +
  scale_x_date(
    date_breaks = "1 month", 
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
    title = "2ª maior lacuna de Campinas"
  )

camp_3 <- campinas_rem_17 |>
  filter(DATA >= "2025-07-01" & DATA <= "2025-12-31") |> 
  
  ggplot(aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(color = "darkblue") +
  scale_x_date(
    date_breaks = "1 month", 
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
    title = "3ª maior lacuna de Campinas"
  )

#Limeira
lim_1 <- limeira_rem_17 |>
  filter(DATA >= "2022-10-01" & DATA <= "2023-03-31") |> 
  
  ggplot(aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(color = "darkblue") +
  scale_x_date(
    date_breaks = "1 month", 
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
    title = "1ª maior lacuna de Limeira"
  )

lim_2 <- limeira_rem_17 |>
  filter(DATA >= "2022-06-01" & DATA <= "2022-11-30") |> 
  
  ggplot(aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(color = "darkblue") +
  scale_x_date(
    date_breaks = "1 month", 
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
    title = "2ª maior lacuna de Limeira"
  )

lim_3 <- limeira_rem_17 |>
  filter(DATA >= "2021-06-01" & DATA <= "2021-11-30") |> 
  
  ggplot(aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(color = "darkblue") +
  scale_x_date(
    date_breaks = "1 month", 
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
    title = "3ª maior lacuna de Limeira"
  )

#Paulínia
paul_1 <- paulinia_rem_17 |>
  filter(DATA >= "2021-08-01" & DATA <= "2022-02-28") |> 
  
  ggplot(aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(color = "darkblue") +
  scale_x_date(
    date_breaks = "1 month", 
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
    title = "1ª maior lacuna de Paulínia"
  )

paul_2 <- paulinia_rem_17 |>
  filter(DATA >= "2022-06-01" & DATA <= "2022-11-30") |> 
  
  ggplot(aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(color = "darkblue") +
  scale_x_date(
    date_breaks = "1 month", 
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
    title = "2ª maior lacuna de Paulínia"
  )

paul_3 <- paulinia_rem_17 |>
  filter(DATA >= "2023-01-01" & DATA <= "2023-06-30") |> 
  
  ggplot(aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(color = "darkblue") +
  scale_x_date(
    date_breaks = "1 month", 
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
    title = "3ª maior lacuna de Paulínia"
  )


#Imputação de dados
#kNN
library(VIM)

if (!dir.exists("dados_QUALAR/dados_processados/kNN")){
  dir.create("dados_QUALAR/dados_processados/kNN")
}

imputa_MP25_kNN <- function(base){
  datas_originais <- base$DATA #Guarda as datas no formato original
  horas_mp25 <- base$HORAS_MONIT_MP25 #Remove quantidade de horas monitoradas
  horas_temp <- base$HORAS_MONIT_TEMP
  horas_umid <- base$HORAS_MONIT_UMIDADE
  
  #Converte os dias e meses para um formato que pode ser entendido pelo algoritmo kNN
  base$MES <- as.factor(format(base$DATA, "%m"))
  base$DIA_SEMANA <- as.factor(format(base$DATA, "%u"))
  
  base$DATA <- NULL #Remove a data no padrão AAAA/MM/DD e horas monitoradas
  base$HORAS_MONIT_MP25 <- NULL
  base$HORAS_MONIT_TEMP <- NULL
  base$HORAS_MONIT_UMIDADE <- NULL
  
  base_imputada <- kNN(
    data = base, 
    variable = "MEDIA_MP25",
    k = 10,
    imp_var = TRUE,
    imp_suffix = "imp"
  )
  
  base_imputada$DATA <- datas_originais
  base_imputada$HORAS_MONIT_MP25 <- horas_mp25
  base_imputada$HORAS_MONIT_TEMP <- horas_temp
  base_imputada$HORAS_MONIT_UMIDADE <- horas_umid
  
  base_imputada <- base_imputada |>
    select(-MES, -DIA_SEMANA)|>
    relocate(DATA, MEDIA_MP25, MEDIA_TEMP, MEDIA_UMIDADE, HORAS_MONIT_MP25, HORAS_MONIT_TEMP, HORAS_MONIT_UMIDADE, MEDIA_MP25_imp)
  
  return(as_tibble(base_imputada))
}

knn_campinas <- imputa_MP25_kNN(campinas_rem_17)
knn_limeira <- imputa_MP25_kNN(limeira_rem_17)
knn_paulinia <- imputa_MP25_kNN(paulinia_rem_17)

saveRDS(knn_campinas, "dados_QUALAR/dados_processados/kNN/knn_campinas.rds")
saveRDS(knn_limeira, "dados_QUALAR/dados_processados/kNN/knn_limeira.rds")
saveRDS(knn_paulinia, "dados_QUALAR/dados_processados/kNN/knn_paulinia.rds")

graf_knn_camp <- ggplot(knn_campinas, aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(aes(color = MEDIA_MP25_imp, group = 1), linewidth = 0.6) +
  scale_color_manual(
    name = "Tipo de Dado",
    values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"), 
    labels = c("Observado", "Imputado")
  ) +
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
    plot.subtitle = element_text(hjust = 0.5),   
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    title = "Série Temporal de MP2,5 com dados imputados - Campinas",
    subtitle = "Imputação realizada por kNN",
    x = "",
    y = ""
  )

graf_knn_lim <- ggplot(knn_limeira, aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(aes(color = MEDIA_MP25_imp, group = 1), linewidth = 0.6) +
  scale_color_manual(
    name = "Tipo de Dado",
    values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"), 
    labels = c("Observado", "Imputado")
  ) +
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
    plot.subtitle = element_text(hjust = 0.5),   
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    title = "Série Temporal de MP2,5 com dados imputados - Limeira",
    subtitle = "Imputação realizada por kNN",
    x = "",
    y = ""
  )

graf_knn_pau <- ggplot(knn_paulinia, aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(aes(color = MEDIA_MP25_imp, group = 1), linewidth = 0.6) +
  scale_color_manual(
    name = "Tipo de Dado",
    values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"), 
    labels = c("Observado", "Imputado")
  ) +
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
    plot.subtitle = element_text(hjust = 0.5),   
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    title = "Série Temporal de MP2,5 com dados imputados - Paulínia",
    subtitle = "Imputação realizada por kNN",
    x = "",
    y = ""
  )

#missForest
library(missForest)

if (!dir.exists("dados_QUALAR/dados_processados/missForest")){
  dir.create("dados_QUALAR/dados_processados/missForest")
}

imputa_MP25_mf <- function(base){
  datas_originais <- base$DATA #Guarda a data original
  horas_mp25 <- base$HORAS_MONIT_MP25
  horas_temp <- base$HORAS_MONIT_TEMP
  horas_umid <- base$HORAS_MONIT_UMIDADE
  
  base$MES <- as.factor(format(base$DATA, "%m"))
  base$DIA_SEMANA <- as.factor(format(base$DATA, "%u"))
  
  base$DATA <- NULL
  base$HORAS_MONIT_MP25 <- NULL
  base$HORAS_MONIT_TEMP <- NULL
  base$HORAS_MONIT_UMIDADE <- NULL
  
  base <- as.data.frame(base)
  
  resultado_mf <- missForest(
    xmis = base,
    maxiter = 10, 
    ntree = 100, 
    variablewise = TRUE,#Retorna o erro de imputação
    verbose = TRUE #Mostra na tela o acompanhamento
  )
  
  #Coluna indicando se houve imputação (TRUE) ou não (FALSE)
  base$MEDIA_MP25_imp <- is.na(base$MEDIA_MP25)  
  
  #Pega apenas os dados imputados de média do MP2,5
  base$MEDIA_MP25 <- resultado_mf$ximp$MEDIA_MP25
  
  base$DATA <- datas_originais #Restaura a data para o formato original
  base$HORAS_MONIT_MP25 <- horas_mp25
  base$HORAS_MONIT_TEMP <- horas_temp
  base$HORAS_MONIT_UMIDADE <- horas_umid
  
  print(resultado_mf$OOBerror)
  
  base <- base|>
    select(-MES, -DIA_SEMANA)|>
    relocate(DATA, MEDIA_MP25, MEDIA_TEMP, MEDIA_UMIDADE, HORAS_MONIT_MP25, HORAS_MONIT_TEMP, HORAS_MONIT_UMIDADE, MEDIA_MP25_imp)
  
  return(as_tibble(base))
}

mf_campinas <- imputa_MP25_mf(campinas_rem_17)
mf_limeira <- imputa_MP25_mf(limeira_rem_17)
mf_paulinia <- imputa_MP25_mf(paulinia_rem_17)

saveRDS(mf_campinas, "dados_QUALAR/dados_processados/missForest/mf_campinas.rds")
saveRDS(mf_limeira, "dados_QUALAR/dados_processados/missForest/mf_limeira.rds")
saveRDS(mf_paulinia, "dados_QUALAR/dados_processados/missForest/mf_paulinia.rds")

graf_mf_camp <- ggplot(mf_campinas, aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(aes(color = MEDIA_MP25_imp, group = 1), linewidth = 0.6) +
  scale_color_manual(
    name = "Tipo de Dado",
    values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"), 
    labels = c("Observado", "Imputado")
  ) +
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
    plot.subtitle = element_text(hjust = 0.5),   
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    title = "Série Temporal de MP2,5 com dados imputados - Campinas",
    subtitle = "Imputação realizada por missForest",
    x = "",
    y = ""
  )

graf_mf_lim <- ggplot(mf_limeira, aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(aes(color = MEDIA_MP25_imp, group = 1), linewidth = 0.6) +
  scale_color_manual(
    name = "Tipo de Dado",
    values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"), 
    labels = c("Observado", "Imputado")
  ) +
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
    plot.subtitle = element_text(hjust = 0.5),   
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    title = "Série Temporal de MP2,5 com dados imputados - Limeira",
    subtitle = "Imputação realizada por missForest",
    x = "",
    y = ""
  )

graf_mf_pau <- ggplot(mf_paulinia, aes(x = DATA, y = MEDIA_MP25)) +
  geom_line(aes(color = MEDIA_MP25_imp, group = 1), linewidth = 0.6) +
  scale_color_manual(
    name = "Tipo de Dado",
    values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"), 
    labels = c("Observado", "Imputado")
  ) +
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
    plot.subtitle = element_text(hjust = 0.5),   
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    title = "Série Temporal de MP2,5 com dados imputados - Paulínia",
    subtitle = "Imputação realizada por missForest",
    x = "",
    y = ""
  )

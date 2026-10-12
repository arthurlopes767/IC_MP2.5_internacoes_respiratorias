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
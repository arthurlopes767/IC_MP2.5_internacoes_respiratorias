library(tidyverse)

campinas_qualar <- readRDS("dados_QUALAR/campinas_qualar_processado.rds")
limeira_qualar <- readRDS("dados_QUALAR/limeira_qualar_processado.rds")
paulinia_qualar <- readRDS("dados_QUALAR/paulinia_qualar_processado.rds")

#Desconsidera a média de dias com menos de 12 horas de monitoramento e os coloca na mesma categoria de dias sem monitoramento (inválidos)
remove_dias_menos_12h <- function(data){
  data |>
    mutate(
      MEDIA_MP25 = ifelse((HORAS_MONIT_MP25<12), NA, MEDIA_MP25),
      HORAS_MONIT_MP25 = ifelse((HORAS_MONIT_MP25<12), 0, HORAS_MONIT_MP25)
    )
}

campinas_rem_12 <- remove_dias_menos_12h(campinas_qualar) 
limeira_rem_12 <- remove_dias_menos_12h(limeira_qualar)
paulinia_rem_12 <- remove_dias_menos_12h(paulinia_qualar)

#Gráficos após remover dias com menos de 12 horas de monitoramento
campinas_fil_12 <- ggplot(campinas_rem_12, aes(x = DATA, y = HORAS_MONIT_MP25)) +
  geom_col(fill = "lightblue", width = 1) +
  geom_hline(yintercept = 17, linewidth = 1, color = "red")+
  geom_hline(yintercept = 12, linewidth = 1, color = "red")+
  scale_x_date(
    breaks = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "1 month"),
    date_labels = "%m/%Y",
    expand = c(0, 0)
  )+
  scale_y_continuous(breaks = seq(0, 24, by = 1), expand = c(0,0)) +
  labs(title = "Horas de monitoramento por dia - Campinas", x = "Data", y = "Horas de monitoramento", caption = "*Sem dias com menos de 12 horas de monitoramento") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  ) 

saveRDS(campinas_rem_12, "dados_QUALAR/dados_processados/campinas_qualar_exc12.rds")
saveRDS(limeira_rem_12, "dados_QUALAR/dados_processados/limeira_qualar_exc12.rds")
saveRDS(paulinia_rem_12, "dados_QUALAR/dados_processados/paulinia_qualar_exc12.rds")

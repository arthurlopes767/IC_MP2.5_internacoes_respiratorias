library(tidyverse)

dados_camp <- readRDS("dados_QUALAR/Campinas_2021_2025.rds")
dados_lim <- readRDS("dados_QUALAR/Limeira_2021_2025.rds")
dados_pal <- readRDS("dados_QUALAR/Paulinia_2021_2025.rds")

agrupa_dados_diarios <- function(dados) {
  dados |>
    
    #Remove as linhas duplicadas da virada de ano
    distinct(date, .keep_all = TRUE) |>
    
    #Converte a data para o formato correto
    mutate(DATA = as.Date(date - 3600, tz = "UTC")) |> 
    #Como o QUALAR considera um dia indo da hora 1 até 24, isso faz um reajuste, e garante que a hora 24 não se torne 00h do dia seguinte
    
    #Agrupa valores diários
    group_by(DATA) |> 
    summarise(
      MEDIA_MP25 = mean(pm25, na.rm = TRUE),
      MEDIA_TEMP = mean(tc, na.rm = TRUE),
      MEDIA_UMIDADE = mean(rh, na.rm = TRUE),
      
      HORAS_MONIT_MP25 = sum(!is.na(pm25)),
      HORAS_MONIT_TEMP = sum(!is.na(tc)),
      HORAS_MONIT_UMIDADE = sum(!is.na(rh)),
      
      .groups = "drop"
    ) |> 
    
    #Corrige valores NAN das médias de dias sem valores
    mutate(
      MEDIA_MP25 = ifelse(is.nan(MEDIA_MP25), NA, MEDIA_MP25),
      MEDIA_TEMP = ifelse(is.nan(MEDIA_TEMP), NA, MEDIA_TEMP),
      MEDIA_UMIDADE = ifelse(is.nan(MEDIA_UMIDADE), NA, MEDIA_UMIDADE)
    ) |>
    
    filter(DATA >= as.Date("2021-01-01") & DATA <= as.Date("2025-12-31"))
}

qualar_proces_campinas <- agrupa_dados_diarios(dados_camp)
qualar_proces_limeira <- agrupa_dados_diarios(dados_lim)
qualar_proces_paulinia <- agrupa_dados_diarios(dados_pal)

saveRDS(qualar_proces_campinas, "dados_QUALAR/campinas_qualar_processado.rds")
saveRDS(qualar_proces_limeira, "dados_QUALAR/limeira_qualar_processado.rds")
saveRDS(qualar_proces_paulinia, "dados_QUALAR/paulinia_qualar_processado.rds")
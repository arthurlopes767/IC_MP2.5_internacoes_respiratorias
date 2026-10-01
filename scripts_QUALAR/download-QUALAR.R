library(qualR)
library(tidyverse)

#Inserindo credenciais
meu_usuario <- Sys.getenv("QUALAR_USER")
minha_senha <- Sys.getenv("QUALAR_PASS")

#Instalação do dados do QUALAR de 2021 a 2025
download_QUALAR <- function(estacao, nome_municipio){
  d2021 <- cetesb_retrieve_met_pol(meu_usuario, minha_senha, estacao, "31/12/2020", "31/12/2021")
  d2022 <- cetesb_retrieve_met_pol(meu_usuario, minha_senha, estacao, "01/01/2022", "31/12/2022")
  d2023 <- cetesb_retrieve_met_pol(meu_usuario, minha_senha, estacao, "01/01/2023", "31/12/2023")
  d2024 <- cetesb_retrieve_met_pol(meu_usuario, minha_senha, estacao, "01/01/2024", "31/12/2024")
  d2025 <- cetesb_retrieve_met_pol(meu_usuario, minha_senha, estacao, "01/01/2025", "31/12/2025")

  #Empilha os 5 anos de dados em uma única base
  bind_rows(d2021, d2022, d2023, d2024, d2025)|> 
    mutate(municipio = nome_municipio)|>
    
    #Remove as linhas duplicadas de virada de ano 
    distinct(date, .keep_all = TRUE) |>
    
    #Impede que a hora 24 (de um dia com horas de 1 a 24) no QUALAR vire 00h do dia seguinte
    mutate(DATA = as.Date(date - 3600, tz = "UTC")) |>
    
    #Garante que após a correção de horários só o período de estudo será mantido na base final
    filter(DATA >= as.Date("2021-01-01") & DATA <= as.Date("2025-12-31"))
}

qualar_Campinas <- download_QUALAR("Campinas-V.União", "Campinas")
qualar_Limeira <- download_QUALAR("Limeira", "Limeira")
qualar_Paulinia <- download_QUALAR("Paulínia-Sta Terezinha", "Paulinia")

if (!dir.exists("dados_QUALAR")){
  dir.create("dados_QUALAR")
}

saveRDS(qualar_Campinas, "dados_QUALAR/Campinas_2021_2025.rds")
saveRDS(qualar_Limeira, "dados_QUALAR/Limeira_2021_2025.rds")
saveRDS(qualar_Paulinia, "dados_QUALAR/Paulinia_2021_2025.rds")
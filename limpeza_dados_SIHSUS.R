library(microdatasus)
library(tidyverse)

sih_rd <- readRDS("dados_SIH/dados_sih_sp_2021_2025.rds")
sih_proc <- process_sih(sih_rd) #Corrige os formatos dos dados e imputa dados sobre o munícipio de residência

sih_tratado <- sih_proc |> 
  mutate(
    #Converte as datas para o formato padrão
    DT_INTER = ymd(DT_INTER),
    DT_SAIDA = ymd(DT_SAIDA),
    NASC     = ymd(NASC),
    
    #Calcula quantos dias o paciente ficou internado
    DIAS_PERM = as.numeric(DT_SAIDA - DT_INTER),
    
    #Calcula a idade do paciente
    IDADE_ANOS = floor(as.numeric(interval(NASC, DT_INTER) / years(1))),
    
    #Converte os valores gastos para dados numéricos
    VAL_TOT = as.numeric(VAL_TOT),
    
    #Padroniza o indicador de óbito (1 = Sim, 0 = Não)
    MORTE = as.integer(MORTE)
  )

#Seleciona os CIDS e municípios de estudo
municipios <- c("350950", "352690", "353650")
doencas <- c("J41", "J42", "J43", "J44", "J45")

#Filtra os dados, removendo municípios, doenças e colunas não utilizadas no estudo
sih_final <- sih_tratado |> 
  filter(MUNIC_RES %in% municipios) |> 
  filter(substr(DIAG_PRINC, 1, 3) %in% doencas) |> 
  select(-ANO_CMPT, -MES_CMTP, -munResStatus, -munResTipo) 

saveRDS(sih_final, "dados_SIH/sih_final_respiratorias")
library(microdatasus)
library(tidyverse)

sih_rd <- readRDS("dados_SIH/dados_sih_sp_2021_2025.rds")
sih_proc <- process_sih(sih_rd)

sih_tratado <- sih_proc |> 
  mutate(
    DT_INTER = ymd(DT_INTER),
    DT_SAIDA = ymd(DT_SAIDA),
    NASC     = ymd(NASC),
    
    DIAS_PERM = as.numeric(DT_SAIDA - DT_INTER),
    
    IDADE_ANOS = floor(as.numeric(interval(NASC, DT_INTER) / years(1))),
    
    VAL_TOT = as.numeric(VAL_TOT),
    
    MORTE = as.integer(MORTE)
  )

municipios <- c("350950", "352690", "353650")
doencas <- c("J41", "J42", "J43", "J44", "J45")

sih_final <- sih_tratado |> filter(MUNIC_RES %in% municipios) |> filter(substr(DIAG_PRINC, 1, 3) %in% doencas)

head(sih_final)
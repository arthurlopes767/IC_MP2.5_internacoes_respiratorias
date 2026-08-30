library(tidyverse)

sih_tratado <- readRDS("dados_SIH/sih_tratado_respiratorias.rds")

#Divide a base em 3 novas bases de acordo com o município
sih_campinas <- sih_tratado |>
  filter(munResNome == "Campinas") |>
  count(DT_INTER, name = "TOTAL_INTER")

sih_limeira <- sih_tratado |>
  filter(munResNome == "Limeira") |>
  count(DT_INTER, name = "TOTAL_INTER")

sih_paulinia <- sih_tratado |>
  filter(munResNome == "Paulínia") |>
  count(DT_INTER, name = "TOTAL_INTER")

# Cria um novo repositório com as bases finais salvas
if (!dir.exists("dados_SIH/Cidades")) {
  dir.create("dados_SIH/Cidades")
}

saveRDS(sih_campinas, "dados_SIH/Cidades/internacoes_Campinas")
saveRDS(sih_limeira, "dados_SIH/Cidades/internacoes_Limeira")
saveRDS(sih_paulinia, "dados_SIH/Cidades/internacoes_Paulinia")

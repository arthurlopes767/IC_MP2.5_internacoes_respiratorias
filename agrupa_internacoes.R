library(tidyverse)

sih_tratado <- readRDS("dados_SIH/sih_sp_tratado_respiratorias.rds")

#Função para separar os pacientes por faixa etária de acordo com a cidade
inter_idade_cidade <- function(base, nome_municipio, data_inicio = "2021-01-01", data_fim = "2025-12-31"){
  base |>
    filter(munResNome == nome_municipio) |>
    group_by(DT_INTER) |>
    summarise(
      TOTAL_INTER = n(),
      INTER_ZERO_A_CINCO_ANOS = sum(IDADE_ANOS <= 5),
      INTER_MAIOR_IGUAL_SESSENTA_ANOS = sum(IDADE_ANOS >= 60),
      .groups = "drop"
    ) |>
    
    complete(
      DT_INTER = seq.Date(as.Date(data_inicio), as.Date(data_fim), by = "day"),
      fill = list(
        TOTAL_INTER = 0,
        INTER_ZERO_A_CINCO_ANOS = 0,
        INTER_MAIOR_IGUAL_SESSENTA_ANOS = 0
      )
    )
}

#Chamada da função para cada município individualmente
sih_campinas <- inter_idade_cidade(sih_tratado,"Campinas")
sih_limeira <- inter_idade_cidade(sih_tratado, "Limeira")
sih_paulinia <- inter_idade_cidade(sih_tratado, "Paulínia")

# Cria um novo repositório com as bases finais salvas
if (!dir.exists("dados_SIH/Cidades")) {
  dir.create("dados_SIH/Cidades")
}

saveRDS(sih_campinas, "dados_SIH/Cidades/internacoes_Campinas.rds")
saveRDS(sih_limeira, "dados_SIH/Cidades/internacoes_Limeira.rds")
saveRDS(sih_paulinia, "dados_SIH/Cidades/internacoes_Paulinia.rds")

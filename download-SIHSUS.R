library(microdatasus)

#Instala as bases de dados do tipo RD do SIH-SUS
sih_rd <- fetch_datasus(
  year_start = 2021,
  month_start = 1,
  year_end = 2025,
  month_end = 12,
  uf = "SP",
  information_system = "SIH-RD",
  vars = c("N_AIH", "DT_INTER", "DT_SAIDA", "ANO_CMPT", "MES_CMPT", "DIAG_PRINC", 
           "MUNIC_MOV", "MUNIC_RES", "NASC", "SEXO", "VAL_TOT", "MORTE", 
           "RACA_COR"),
  timeout = 1200
)

#Cria um repositório para guardar os dados
if (!dir.exists("dados_SIH")) {
  dir.create("dados_SIH")
}

saveRDS(sih_rd, "dados_SIH/dados_sih_sp_2021_2025.rds")
library(microdatasus)

#Instala as bases de dados do tipo RD do SIH-SUS
sih_rd <- fetch_datasus(
  year_start = 2021,
  month_start = 1,
  year_end = 2026, 
  month_end = 6,
  uf = "SP",
  information_system = "SIH-RD",
  vars = c("DT_INTER", "DT_SAIDA", "DIAG_PRINC","MUNIC_MOV", "MUNIC_RES",
           "VAL_TOT", "NASC", "SEXO", "MORTE", "RACA_COR"),
  timeout = 1200
)

#Cria um repositório para guardar os dados
if (!dir.exists("dados_SIH")) {
  dir.create("dados_SIH")
}

saveRDS(sih_rd, "dados_SIH/sih_sp_2021_1s2026.rds")
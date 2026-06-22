# Análise de concluintes de bacharelados em Saúde Coletiva e áreas correlatas
# Censo da Educação Superior/INEP, 2013-2023

rm(list = ls())
gc()
cat("\014")

pacotes <- c(
  "data.table", "geobr", "ggplot2", "sf",
  "rnaturalearth", "rnaturalearthdata", "scales"
)

faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]

if (length(faltantes) > 0) {
  stop(
    "Instale os pacotes antes de executar: ",
    paste(faltantes, collapse = ", ")
  )
}

library(data.table)
library(geobr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scales)

# CONFIGURAÇÃO ---------------------------------------------------------------

# Opção 1: defina a variável INEP_DATA_DIR antes de executar.
# Opção 2: substitua o caminho abaixo pelo diretório local dos arquivos ZIP.
pasta_dados <- Sys.getenv(
  "INEP_DATA_DIR",
  unset = "C:/CAMINHO/PARA/INEP_DADOS_2013_2024"
)

anos_analise <- 2013:2023
dir.create("resultados", showWarnings = FALSE)

if (!dir.exists(pasta_dados)) {
  stop(
    "Diretório não encontrado: ", pasta_dados,
    "\nDefina INEP_DATA_DIR ou altere pasta_dados no script."
  )
}

# IMPORTAÇÃO -----------------------------------------------------------------

arquivos_zip <- list.files(
  path = pasta_dados,
  pattern = "\\.zip$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

anos_zip <- as.integer(sub(".*_(20[0-9]{2})\\.zip$", "\\1", arquivos_zip))
arquivos_zip <- arquivos_zip[anos_zip %in% anos_analise]

arquivos_encontrados <- rbindlist(lapply(arquivos_zip, function(zip_file) {
  conteudo <- unzip(zip_file, list = TRUE)
  nome_encontrado <- grep(
    "MICRODADOS_CADASTRO_CURSOS",
    conteudo$Name,
    value = TRUE,
    ignore.case = TRUE,
    useBytes = TRUE
  )

  data.table(
    zip = zip_file,
    arquivo_interno = nome_encontrado
  )
}))

if (nrow(arquivos_encontrados) != length(anos_analise)) {
  warning(
    "Esperados ", length(anos_analise),
    " arquivos, mas foram encontrados ", nrow(arquivos_encontrados), "."
  )
}

lista_dados <- vector("list", nrow(arquivos_encontrados))

for (i in seq_len(nrow(arquivos_encontrados))) {
  ano <- as.integer(
    sub(".*_(20[0-9]{2})\\.zip$", "\\1", arquivos_encontrados$zip[i])
  )

  message("Lendo ", ano, " — arquivo ", i, " de ", nrow(arquivos_encontrados))

  arquivo_temp <- tempfile(
    pattern = paste0("cursos_", ano, "_"),
    fileext = ".csv"
  )

  entrada <- unz(
    arquivos_encontrados$zip[i],
    arquivos_encontrados$arquivo_interno[i],
    open = "rb"
  )
  saida <- file(arquivo_temp, open = "wb")

  tryCatch(
    {
      repeat {
        bloco <- readBin(entrada, what = "raw", n = 1024^2)
        if (length(bloco) == 0) break
        writeBin(bloco, saida)
      }
    },
    finally = {
      close(entrada)
      close(saida)
    }
  )

  dados_ano <- fread(
    arquivo_temp,
    sep = ";",
    encoding = "UTF-8",
    showProgress = TRUE
  )

  colunas_texto <- names(dados_ano)[
    vapply(dados_ano, is.character, logical(1))
  ]

  dados_ano[
    ,
    (colunas_texto) := lapply(
      .SD,
      iconv,
      from = "latin1",
      to = "UTF-8"
    ),
    .SDcols = colunas_texto
  ]

  dados_ano[, ANO_ARQUIVO := ano]
  lista_dados[[i]] <- dados_ano
  unlink(arquivo_temp)
}

dados_cursos <- rbindlist(lista_dados, use.names = TRUE, fill = TRUE)
rm(lista_dados)
gc()

# SELEÇÃO E PADRONIZAÇÃO ------------------------------------------------------

cursos_saude <- c(
  "Saúde Coletiva",
  "Saúde Pública",
  "Gestão em Saúde Coletiva Indígena",
  "Administração em Sistemas e Serviços de Saúde",
  "Gestão em Saúde",
  "Gestão de Saúde",
  "Gestão de Saúde Pública",
  "Gestão de Saúde Coletiva",
  "Gestão de Serviços de Saúde",
  "Gestão em Vigilância em Saúde",
  "Vigilância em Saúde",
  "Auditoria em Saúde"
)

dados_saude <- dados_cursos[
  NU_ANO_CENSO %in% anos_analise &
    TP_GRAU_ACADEMICO == 1 &
    !is.na(NO_CURSO) &
    tolower(trimws(NO_CURSO)) %in% tolower(cursos_saude)
]

dados_saude[
  ,
  NO_CURSO_PADRAO := cursos_saude[
    match(toupper(trimws(NO_CURSO)), toupper(cursos_saude))
  ]
]

dados_saude[
  NO_CURSO_PADRAO == "Gestão de Saúde",
  NO_CURSO_PADRAO := "Gestão em Saúde"
]

dados_saude[
  NO_CURSO_PADRAO == "Gestão de Saúde Coletiva",
  NO_CURSO_PADRAO := "Gestão em Saúde Coletiva"
]

# TABELAS --------------------------------------------------------------------

concluintes_por_ano <- dados_saude[
  ,
  .(TOTAL_CONCLUINTES = sum(QT_CONC, na.rm = TRUE)),
  by = NU_ANO_CENSO
][order(NU_ANO_CENSO)]

total_concluintes_curso <- dados_saude[
  ,
  .(TOTAL_CONCLUINTES = sum(QT_CONC, na.rm = TRUE)),
  by = NO_CURSO_PADRAO
][order(-TOTAL_CONCLUINTES)]

concluintes_uf <- dados_saude[
  !is.na(NO_UF) & trimws(NO_UF) != "",
  .(TOTAL_CONCLUINTES = sum(QT_CONC, na.rm = TRUE)),
  by = NO_UF
][order(-TOTAL_CONCLUINTES)]

concluintes_municipio <- dados_saude[
  !is.na(CO_MUNICIPIO),
  .(TOTAL_CONCLUINTES = sum(QT_CONC, na.rm = TRUE)),
  by = .(CO_MUNICIPIO, NO_MUNICIPIO, NO_UF)
][TOTAL_CONCLUINTES > 0]

concluintes_rede <- dados_saude[
  ,
  .(TOTAL_CONCLUINTES = sum(QT_CONC, na.rm = TRUE)),
  by = TP_REDE
][
  ,
  REDE := fcase(
    TP_REDE == 1, "Pública",
    TP_REDE == 2, "Privada",
    default = "Não informada"
  )
][, .(REDE, TOTAL_CONCLUINTES)]

concluintes_modalidade <- dados_saude[
  ,
  .(TOTAL_CONCLUINTES = sum(QT_CONC, na.rm = TRUE)),
  by = TP_MODALIDADE_ENSINO
][
  ,
  MODALIDADE := fcase(
    TP_MODALIDADE_ENSINO == 1, "Presencial",
    TP_MODALIDADE_ENSINO == 2, "A distância",
    default = "Não informada"
  )
][, .(MODALIDADE, TOTAL_CONCLUINTES)]

tabela_ies <- data.table(
  CO_IES = c(
    1, 2, 17, 38, 55, 409, 549, 570, 571, 575, 578, 580,
    581, 586, 717, 789, 2409, 3172, 3336, 15001, 15059, 18440
  ),
  NO_IES = c(
    "UNIVERSIDADE FEDERAL DE MATO GROSSO",
    "UNIVERSIDADE DE BRASÍLIA",
    "UNIVERSIDADE FEDERAL DE UBERLÂNDIA",
    "UNIVERSIDADE DO ESTADO DO PARÁ",
    "UNIVERSIDADE DE SÃO PAULO",
    "UNIVERSIDADE DE PERNAMBUCO",
    "UNIVERSIDADE FEDERAL DO ACRE",
    "UNIVERSIDADE FEDERAL DO RIO GRANDE DO NORTE",
    "UNIVERSIDADE FEDERAL DO PARANÁ",
    "UNIVERSIDADE FEDERAL DE MINAS GERAIS",
    "UNIVERSIDADE FEDERAL DA BAHIA",
    "UNIVERSIDADE FEDERAL DE PERNAMBUCO",
    "UNIVERSIDADE FEDERAL DO RIO GRANDE DO SUL",
    "UNIVERSIDADE FEDERAL DO RIO DE JANEIRO",
    "FUNDAÇÃO UNIVERSIDADE FEDERAL DE CIÊNCIAS DA SAÚDE DE PORTO ALEGRE",
    "UNIVERSIDADE FEDERAL DE RORAIMA",
    "CENTRO UNIVERSITÁRIO TABOSA DE ALMEIDA",
    "UNIVERSIDADE DO ESTADO DO AMAZONAS",
    "UNIVERSIDADE ESTADUAL DO RIO GRANDE DO SUL",
    "UNIVERSIDADE FEDERAL DA INTEGRAÇÃO LATINO-AMERICANA",
    "UNIVERSIDADE FEDERAL DO OESTE DO PARÁ",
    "UNIVERSIDADE FEDERAL DO SUL E SUDESTE DO PARÁ"
  )
)

concluintes_ies <- dados_saude[
  ,
  .(TOTAL_CONCLUINTES = sum(QT_CONC, na.rm = TRUE)),
  by = CO_IES
]

concluintes_ies <- merge(
  tabela_ies,
  concluintes_ies,
  by = "CO_IES",
  all.x = TRUE
)[order(-TOTAL_CONCLUINTES)]

# EXPORTAÇÃO -----------------------------------------------------------------

fwrite(concluintes_por_ano, "resultados/concluintes_por_ano.csv")
fwrite(total_concluintes_curso, "resultados/concluintes_por_curso.csv")
fwrite(concluintes_uf, "resultados/concluintes_por_uf.csv")
fwrite(concluintes_municipio, "resultados/concluintes_por_municipio.csv")
fwrite(concluintes_rede, "resultados/concluintes_por_rede.csv")
fwrite(concluintes_modalidade, "resultados/concluintes_por_modalidade.csv")
fwrite(concluintes_ies, "resultados/concluintes_por_ies.csv")

# MAPAS ----------------------------------------------------------------------

mapa_estados <- read_state(
  year = 2020,
  simplified = TRUE,
  showProgress = FALSE
)

mapa_estados$TOTAL_CONCLUINTES <- concluintes_uf$TOTAL_CONCLUINTES[
  match(mapa_estados$name_state, concluintes_uf$NO_UF)
]
mapa_estados$TOTAL_CONCLUINTES[
  is.na(mapa_estados$TOTAL_CONCLUINTES)
] <- 0

america_sul <- ne_countries(
  continent = "South America",
  scale = "medium",
  returnclass = "sf"
)
america_sul <- st_transform(america_sul, st_crs(mapa_estados))

tema_transparente <- theme_void() +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA)
  )

mapa_concluintes <- ggplot() +
  geom_sf(
    data = america_sul,
    fill = "grey85",
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf(
    data = mapa_estados,
    aes(fill = TOTAL_CONCLUINTES),
    color = "white",
    linewidth = 0.35
  ) +
  scale_fill_gradient(
    low = "#DCEAF5",
    high = "#075A9C",
    name = "Concluintes",
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  coord_sf(xlim = c(-82, -34), ylim = c(-57, 14), expand = FALSE) +
  tema_transparente

ggsave(
  "resultados/mapa_concluintes_uf.png",
  mapa_concluintes,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "transparent"
)

mapa_municipios <- read_municipality(
  code_muni = "all",
  year = 2020,
  simplified = TRUE,
  showProgress = FALSE
)

mapa_municipios$code_muni <- as.character(mapa_municipios$code_muni)
concluintes_municipio[, CO_MUNICIPIO := as.character(CO_MUNICIPIO)]

mapa_municipios <- merge(
  mapa_municipios,
  concluintes_municipio,
  by.x = "code_muni",
  by.y = "CO_MUNICIPIO"
)

pontos_municipios <- mapa_municipios |>
  st_transform(5880) |>
  st_point_on_surface() |>
  st_transform(4326)

mapa_pontos <- ggplot() +
  geom_sf(
    data = america_sul,
    fill = "grey85",
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf(
    data = mapa_estados,
    fill = "grey96",
    color = "grey70",
    linewidth = 0.3
  ) +
  geom_sf(
    data = pontos_municipios,
    aes(size = TOTAL_CONCLUINTES),
    color = "#075A9C",
    alpha = 0.7
  ) +
  scale_size_area(
    max_size = 14,
    name = "Concluintes",
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  coord_sf(xlim = c(-82, -34), ylim = c(-57, 14), expand = FALSE) +
  tema_transparente

ggsave(
  "resultados/mapa_concluintes_municipios.png",
  mapa_pontos,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "transparent"
)

message("Análise concluída. Arquivos salvos em resultados/.")

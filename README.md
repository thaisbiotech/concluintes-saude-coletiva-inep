# Concluintes de bacharelados em Saúde Coletiva — INEP

Análise descritiva dos concluintes de cursos de bacharelado em Saúde Coletiva,
Saúde Pública, Gestão e Vigilância em Saúde, com base nos microdados do Cadastro
de Cursos do Censo da Educação Superior/INEP, de 2013 a 2023.

## Metodologia

- Fonte: microdados do Censo da Educação Superior/INEP.
- Download dos microdados: [portal oficial do INEP](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-da-educacao-superior).
- Período: 2013–2023.
- Grau acadêmico: bacharelado (`TP_GRAU_ACADEMICO = 1`).
- Seleção: denominações relacionadas à Saúde Coletiva, Saúde Pública, Gestão e
  Vigilância em Saúde.
- Tratamento: padronização de grafias, maiúsculas/minúsculas e preposições.
- Indicador: soma da variável `QT_CONC`.
- Distribuição territorial: município (`CO_MUNICIPIO`) e UF (`NO_UF`).

## Estrutura

- `analise_concluintes_inep.R`: importação, tratamento, tabelas e mapas.
- `resultados/`: tabelas CSV e mapas PNG gerados pelo script.

Os arquivos ZIP originais não são versionados porque são grandes e devem ser
obtidos diretamente no portal do INEP.

## Como executar

1. Baixe os arquivos anuais na página oficial dos
   [microdados do Censo da Educação Superior/INEP](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-da-educacao-superior)
   e coloque os arquivos ZIP em uma pasta local.
2. Instale os pacotes necessários:

```r
install.packages(c(
  "data.table", "geobr", "ggplot2", "sf",
  "rnaturalearth", "rnaturalearthdata", "scales"
))
```

3. Informe a pasta dos ZIPs:

```r
Sys.setenv(
  INEP_DATA_DIR = "C:/caminho/para/INEP_DADOS_2013_2024"
)
```

4. Execute:

```r
source("analise_concluintes_inep.R", encoding = "UTF-8")
```

## Saídas

O script gera:

- concluintes por ano;
- concluintes por curso;
- concluintes por UF e município;
- concluintes por rede e modalidade;
- concluintes por instituição;
- mapa coroplético por UF;
- mapa de pontos por município.

## Observação

O repositório contém apenas código e resultados derivados. Os microdados brutos
permanecem fora do GitHub.

## Apoio no desenvolvimento

Análise desenvolvida pela autora com apoio do Codex, da OpenAI, na organização,
revisão e documentação do código em R. As decisões metodológicas, a conferência
dos resultados e a responsabilidade pelo conteúdo são da autora.


# Rendimento médio do trabalho de idosos de mais de 65 anos, renda familiar per capita < 1.4 sm, que trabalham

library(PNADcIBGE)
library(tidyverse)
library(hutils)
library(survey)
library(dplyr)


surveyPnad <- get_pnadc(year = 2024, interview = 1)
dfPnadOrig <- surveyPnad$variables 

dfPnad <- dfPnadOrig %>%
  mutate(peso = surveyPnad$pweights) %>%
  mutate(peso_normalizado = peso/sum(peso)) %>%
  mutate(domicilio = paste0(UPA, V1008, V1014)) %>%
  mutate(individuo = paste0(UPA, V1008, V1014, V2003)) %>%
  mutate(condicao_trabalho = paste0(VD4001)) %>%
  mutate(rendimento = paste0(VD4020)) %>%
  mutate(
    rendimento = na_if(rendimento, "NA"),    # transforma "NA" em NA real
    rendimento = na_if(rendimento, ""),      # transforma string vazia em NA
    rendimento = na_if(rendimento, "--"),    # adiciona mais casos conforme necessário
    rendimento = as.numeric(gsub(",", ".", rendimento)))  %>% # converte para número (se tiver vírgula decimal)
  mutate(rendimento = as.numeric(rendimento)) %>%
  mutate(rendimento_p = (rendimento * peso)) %>%
  mutate(idoso = V2009>=65) %>% 
  mutate(BPC = V5001A) %>%
  mutate(idoso_nao_bpc = idoso & (BPC == "Não")) # chave para idoso beneficiario do BPC

dfPnad <- dfPnad %>%
  group_by(domicilio) %>%
  mutate(
    renda_total = sum(rendimento, na.rm = TRUE),
    n_moradores = n(),
    renda_per_capita = renda_total / n_moradores
  ) %>%
  ungroup()

dfPnadFiltered <- dfPnad %>%
  filter(condicao_trabalho == "Pessoas na força de trabalho") %>%
  filter(idoso_nao_bpc == TRUE) %>%
  filter(renda_per_capita <= 1412/4) %>%
  filter(!is.na(rendimento))

ggplot(dfPnadFiltered, aes(x = rendimento)) +
geom_histogram(binwidth = 100, fill = "steelblue", color = "white") +
labs(
  title = "Distribuição de domicílios por faixa de renda per capita",
  x = "Renda domiciliar per capita (R$)",
  y = "Número de domicílios"
) +
xlim(0, 2000) +
theme_minimal()

# Calcula valor médio (ponderado) do rendimento dos coresidentes
wavg_idosos_trabalhadores <- sum(dfPnadFiltered$rendimento_p, na.rm = TRUE) / sum(dfPnadFiltered$peso, na.rm = TRUE)
print(wavg_idosos_trabalhadores)

# Define o desenho amostral com os pesos
design <- svydesign(ids = ~1, weights = ~peso, data = dfPnadFiltered)

# Calcula a média e erro padrão da proporção de participação na força de trabalho
resultado <- svymean(~rendimento, design)

# Exibe resultado
print(resultado)

# Quantidade de observações
length(dfPnadFiltered$rendimento)

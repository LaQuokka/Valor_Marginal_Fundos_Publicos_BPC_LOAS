library(PNADcIBGE)
library(tidyverse)
library(hutils)
library(survey)
library(dplyr)


### Cálculo dO rendimento médio ponderado de co-residentes do idoso beneficiário que trabalham

surveyPnad <- get_pnadc(year = 2024, interview = 1)

dfPnad <- surveyPnad$variables 
  # Cria coluna peso
  mutate(peso = surveyPnad$pweights) %>%
  # Cria coluna peso normalizad
  mutate(peso_normalizado = peso/sum(peso)) %>% 
  # Cria coluna domicilio
  mutate(domicilio = paste0(UPA, V1008, V1014)) %>%
  # Cria coluna indivíduo
  mutate(individuo = paste0(UPA, V1008, V1014, V2003)) %>%
  # Cria coluna condicao_trabalho
  mutate(condicao_trabalho = paste0(VD4001)) %>%
  # Cria coluna rendimento
  mutate(rendimento = paste0(VD4020)) %>%
  # Trata a coluna de rendimento
  mutate(
    rendimento = na_if(rendimento, "NA"),    # transforma "NA" em NA real
    rendimento = na_if(rendimento, ""),      # transforma string vazia em NA
    rendimento = na_if(rendimento, "--"),    # adiciona mais casos conforme necessário
    rendimento = as.numeric(gsub(",", ".", rendimento)))  # converte para número (se tiver vírgula decimal)
  # Cria coluna rendimento ponderado
  dfPnad %>% mutate(rendimento_p = (rendimento * peso)) %>% 
  # chave para pesssoas de 18 a 49 anos
  mutate(residente_18_49 = V2009<= 49 & V2009 >= 18) %>%  
  # chave para idoso de >= 65 anos
  mutate(idoso = V2009>=65) %>%  
  # chave que indica recebimento do BPC
  mutate(BPC = V5001A) %>%
  # chave para idoso beneficiario do BPC
  mutate(idoso_bpc = idoso & (BPC == "Sim")) %>%  
  # chave que indica se o domicilio possui pelo menos 1 idoso beneficiario do BPC
  mutate(domicilio_idoso_bpc = any(idoso_bpc == TRUE)) %>% 
  ungroup() %>% 
  # filtra para somente domicilios com idosos beneficiario do BPC
  filter(domicilio_idoso_bpc == TRUE) 


coresidentes <- dfBPC %>% 
  filter(idoso_bpc == FALSE) %>%
  filter(!is.na(condicao_trabalho) & !is.na(rendimento) ) %>%
  filter(condicao_trabalho == "Pessoas na força de trabalho") # filtra df para os individuos 
                                                               # co-residentes que tem informacao sobre trabalho

# Calcula valor médio (ponderado) do rendimento dos coresidentes
media_ponderada_rendimento_coresidentes <- sum(coresidentes$rendimento_p, na.rm = TRUE) / sum(coresidentes$peso, na.rm = TRUE)
print(media_ponderada_rendimento_coresidentes)

# Define o desenho amostral com os pesos
design <- svydesign(ids = ~1, weights = ~peso, data = coresidentes)

# Calcula a média e erro padrão da proporção de participação na força de trabalho
resultado <- svymean(~rendimento, design)

# Exibe resultado
print(resultado)

# Quantidade de observações
length(coresidentes$rendimento)

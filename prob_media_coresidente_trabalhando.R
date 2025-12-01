library(PNADcIBGE)
library(tidyverse)
library(hutils)
library(survey)
library(dplyr)


### Cálculo da proporção ponderada de co-residentes do idoso beneficiário que trabalham

surveyPnad <- get_pnadc(year = 2024, interview = 1)

dfPnad <- surveyPnad$variables # transforma em df

dfPnad <- dfPnad %>%
  mutate(peso = surveyPnad$pweights)

dfPnad <- dfPnad %>%
  mutate(peso_normalizado = peso/sum(peso))

dfPnad <- dfPnad %>%
  mutate(domicilio = paste0(UPA, V1008, V1014))

dfPnad <- dfPnad %>%
  mutate(individuo = paste0(UPA, V1008, V1014, V2003))

dfPnad <- dfPnad %>%
  mutate(condicao_trabalho = paste0(VD4001))

dfPnad <- dfPnad %>%
  mutate(rendimento = paste0(VD4020))

dfPnad <- dfPnad %>%
  mutate(
    rendimento = na_if(rendimento, "NA"),    # transforma "NA" em NA real
    rendimento = na_if(rendimento, ""),      # transforma string vazia em NA
    rendimento = na_if(rendimento, "--"),    # adiciona mais casos conforme necessário
    rendimento = as.numeric(gsub(",", ".", rendimento)))  # converte para número (se tiver vírgula decimal)

dfPnad <- dfPnad %>% mutate(rendimento_p = (rendimento * peso))

dfPnad <- dfPnad %>% 
  mutate(residente_18_49 = V2009<= 49 & V2009 >= 18) # chave para pesssoas de 18 a 49 anos

dfPnad <- dfPnad %>% 
  mutate(idoso = V2009>=65) # chave para idoso de >= 65 anos

dfPnad <- dfPnad %>% 
  mutate(BPC = V5001A) # chave que indica recebimento do BPC

dfPnad <- dfPnad %>% 
  mutate(idoso_bpc = idoso & (BPC == "Sim")) # chave para idoso beneficiario do BPC

dfPnad <- dfPnad %>%
  group_by(domicilio) %>%
  mutate(domicilio_idoso_bpc = any(idoso_bpc == TRUE)) %>% 
  ungroup() # chave que indica se o domicilio possui pelo menos 1 idoso beneficiario do BPC

dfBPC <- dfPnad %>%
  filter(domicilio_idoso_bpc == TRUE) # filtra para somente domicilios com idosos beneficiario do BPC

# Cria coluna com a participaçao no mercado de trabalho ponderada de cada individuo
coresidentes <- dfBPC %>% 
  filter(idoso_bpc == FALSE) %>%
  filter(!is.na(condicao_trabalho)) %>%                                       
  mutate(bool_trabalho = condicao_trabalho == "Pessoas na força de trabalho") %>% 
  mutate(bool_trabalho_p = bool_trabalho * peso)                                                                           

# calcula probabilidade média ponderada de estar trabalhando sendo um co-residente de um idoso que recebe BPC

media_ponderada_participacao_trabalho <- sum(coresidentes$bool_trabalho_p, na.rm = TRUE) / sum(coresidentes$peso, na.rm = TRUE)
print(media_ponderada_participacao_trabalho)

# Define o desenho amostral com os pesos
design <- svydesign(ids = ~1, weights = ~peso, data = coresidentes)

# Calcula a média e erro padrão da proporção de participação na força de trabalho
resultado <- svymean(~bool_trabalho, design)

# Exibe resultado
print(resultado)

# Quantidade de observações
length(coresidentes$bool_trabalho)

library(dplyr)
library(ggplot2)
library(tidyselect)
source("R/functions/knit_kable.R")

# df_original <- readxl::read_xlsx("atv_fis_ufsm_ajustado.xlsx")
# 
# unique(df$Genero) # tudo certo
# unique(df$Curso) # tudo certo
# unique(df$Turno) # "Norturno"
# unique(df$P1) # tudo certo
# unique(df$P4) # tudo certo
# unique(df$P6) # tudo certo
# unique(df$P8) # tudo certo
# unique(df$P9) # tudo certo

# df <- df_original %>% 
#   mutate(Turno = ifelse(Turno == "Diurno", "Diurno", "Noturno"))

# df %>% 
#   writexl::write_xlsx("atv_fis_ufsm_ajustado_v2.xlsx")

df_original <- readxl::read_xlsx("Dados/atv_fis_ufsm_ajustado_v2.xlsx")

df <- df_original %>% 
  filter(P5 != "REMOVER") %>%  # REMOVE PESSOAS QUE PRATICAM ATV "NENHUMA VEZ",
                               # MAS TÊM HORAS DE ATIVIDADES
  select(-Turno)

# Definindo os grupos ativos e não ativos
df <- df %>% 
  mutate(P5 = as.numeric(P5),
         fisicamente_ativo = ifelse(P5 >= 2.5, "Sim", "Não"))

## Comparando os grupos
# Análise descritiva
df %>% 
  tidyr::pivot_longer(cols = where(is.numeric),
                      names_to = "variavel_num",
                      values_to = "valores_num") %>%
  group_by(variavel_num, fisicamente_ativo) %>% 
  summarise(`Media` = mean(valores_num, na.rm = T),
            `Desvio-Padrao` = sd(valores_num, na.rm = T),
            `Minimo` = min(valores_num, na.rm = T),
            Mediana = median(valores_num, na.rm = T),
            `Maximo` = max(valores_num, na.rm = T)) %>% 
  mutate(across(3:4, round, 2)) %>% 
  rename(`Variável` = variavel_num) %>% 
  knit_kable(caption = "Tabela 1. Estatísticas descritivas das variáveis numéricas.")
  
# Frequências (vars qualitativas)
df %>%
  select(!where(is.numeric), -Data, -coment, -P10) %>%
  tidyr::pivot_longer(cols = !fisicamente_ativo,
                      names_to = "variavel_quali",
                      values_to = "valores_quali") %>%
  group_by(variavel_quali, fisicamente_ativo) %>%
  count(valores_quali) %>% 
  tidyr::pivot_wider(names_from = fisicamente_ativo,
                     names_prefix = "ativo_",
                     values_from = n) %>%
  group_by(variavel_quali) %>% 
  mutate(ativo_Não = ifelse(is.na(`ativo_Não`), 0, `ativo_Não`),
         ativo_Sim = ifelse(is.na(`ativo_Sim`), 0, `ativo_Sim`),
         porcentagem_ativo_Não = scales::percent(`ativo_Não`/sum(`ativo_Não`, na.rm = T), accuracy = 0.01),
         porcentagem_ativo_Sim = scales::percent(`ativo_Sim`/sum(`ativo_Sim`, na.rm = T), accuracy = 0.01))

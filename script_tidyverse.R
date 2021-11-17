# carregar os pacotes -----------------------------------------------------

install.packages("tidyverse")

library(tidyverse) #carregar o pacote tidyverse


# carregar os dados -------------------------------------------------------

#conferir o dir()

dir()

pinguins <- read.csv("dados_pinguins.csv")

# ver os dados ------------------------------------------------------------

view(pinguins)

# PIPE --------------------------------------------------------------------

#faz parte do pacote magrittr
#podemos facilitar o funcionamento das funções
#pipe significa "canos" e o sinal é: %>% (atalho: Ctrl+Shift+m)

#Receita de bolo sem pipe. Tente entender o que é preciso fazer.

esfrie(
  asse(
    coloque(
      bata(
        acrescente(
          recipiente(
            rep("farinha", 2),
            "água",
            "fermento",
            "leite",
            "óleo"
          ),
          "farinha",
          ate = "macio"
        ),
        duracao = "3min"
      ),
      lugar = "forma",
      tipo = "grande",
      untada = TRUE
    ),
    duracao = "50min"
  ),
  lugar = "geladeira",
  duracao = "20min"
)


# Veja como o código acima pode ser reescrito utilizando-se o pipe.
# Agora realmente se parece com uma receita de bolo.

recipiente(rep("farinha", 2), "água", "fermento", "leite", "óleo") %>%
  acrescente("farinha", ate = "macio") %>%
  bata(duracao = "3min") %>%
  coloque(lugar = "forma", tipo = "grande", untada = TRUE) %>%
  asse(duracao = "50min") %>%
  esfrie(lugar = "geladeira", duracao = "20min")



# agora vamos ver e editar os dados ----------------------------------------

#ao invés de
view(pinguins)

#podemos usar
pinguins %>%
  view()


#ao invés de
pinguins %>%
  str()

#podemos usar
pinguins %>%
  glimpse()


# tibble ------------------------------------------------------------------

#nossos dados estão em formato dataframe
#olha como fica gigante no console

pinguins

#o tibble é a "nova versão" do dataframe

pinguins %>%
  as_tibble()

#por isso vamos tornar essa a versão padrão

pinguins <- pinguins %>%
  as_tibble()

# janitor -----------------------------------------------------------------

library(janitor)

# e se os nomes das colunas estivessem uma zona?

pinguins_zoado <- read.csv("dados_pinguins_nomescolunas2.csv")

#podemos usar o rename! Mas para muitas colunas é trabalhoso
pinguins_clean <- pinguins_zoado %>%
  rename(especie = "Espécie",
         ilha = "Ilha",
         comprimento_bico = "Comprimento.Bico",
         profundidade_bico = "Profundidade.Bico",
         comprimento_nadadeira = "Comprimento.Nadadeira",
         massa_corporal = "Massa.Corporal",
         sexo = "Sexo",
         ano = "Ano"
         )


#para isso existe o janitor!
library(janitor)

pinguins_clean <- pinguins_zoado %>%
  clean_names()

# filtrar -----------------------------------------------------------------

## filter()

pinguins %>%
  filter(ilha == "Dream")

pinguins %>%
  filter(ilha != "Dream")

#exercicio!
#filtrem pinguins com comprimento do bico maior ou igual 45!
pinguins %>%
  filter(comprimento_bico >= 45)

pinguins %>%
  filter(!is.na(sexo))

pinguins %>%
  filter(ilha == "Dream",
         comprimento_bico >= 45,
         !is.na(sexo))

# ordenar -----------------------------------------------------------------

## arrange()

pinguins %>%
  arrange(comprimento_bico)

pinguins %>%
  arrange(desc(comprimento_bico))

pinguins %>%
  arrange(-comprimento_bico)

pinguins %>% 
  arrange(-massa_corporal)

#exercicio - ordenem com base na massa corporal
pinguins %>%
  arrange(massa_corporal)


# criar colunas -----------------------------------------------------------


## mutate() apenas cria coluna

pinguins %>%
  mutate(area_bico = comprimento_bico*profundidade_bico) %>%
  view()

pinguins %>%
  mutate(massa_kg = massa_corporal/1000)%>%
  view()


#exercicio!
#criem uma tabela com os dados dos pinguins + uma coluna com a soma do comprimento do bico e da asa
pinguins %>% 
  mutate(comp_bico_asa = comprimento_bico + comprimento_nadadeira) %>% 
  view()

pinguins %>%
  mutate(super_comprimento = comprimento_bico+comprimento_nadadeira)%>%
  view()


##transmute() mantém apenas a coluna criada

pinguins %>%
  transmute(area_bico = comprimento_bico*profundidade_bico)


# selecionar colunas ------------------------------------------------------

##select()

pinguins %>%
  select(especie, ilha, sexo, ano)

pinguins %>%
  select(especie,
         ilha,
         comprimento_bico,
         profundidade_bico,
         comprimento_nadadeira)


pinguins %>%
  select(especie:comprimento_nadadeira)


pinguins %>%
  select(-ano)

pinguins %>% 
  select(comprimento_bico:massa_corporal, ano) %>% 
  view()


# sumarizar bases ---------------------------------------------------------

##summarise() ou summarize() - é um resumo

pinguins %>%
  summarise(media_comprimento = mean(comprimento_bico, na.rm = TRUE),
            mediana_profundidade = median(profundidade_bico, na.rm = TRUE),
            media_profundidade = mean(profundidade_bico, na.rm = TRUE),
            mediana_comprimento = median(comprimento_bico, na.rm = TRUE))

#group_by()
pinguins %>%
  group_by(especie) %>%
  summarise(media_comprimento = mean(comprimento_bico, na.rm = TRUE),
            mediana_comprimento = median(comprimento_bico, na.rm = TRUE))

#exercicio!!
##vejam a média da profundidade do bico dos pinguins por ilha!
pinguins %>%
  group_by(ano) %>%
  summarise(media_profundidade = mean(profundidade_bico, na.rm = TRUE))


##count()
pinguins %>%
  count(sexo)


# distinct ----------------------------------------------------------------

#retirando valores repetidos!


pinguins %>%
  distinct()

pinguins %>%
  distinct(especie, .keep_all = TRUE)


pinguins %>%
  distinct(ano)


# exercicio ---------------------------------------------------------------

##seu projeto de pesquisa envolve estudar as medidas de nadadeira e
##massa corporal dos pinguins fêmea da espécie "Pinguim-de-adélia" da ilha "Biscoe".
##Você usará um índice obtido a partir da multiplicação do comprimento da nadadeira
##pela massa corporal, chamado de IMC_pinguin.
##Por fim, faça uma sumarização com a média do IMC dos pinguins, agrupados por ano.


pinguins %>%
  filter(especie == "Pinguim-de-adélia",
         sexo == "fêmea",
         ilha == "Biscoe") %>%
  mutate(IMC_pinguin = comprimento_nadadeira*massa_corporal) %>%
  group_by(ano) %>%
  summarise(media_IMC = mean(IMC_pinguin, na.rm = TRUE))



# INTERVALO ---------------------------------------------------------------






# ggplot ------------------------------------------------------------------

#o ggplot é como a pintura de um quadro

#esse é o quadro em branco
ggplot(pinguins, aes(x = comprimento_bico, y = profundidade_bico))

#agora podemos adicionar a pintura - plot 1
ggplot(pinguins,aes(x = comprimento_bico, y = profundidade_bico))+
  geom_point()

#agora podemos adicionar as cores! - plot 1
ggplot(pinguins,aes(x = comprimento_bico, y = profundidade_bico))+
  geom_point(aes(color = especie))

#salvar um gráfico
ggsave("grafico_pinguins.png", units = "cm", width = 12, height = 10, dpi = 300)

#boxplot!
ggplot(pinguins, aes(x = especie, y = profundidade_bico))+
  geom_boxplot(aes(fill = especie))

#e se quisermos adicionar mais camadas?

#boxplot com jitter!
ggplot(pinguins,aes(x = especie, y = profundidade_bico))+
  geom_boxplot(aes(fill = especie))+
  geom_jitter(width = 0.2) # a ordem faz diferença!

ggplot(pinguins,aes(x = especie, y = profundidade_bico))+
  geom_jitter(width = 0.2)+ # a ordem faz diferença!
  geom_boxplot(aes(fill = especie))

#geom_violin
ggplot(pinguins,aes(x = especie, y = profundidade_bico))+
  geom_violin(aes(fill = especie))


#podemos juntar as funções aprendidas com o ggplot
pinguins %>%
  filter(especie == "Pinguim-de-adélia") %>%
  ggplot(aes(x = comprimento_bico,
             y = profundidade_bico))+
  geom_point()+
  geom_smooth(method = "lm") #linear models

#vamos deixar ele bonito!
pinguins %>%
  filter(especie == "Pinguim-de-adélia") %>%
  ggplot(aes(x = comprimento_bico,
             y = profundidade_bico))+
  geom_point(color = "darkorange", size = 2, shape = "triangle")+
  geom_smooth(method = "lm", color = "darkorange")+ #linear models
  theme_minimal()+
  labs(title = "Pinguins!",
       x = "Comprimento do bico",
       y = "Profundidade do bico",
       shape = "Espécies",
       color = "Espécies")


#agora vamos ver um exemplo da importância de exibir os dados corretamente
ggplot(pinguins, aes(x = comprimento_bico, y = profundidade_bico))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()

#existe uma correlação negativa???


#e se colorirmos as espécies?
ggplot(pinguins, aes(x = comprimento_bico, y = profundidade_bico, color = especie))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()
#na verdade é positiva!!



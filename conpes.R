library(readxl)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

options(encoding = "UTF-8")

# Conpes ####
#Datos desde Github en csv
read.csv("https://raw.githubusercontent.com/nelsonamayad/conpes/master/conpes.csv", sep=";") %>%
#Clasificaciones
  mutate(fecha = as_date(fecha),
         class = case_when(str_detect(nombre, "importancia estra") ~ "Importancia Estrategica",
                           str_detect(nombre, "externo") ~ "Credito",
                           str_detect(nombre, "plan operativo") ~ "POAI",
                           str_detect(nombre, "mediano plazo") ~ "MGMP",
                           str_detect(nombre, "sistema general de par") ~ "SGP"),
         tipo = if_else(str_detect(num,"SOCIAL"),"Social","Economico"),
         n = str_extract(num, "\\d+") %>% as.numeric(),
         y = year(fecha),
         t = case_when(class=="Importancia Estrategica" ~ "Importancia Estrategica",
                       class!="Importancia Estrategica" ~ "Otros presupuestales*",
                       is.na(class) ~ "Otros"),
         ie = if_else(class=="Importancia Estrategica","Importancia Estrategica","Otros")) %>%
  mutate(ie = replace_na(ie,"Otros"),
         class = replace_na(class,"Otros")) %>%
  #Filtro desde 1999
  filter(y>1999) %>%
  #Agrupar para crear proporciones y sumas de Conpes por categoria
  group_by(y, t) %>%
  summarize(n=n()) %>%
  mutate(p = 100*n/sum(n)) %>%
  #La grafica
  ggplot(aes(x=y))+
  geom_col(aes(y=n,fill=t),width=1)+
  scale_x_continuous(breaks = c(1967:2019))+
  scale_fill_brewer(palette="Set1", direction = 1)+
  theme_minimal()+
  labs(y="Documentos Conpes",
       x=NULL,
       title="Ojalá recuperen el Conpes",
       subtitle="Los que no se le niegan a nadie: Declaratoria de importancia estrategica \npara obtener vigencias futuras exceptionales.",
       caption = "*MGMP, SGP, POAI y Creditos externos \nFuente: DNP, calculos propios")+
  theme(legend.position = "top", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size=7))

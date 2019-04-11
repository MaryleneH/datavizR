library(ggplot2)

ggplot(diamonds)+
  geom_point(aes(x = carat, y = price))+
  ggtitle(bquote(underline("Le titre")))


# Téléchargement des fichiers ISF
library(tidyverse)
library(readxl)

tf <- tempfile(fileext = ".xlsx")
f <- download.file("https://www.impots.gouv.fr/portail/files/media/stats/isfcom2017.xlsx",tf)
table_test <- read_excel(tf, sheet = 2, skip = 1) %>% 
  arrange(desc(`nombre de redevables`))

head(tf)

rm(list=ls())

require(tidyverse)
require(lubridate)

## Carga los registros desde el sistema de archivo
Mensajes2019 <- read_csv("./Data/2019/IndiceCorreos.csv", col_names=F)
colnames(Mensajes2019) <- c("subject","from","to","date","hasattachment","fullcontent")

## Limpia mensajes que no corresponden directamente a mensajes de la lista
sinIssues <- Mensajes2019[!str_detect(Mensajes2019$subject,"Re: \\[unloquer/AQA\\].*"),] %>% as_tibble

## Convierte a formato fecha la columna date
sinIssues$date <- dmy_hm(sinIssues$date)

## Busca correos que SON respuestas de un hilo y obtiene los índices de los registros
responseString <- "Re: \\[un/loquer\\] "
threadseed.idx <- which(is.na(sinIssues$subject %>% str_extract(responseString)))
## Busca correos que NO SON respuestas de un hilo y obtiene los índices de los registros
threads.idx <- !is.na(sinIssues$subject %>% str_extract(responseString))

## Correos respuesta y conforman un hilo sumarizados por cantidad de apariciones
uniqueThreads <- sinIssues$subject[threads.idx] %>% str_replace(responseString,"") %>% str_replace_all("\"","") %>% as_tibble %>% mutate(cnt = "") %>% group_by(value) %>% summarise(cnt = n())
colnames(uniqueThreads) <- c("subject","cnt")
uniqueThreads$flag <- "*"

uniqueThreads %>% View

## Correos semilla de un hilo, es posible que no tengan una respuesta, sumarizados por subject
threadseed <- sinIssues[threadseed.idx, "subject"] %>% mutate(cnt = "") %>% group_by(subject) %>% summarise(cnt = n()) %>% as_tibble
## Se unen sumatoria de correos respuesta de un hilo y las semillas
mailThreads <- threadseed %>% left_join(uniqueThreads, by="subject")

mailThreads %>% arrange(desc(cnt.y,cnt.x)) %>% View

## Obtener las fecha y sender del correo semilla y las respuestas del hilo
mailThreads %>% filter(cnt.x > 1) ## Seed de los que no se puede obtener la fecha direcatmente porque hay varios con el mismo nombre
mailThreads[-which(mailThreads$cnt.x > 1),]


sinIssues[threadseed.idx, c("subject","date","from")]

sinIssues[threads.idx,] %>% group_by(subject) %>% str_replace(responseString,"") %>% str_replace_all("\"","") %>% as_tibble



sinIssues$subject %>% str_replace(responseString,"") %>% as_tibble

sinIssues[is.na(sinIssues$subject %>% str_extract("Re: ")),c("from","subject","date")]

## Algunos cálculos con resúmenes de los datos
sinIssues %>% group_by(from) %>% summarise(cnt = n()) %>% View
sinIssues %>% group_by(to) %>% summarise(cnt = n()) %>% View

sinIssues %>% group_by(from, `año` = year(sinIssues$date), mes = month(sinIssues$date)) %>% summarise(cnt = n()) %>% arrange(`año`, mes) %>% View

sinIssues %>% group_by(from, subject) %>% summarise(cnt = n()) %>% View

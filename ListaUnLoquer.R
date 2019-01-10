rm(list=ls())

require(tidyverse)
require(lubridate)
##devtools::install_github("timelyportfolio/listviewer")

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
uniqueThreads <- sinIssues$subject[threads.idx] %>% str_replace(responseString,"") %>% str_replace_all("\"","") %>% as_tibble %>% mutate(cnt = 0) %>% group_by(value) %>% summarise(cnt = n())
colnames(uniqueThreads) <- c("subject","cnt")
uniqueThreads$flag <- "*"

uniqueThreads %>% View

## Correos semilla de un hilo, es posible que no tengan una respuesta, sumarizados por subject
threadseed <- sinIssues[threadseed.idx, "subject"] %>% mutate(cnt = 1) %>% group_by(subject) %>% summarise(cnt = n()) %>% as_tibble
## Se unen sumatoria de correos respuesta de un hilo y las semillas
mailThreads <- threadseed %>% left_join(uniqueThreads, by="subject")

mailThreads %>% arrange(desc(cnt.y,cnt.x)) %>% View

## Obtener las fecha y sender del correo semilla y las respuestas del hilo
mailThreads %>% filter(cnt.x > 1) ## Seed de los que no se puede obtener la fecha direcatmente porque hay varios con el mismo nombre
mailThreads[-which(mailThreads$cnt.x > 1),]


mailThreads$cnt.y[is.na(mailThreads$cnt.y)] <- 0

## Agrupa por subject todos los correos que correspondan a un hilo
subjectFromDate <- map(list(mailThreads$subject), function(s){
    sinIssues[sinIssues$subject %in% s, c("subject","from","date")]
})[[1]] %>% group_by(subject) %>% nest

## Une el conteneo de correos por hilo con la estructura que contiene todas las personas que participaron en un hilo y la fecha de participación
mailThreadsStr <- mailThreads %>% left_join(subjectFromDate, by = "subject") %>% mutate(cntMails = cnt.x + cnt.y) %>% select(subject, cntMails, flag, data)

## mailThreadsStr %>% listviewer::jsonedit(.)

## Extrae la persona y fecha de primera participación en un hilo y la persona y fecha de la última participación de la estructura unida anteriormente
## y la dispone de manera plana en el registro por subject/tema de correo
mailThreadsPeopleTime <- mailThreadsStr %>% mutate(originalSender = map(data, function(mt){
    lng <- length(mt$from)
    tibble(origFrom = mt$from[1], origDate = mt$date[1], finalFrom = mt$from[lng],finalDate = mt$date[lng])
} )) %>% select(subject, cntMails, flag, originalSender) %>% unnest


mailThreadsPeopleTime %>% inner_join(mailThreadsStr %>% select(subject,data), by = "subject") %>% jsonlite::write_json(.,"/tmp/lista.json")


str(mailThreadsStr$data[[3]], max.level = 1)

str(mailThreadsStr$data[[2]])
mailThreadsStr$data[[2]]$date[1]

## ¿Cómo exportar el archivo json sin bloquear el emacs?

sinIssues[threadseed.idx, c("subject","date","from")]

sinIssues[threads.idx,] %>% group_by(subject) %>% str_replace(responseString,"") %>% str_replace_all("\"","") %>% as_tibble

sinIssues$subject %>% str_replace(responseString,"") %>% as_tibble

sinIssues[is.na(sinIssues$subject %>% str_extract("Re: ")),c("from","subject","date")]



## Algunos cálculos con resúmenes de los datos
sinIssues %>% group_by(from) %>% summarise(cnt = n()) %>% View
sinIssues %>% group_by(to) %>% summarise(cnt = n()) %>% View

sinIssues %>% group_by(from, `año` = year(sinIssues$date), mes = month(sinIssues$date)) %>% summarise(cnt = n()) %>% arrange(`año`, mes) %>% View

sinIssues %>% group_by(from, subject) %>% summarise(cnt = n()) %>% View

Mensajes2019[grep("aíre",Mensajes2019$subject),"subject"] %>% View

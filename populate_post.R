# Comentado. No funciona el outh para poder usar la librería.
# library(tuber)
# app_id <- "847239031074-oo4ju6taji75g8umni3evgp52j4or9r0.apps.googleusercontent.com"
# app_secret <- "3mxLgGGzgbt4Mq7DVLT7UBrV"
# yt_oauth(app_id, app_secret, scope = 'basic', token = '')
# get_stats(video_id = 'HZ3oLNHy9qA', "&key=AIzaSyCr5sWtGYCICJYXRovRmPuws6b1U2wYZHM")

# Probamos directamente con la dirección de la API
# Devuelve un json con los datos del vídeo
# Recogemos los datos que nos interesa y creamos el fichero md
# que colocaremos en el directorio del blog Hugo


library(jsonlite)
library(magick)
library(dplyr)
library(stringr)
library(readxl)
library(purrr)

yt1 <- read_excel("C:/Users/arama/Desktop/170608 ASEPUMA A Coruña/Youtube/Patri_def.xlsx")
yt1 <- yt1[,1:16]

yt2 <- read_excel("C:/Users/arama/Desktop/170608 ASEPUMA A Coruña/Youtube/Dani_def.xlsx")
yt2 <- yt2[,1:16]

yt_todas <- bind_rows(list(yt1,yt2))

names(yt_todas) <- c("id","nombre","provincia","ccaa","anyo","tipo","acrom","enlace","titulo","canal","suscriptores","visualizacion","publicacion","duracion","like","dislikes")

yt <- yt_todas %>%
  filter(!is.na(enlace)) %>%
  mutate(d_segundos = duracion - lubridate::ymd_hms("1899-12-30 00:00:00"))

set.seed(1234)
yt_filtered <- yt %>% slice(sample(nrow(.),5)) %>% select(acrom, enlace)

write_post <- function(acrom,enlace){

id_video = str_remove(enlace, 'https://www.youtube.com/watch\\?v=') %>%
  str_remove('&.*')

print(id_video)

url <- paste0('https://www.googleapis.com/youtube/v3/videos?part=snippet&id=',
              id_video,'&key=AIzaSyCuz3FsXNjODEWo68hBSgz-LIaGGBPmyRE')



datos <- fromJSON(url)
title <- datos$items$snippet$title
print(title)
fecha <- datos$items$snippet$publishedAt
description <- datos$items$snippet$description
nombre <- datos$items$snippet$channelTitle
tags <- datos$items$snippet$tags[[1]]
universidad <- acrom
img <- datos$items$snippet$thumbnails$maxres$url
if (is.null(img)) img <- datos$items$snippet$thumbnails$standard$url
if (is.null(img)) img <- datos$items$snippet$thumbnails$default$url

print(img)
temp <- tempfile()
download.file(img, temp, mode = "wb")
imFile <- image_read(temp)
resized <- image_scale(imFile,'1000x750!' )
path <- 'static/img/banners/'
# name_file <- paste0(paste(c('banner',
#                    Filter(function (x) nchar(x)>3,
#                           title %>% str_split(' ') %>% `[[`(1)) %>% substr(1,4)),
#                    collapse = '-'),'.jpg')

name_file <- title %>% str_split(',', simplify = T) %>% `[[`(1) %>%
  str_remove_all('[¿?¡!:]') %>% str_replace_all('á','a') %>%
  str_replace_all('é','e') %>% str_replace_all('í','i') %>%
  str_replace_all('ó','o') %>% str_replace_all('ú','u') %>%
  str_replace_all('ñ','n') %>% str_replace_all(' - ', ' ') %>%
  str_replace_all(' ','-') %>% str_to_lower() %>% str_c('.jpg')

print(name_file)

if (name_file %in% list.files(path)) {
  if (str_detect(name_file,'-\\d.jpg')) {
    num <- str_extract(name_file,'-\\d.jpg') %>% str_extract('\\d') %>% as.integer()
    name_file <- str_replace(name_file,'-\\d.jpg',str_c('-',num + 1,'.jpg'))
    } else {
    name_file <- str_replace(name_file,'.jpg','-1.jpg')
    }
  }


image_write(resized,paste0(path,name_file))


text <- paste0("---\nbanner: ",
               paste0(str_replace(path,'static/',''),name_file)
               ,"\ncategories:\n- ",
universidad,"\ndate: ",
fecha,"\ntags:\n- ",
paste(tags,collapse = '\n- '),
"\ntitle: '",title,
"'\n---\n\n",
description,
str_c("\n\n{{< youtube ",id_video," >}}"))
writeLines(text,
           str_c('content/blog/',str_replace(name_file,'.jpg','.md')),
           useBytes = T)
return(NULL)
}

yt_list <- as.list(yt_filtered)

pmap(yt_list, ~ write_post(.x,.y))



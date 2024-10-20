library("rvest")
library("tidyverse")
library("openxlsx")
library(dplyr)

web_site = "https://www.worldsurfleague.com/athletes/tour/mct?year=2024"

first_scrap <- read_html(web_site)

first_scrap %>%
  #html_text()  
  #html_nodes(".tableType-athlete")  %>% 
  html_text()

first_scrap

table_scores<-first_scrap %>%
  #html_text()  
  html_nodes(".tableType-athlete")  %>% 
  html_table()

class(table_scores)
table_scores
#trees

write.xlsx(x = table_scores, file = "./table_scores.xlsx")
x <- table(table_scores,
            header = TRUE,
           sep = "\n",
           dec = ",")
df_score <- read.table("./Scrap_WSL/table_scores.csv", header=T, sep=",",fill=T)
#class(df_score)
#df_score[3]
getwd()

df_score
colnames(df_score)[3]<-"eliminar"
colnames(df_score)[5]
borrar <- c("eliminar","WSL.Finals")
datos2 <- df_score[ , !(names(df_score) %in% borrar)]
head(datos2, n=9)

colnames(datos2)[4]<-"lexus pip pro"
colnames(datos2)[5]<-"hurley pro sunset beach"
colnames(datos2)[6]<-"meo pro portugal"
colnames(datos2)[7]<-"pro bells beach"
colnames(datos2)[8]<-"western australia margaret river pro"
colnames(datos2)[9]<-"shiseido tahiti pro"
colnames(datos2)[10]<-"surf city el salvador pro"
colnames(datos2)[11]<-"rio pro"
colnames(datos2)[12]<-"fiji pro"
colnames(datos2)
datos2

filter(datos2,str_detect(Name, "South Africa"))["Name"]

filter(datos2,Name %in% ("Leonardo FioravantiItaly"))
datos2 %>% filter(str_detect(Name, "Hawaii"))

datos2 <- datos2 %>% mutate(
  country = case_when(
    str_detect(Name, "Hawaii") ~ "Hawaii",
    str_detect(Name, "Italy") ~ "Italy",
    str_detect(Name, "Brazil") ~ "Brazil",
    str_detect(Name, "United States") ~ "United States",
    str_detect(Name, "South Africa") ~ "South Africa",
    str_detect(Name, "Australia") ~ "Australia",
    str_detect(Name, "Morocco") ~ "Morocco",
    str_detect(Name, "Japan") ~ "Japan",
    str_detect(Name, "Indonesia") ~ "Indonesia",
    str_detect(Name, "Portugal") ~ "Portugal",
  )
)

datos2$Name<-gsub( "Hawaii", "", datos2$Name)
datos2$Name<-gsub( "Italy", "", datos2$Name)
datos2$Name<-gsub( "Brazil", "", datos2$Name)
datos2$Name<-gsub( "United States", "", datos2$Name)
datos2$Name<-gsub( "South Africa", "", datos2$Name)
datos2$Name<-gsub( "Australia", "", datos2$Name)
datos2$Name<-gsub( "Morocco", "", datos2$Name)
datos2$Name<-gsub( "Japan", "", datos2$Name)
datos2$Name<-gsub( "Indonesia", "", datos2$Name)
datos2$Name<-gsub( "Portugal", "", datos2$Name)
datos2$Name

table(datos2$country)


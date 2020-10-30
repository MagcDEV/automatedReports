# functions ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(knitr)

data_dowload <- function() {
  url_mbm <- "https://docs.google.com/uc?id=1r3onZkqRzxkPlnWfMnKSjAmKbIAH_Eke&export=download"
  download.file(url_mbm, destfile = "/home/manuel/Documentos/automatedReports/data/mbmData.xls")
}

data_read_clean <- function() {
  mbmDataAereo <- readxl::read_xls("/home/manuel/Documentos/automatedReports/data/mbmData.xls", 
                                   sheet = "AEREO MISCELANEOS", skip = 6)
  mbmDataMaritimo <- readxl::read_xls("/home/manuel/Documentos/automatedReports/data/mbmData.xls", 
                                     sheet = "MARITIMO MISCELANEOS ", skip = 6) 
  mbmDataMaritimo  <- mbmDataMaritimo[,1:12] 
  mbmDataAereo$TIPO  <- "AEREO"
  mbmDataMaritimo$TIPO  <- "MARITIMO"
  mbmDataAereo  <- clean_names(mbmDataAereo)
  mbmDataMaritimo <- clean_names(mbmDataMaritimo)
  mbmDataAereo$gross_weight_ft <- 0
  mbmDataMaritimo$gross_weight_lb <- 0
  mbmData  <- rbind(mbmDataAereo, mbmDataMaritimo)
  rm(mbmDataAereo, mbmDataMaritimo)
    
  mbmData <- mbmData[!is.na(mbmData$mes) & !(mbmData$mes == "44068"),]
  
  mbmData$ganancias_mbm[is.na(mbmData$ganancias_mbm)] <- 0
  mbmData$gross_weight_lb[is.na(mbmData$gross_weight_lb)] <- 0
  mbmData$gross_weight_ft[is.na(mbmData$gross_weight_ft)] <- 0
  
  mbmData$cliente <- stringr::str_to_title(stringr::str_squish(tolower(mbmData$cliente))
)
  mbmData
}

#data_dowload()

resumen_data <- data_read_clean() %>%
  mutate(new_fecha = lubridate::as_date(paste(lubridate::year(fecha),
                           lubridate::month(fecha), 
                           "01", 
                           sep = "-"))) %>% 
  arrange(new_fecha) %>%
  group_by(new_fecha) %>% 
  summarise(wh = n(), ganancias_mbm = sum(ganancias_mbm),
            libras = sum(gross_weight_lb), 
            feet = sum(gross_weight_ft))
names(resumen_data) <- c("Mes", "Warehouse", "Ganancias",
                         "Libras", "Feet")
resumen_data$Mes <- stringr::str_to_title(paste(months(resumen_data$Mes), 
                            lubridate::year(resumen_data$Mes), sep = "-"))

resumen_data$Ganancias <- paste("$",format(resumen_data$Ganancias,
                                 big.mark=",", nsmall = 2,digits=2), sep=" "
)
resumen_data_armar <- data_read_clean() %>%
  arrange(fecha) %>%
  mutate(new_fecha = lubridate::as_date(paste(lubridate::year(fecha), 
                           lubridate::month(fecha),
                           "01",
                           sep = "-"))) %>%
  group_by(new_fecha, tipo) %>%
  summarise(wh = n(), ganancias_mbm = sum(ganancias_mbm),
            libras = sum(gross_weight_lb),
            feet = sum(gross_weight_ft))
names(resumen_data_armar) <- c("Mes", "Envio", "Wh",
                               "Ganancias","Lb", "Ft")
resumen_data_armar$Mes <- paste(stringr::str_to_title(stringr::str_sub(months(resumen_data_armar$Mes), 1, 3)), 
                            lubridate::year(resumen_data_armar$Mes), sep = "-")
resumen_data_armar$Ganancias <- paste("$", format(
resumen_data_armar$Ganancias, digits = 2, nsmall = 2, big.mark = ","),
                                          sep = " ")

resumen_data_cliente <- data_read_clean() %>% group_by(cliente) %>% 
  summarise(wh = n(), ganancias = sum(ganancias_mbm),
            libras = sum(gross_weight_lb),
            feet = sum(gross_weight_ft)) %>%
  arrange(desc(ganancias))
resumen_data_cliente$ganancias <- paste("$",format( 
                                        resumen_data_cliente$ganancias,
                                        digits=2, nsmall=2, 
                                        big.mark=","), sep=" ")

resumen_data_men <- data_read_clean() %>%
  arrange(fecha) %>%
  mutate(new_fecha = lubridate::as_date(paste(lubridate::year(fecha), 
                           lubridate::month(fecha),
                           "01",
                           sep = "-"))) %>% 
  filter(lubridate::month(fecha) == lubridate::month(Sys.Date())) %>% 
  group_by(new_fecha) %>% 
  summarise(wh = n(), ganancias_mbm = sum(ganancias_mbm),
            libras = sum(gross_weight_lb),
            feet = sum(gross_weight_ft))
names(resumen_data_men) <- c("Mes", "Wh", "Gananias", "Lb", "Feet")
  
resumen_data_armar_men <- data_read_clean() %>%
  arrange(fecha) %>%
  mutate(new_fecha = paste(lubridate::year(fecha), 
                           lubridate::month(fecha), 
                           sep = "-")) %>% 
  filter(lubridate::month(fecha) == lubridate::month(Sys.Date())) %>% 
  group_by(new_fecha, tipo) %>% 
  summarise(wh = n(), ganancias_mbm = sum(ganancias_mbm), 
            libras = sum(gross_weight_lb), 
            feet = sum(gross_weight_ft)) %>%
            ungroup() %>%
            select(tipo, wh, ganancias_mbm, libras, feet)
names(resumen_data_armar_men) <- c("Envio", "Wh", "Ganancias", "Lb", "Ft")
resumen_data_armar_men$Ganancias <- paste("$", format(
resumen_data_armar_men$Ganancias, digits = 2, nsmall = 2, big.mark = ","),
                                          sep = " ")

resumen_data_cliente_men <- data_read_clean() %>% 
  filter(lubridate::month(fecha) == lubridate::month(Sys.Date())) %>% 
  group_by(cliente) %>% 
  summarise(wh = n(), ganancias = sum(ganancias_mbm),
            libras = sum(gross_weight_lb), 
            feet = sum(gross_weight_ft)) %>% 
  arrange(desc(ganancias))
resumen_data_cliente_men$ganancias <- paste("$",format( 
                                        resumen_data_cliente_men$ganancias,
                                        digits=2, nsmall=2, 
                                        big.mark=","), sep=" ")


#' @import ggplot2
#' @import htmlwidgets
#' @import tidyverse
#' @import lubridate
#' @import grid
#' @import data.table
#' @import DescTools
#' @import plyr
#' @import rgdal
#' @import ggthemes
#' @import raster
#' @import rgeos
#' @import mapview
#' @import sf
#' @import svMisc
#' @import leaflet.extras
#' @import leaflet.providers
#' @import htmltools
#' @import leaflet
#' @import grid
#' @import gridExtra
#' @export
#' @title Isolation Map in Brazil
#'
#' @description Build a map for each state in Brazil with the isolation index during the COVID-19 pandemic containing a plot for each city with the index and its variation when compared to February 2020, the mena isolation during the pandemic and the isolation one week earlier.
#'
#' @param end_quar The last day with data about the isolation index. Should be in the format yyy-mm-dd.
#'
#' @return Save all maps and plots in the working directory.
#'
#' @example
#' #Run for data until 2020-04-26
#' isolation_map(end_quar = "2020-04-26) #Take very long time!!

isolation_map <- function(end_quar = "2020-04-26"){

  q <- readline("This will erase all content in /home/diego/mdyn/plots, are you sure you wnat to continue (y/n)?")
  if(!(q %in% c("y","Y","yes","Yes","YES","sim","SIM","s","S"))){
    cat("Sorry, cannot help you then.\n")
    return(0)
  }

  #Set wd
  setwd("/home/diego/mdyn")

  #parameters
  dic_estados = list('AC' = 'Acre','AL' = 'Alagoas','AP' = 'Amapá','AM' = 'Amazonas',
                     'BA' = 'Bahia','CE' = 'Ceará','DF' = 'Distrito Federal',
                     'ES' = 'Espírito Santo','GO' = 'Goiás','MA' = 'Maranhão',
                     'MT' = 'Mato Grosso','MS' = 'Mato Grosso do Sul',
                     'MG' = 'Minas Gerais','PA' = 'Pará','PB' = 'Paraíba','PR' = 'Paraná',
                     'PE' = 'Pernambuco','PI' = 'Piauí','RJ' = 'Rio de Janeiro',
                     'RN' = 'Rio Grande do Norte','RS' = 'Rio Grande do Sul','RO' = 'Rondônia',
                     'RR' = 'Roraima','SC' = 'Santa Catarina','SP' = 'São Paulo',
                     'SE' = 'Sergipe','TO' = 'Tocantins')
  dic_shp = list("AC" = "12MUE250GC_SIR","AL" = "27MUE250GC_SIR","AP" = "16MUE250GC_SIR",
                 "AM" = "13MUE250GC_SIR","BA" = "29MUE250GC_SIR",
                 "CE" = "23MUE250GC_SIR","DF" = "53MUE250GC_SIR","ES" = "32MUE250GC_SIR",
                 "GO" = "52MUE250GC_SIR","MA" = "21MUE250GC_SIR","MT" = "51MUE250GC_SIR",
                 "MS" = "50MUE250GC_SIR","MG" = "31MUE250GC_SIR","PA" = "15MUE250GC_SIR",
                 "PB" = "25MUE250GC_SIR","PR" = "41MUE250GC_SIR","PE" = "26MUE250GC_SIR",
                 "PI" = "22MUE250GC_SIR","RJ" = "33MUE250GC_SIR","RN" = "24MUE250GC_SIR",
                 "RS" = "43MUE250GC_SIR","RO" = "11MUE250GC_SIR","RR" = "14MUE250GC_SIR",
                 "SC" = "42MUE250GC_SIR","SP" = "35MUE250GC_SIR","SE" = "28MUE250GC_SIR",
                 "TO" = "17MUE250GC_SIR")
  ini_quar = "2020-03-15"
  init_padrao_pan = "2020-03-29"
  ini_padrao = "2020-02-01"
  end_padrao = "2020-02-29"

  #Calculated dates intervals
  dias_quar <- seq.Date(from = ymd(ini_quar),to = ymd(end_quar),by = 1)
  dias_padrao_pan <- seq.Date(from = ymd(init_padrao_pan),to = ymd(end_quar),by = 1)
  dias_padrao <- seq.Date(from = ymd(ini_padrao),to = ymd(end_padrao),by = 1)

  #States
  estados <- names(dic_estados)

  #Color palletes
  rc_cont <- colorRampPalette(colors = c("red","darkgoldenrod1","green"), space = "Lab")(200)
  rc_2 <- colorRampPalette(colors = c("red","green"), space = "Lab")(3)

  #Get IME logo
  logo <- get_png("./logos/IME_simplificado.png")

  #Files
  files <- paste("/home/pedrosp/mdyn/inloco/",estados,"_Municipios_",end_quar,"_iso_index.csv",sep = "")
  names(files) <- estados

  #Organizing data and saving in rds files
  system("rm -r ./dataR")
  dir.create(path = "./dataR")
  cat("Organizing data and saving in rds files...")
  cat("\n")
  for(s in estados){
    cat("State: ")
    cat(s)
    dados <- data.frame(read.csv(files[[s]]))
    saveRDS(object = dados,file = paste("./dataR/original_",acento(s),".rds",sep = ""))
    dados <- dados %>% dplyr::select("reg_name","iso","day")
    dados$day <- ymd(dados$day)
    dados$weekday <- weekdays(dados$day)
    dados$key <- paste(dados$reg_name,dados$weekday)

    #Padrao pre-pandemia
    padrao_pre <- data.table(dados %>% filter(day %in% dias_padrao))
    padrao_pre <- padrao_pre[,mean_pre := mean_trim(iso),by = key]
    padrao_pre <- padrao_pre %>% dplyr::select("key","mean_pre") %>% unique()

    #Padrao durante pandemia
    padrao_pan <- data.table(dados %>% filter(day %in% dias_quar))
    padrao_pan <- padrao_pan[,mean_pan := mean_pan(iso,day),by = key]
    padrao_pan <- data.frame(padrao_pan[,last_week := last_pan(iso,day),by = key])
    padrao_pan$mean_pan[!(padrao_pan$day %in% dias_padrao_pan)] <- NA
    padrao_pan$last_week[!(padrao_pan$day %in% dias_padrao_pan)] <- NA
    padrao_pan$key <- paste(padrao_pan$reg_name,padrao_pan$day)
    padrao_pan <- padrao_pan %>% dplyr::select("key","mean_pan","last_week") %>% unique()

    #Dias de quarentena
    dados <- dados %>% filter(day %in% dias_quar)

    #Juntando e calculando indice
    dados <- merge(dados,padrao_pre)
    dados$key <- paste(dados$reg_name,dados$day)
    dados <- merge(dados,padrao_pan)
    dados$weekday <- factor(dados$weekday)
    dados$key <- NULL

    #Calculando Indices
    dados$indice_pre <- indice_relativo(iso = dados$iso,media = dados$mean_pre)
    dados$indice_pan <- indice_relativo(iso = dados$iso,media = dados$mean_pan)
    dados$indice_week <- indice_relativo(iso = dados$iso,media = dados$last_week)

    #Salvando
    saveRDS(object = dados,file = paste("./dataR/",s,".rds",sep = ""))

    #Apagando
    rm(padrao_pan,padrao_pre,dados)
    cat(" Ok!")
    cat("\n")
  }
  cat("OK!")
  cat("\n")

  #Bulding maps
  cat("Building maps and saving in html...")
  cat("\n")
  system("rm -r ./html")
  system("cp -r ./logos/ ./html/logos")
  dir.create("./html")

  #Reading data and shapefile
  dadosBR <- data.frame()
  for(s in estados){
    dados <- readRDS(file = paste("./dataR/",s,".rds",sep = ""))
    dados$UF <- s
    if(nrow(dadosBR) == 0)
      dadosBR <- dados
    else
      dadosBR <- rbind.data.frame(dadosBR,dados)
  }
  dadosBR$key <- factor(paste(dadosBR$reg_name,dadosBR$UF))
  dadosBR <- dadosBR %>% filter(day == "2020-03-24") %>% unique()
  manter <- names(table(dadosBR$key))[table(dadosBR$key) == 1]
  dadosBR <- dadosBR %>% filter(key %in% manter)
  shp <- readOGR("/home/pedrosp/mdyn/maps/br_municipios/br_mun_with_uf.shp",verbose = F)
  shp$Nome_UF <- factor(shp$Nome_UF)
  shp$UF <- mapvalues(x = shp$Nome_UF,from = unlist(dic_estados),to = names(dic_estados))
  shp$key <- paste(shp$NM_MUNICIP,shp$UF)
  tmp <- merge(shp,dadosBR,by = "key",all.x = F,all.y = F)

  #Map layout
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }"))

  ime <- tags$div(
    HTML('<a href="https://www.ime.usp.br/"> <img border="0" alt="ImageTitle" src="./logos/IME_simplificado.jpg" width="60" height="100"> </a>')
  )
  usp <- tags$div(
    HTML('<a href="https://www.usp.br/"> <img border="0" alt="ImageTitle" src="./logos/logo_USP.jpeg" width="60" height="35"> </a>')
  )
  fapesp <- tags$div(
    HTML('<a href="http://www.fapesp.br/en/"> <img border="0" alt="ImageTitle" src="./logos/FAPESP.png" width="60" height="35"> </a>')
  )

  voltar <- tags$div(
    HTML('<a href="https://www.ime.usp.br/~pedrosp/covid19/#iso_index"> <img border="0" alt="ImageTitle" src="./logos/voltar.png" width="60" height="35"> </a>')
  )

  obs <- tags$div(
    HTML('<a href="https://www.ime.usp.br/~pedrosp/covid19/#iso_index"> <img border="0" alt="ImageTitle" src="./logos/Obs.png" width="500" height="50"> </a>')
  )

  mypal <- colorNumeric(palette = rc_cont,domain = 100*tmp$iso)

  #Build maps
  for(s in estados){
    cat(paste("State:",s))
    tmpS <- tmp[tmp$UF.x == s,]
    title <- tags$div(tag.map.title, HTML(paste("Isolamento Social Comparativo IME - USP <br>",dic_estados[[s]])))
    mapa <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,options = providerTileOptions(minZoom = 6)) %>%
      addTiles(urlTemplate = "", attribution = "©IME - USP. Design: Diego Marcondes.") %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addControl(title, position = "topleft", className="map-title") %>%
      addControl(ime, position = "bottomleft") %>%
      addControl(usp, position = "bottomleft") %>%
      addControl(fapesp, position = "bottomleft") %>%
      addControl(voltar, position = "topleft") %>%
      addControl(obs, position = "bottomright") %>%
      addPolygons(data = tmpS,fillColor = mypal(100*tmp$iso),color = "grey",
                  popup = paste('<img src = ./plots/isol_',acento(gsub(pattern = " ",replacement = "",
                                                                       x = tmpS$NM_MUNICIP)),'_',tmpS$UF.x,
                                '.png width="750" height="500"/>',sep = ""),
                  options = popupOptions(opacity = 0,closeButton = FALSE),
                  opacity = 0.5,fillOpacity = 0.5,label = paste(tmpS$NM_MUNICIP,'-',tmpS$UF.x,"\n",
                                                                round(100*tmp$iso),"%"),
                  labelOptions = labelOptions(textsize = "15px")) %>%
      addPolylines(data = shp[shp$UF == s,], color = "black", opacity = 1, weight = 1) %>%
      addLegend(position = "bottomright", pal = mypal, values = 100*tmp$iso,na.label = "Sem dados",
                title = paste("Índice de Isolamento social (%) <br> em ",format.Date(end_quar, "%d"),"/",
                              format.Date(end_quar, "%m"),"/2020",sep = ""),opacity = 0.8)

    #Save
    saveWidget(mapa, file = paste("/home/diego/mdyn/html/mapa_",s,".html",sep = ""))
    cat(" Ok!")
    cat("\n")
  }
  cat("Ok!")
  cat("\n")

  #Bulding plots
  cat("Building plots and saving in pdf...")
  cat("\n")
  system("rm -r ./html/plots")
  dir.create("./html/plots")

  for(s in estados){
    cat("State: ")
    cat(s)
    cat("...")

    #Lendo os dados
    dados <- readRDS(file = paste("./dataR/",s,".rds",sep = ""))
    dados$iso <- 100*dados$iso
    dados <- dados %>% gather("indice","value",-reg_name,-day,-weekday,-mean_pre,-mean_pan,-last_week)
    dados$indice <- factor(dados$indice,c("iso","indice_pre","indice_week","indice_pan"))
    dados$cor <- NA
    dados$cor[dados$value < 0] <- "red"
    dados$cor[dados$value >= 0] <- "green"
    dados$cor[dados$indice == "iso"] <- "white"
    dados$cor <- factor(dados$cor)
    dados$indice[dados$indice == "indice_pan" & dados$day < max(dados$day) - 7] <- NA

    breaks_fun <- function(x) {
      if(min(x) > as.numeric(max(dados$day)) - 8)
        seq.Date(from = ymd(ini_quar),to = ymd(end_quar),by = 1)
      else if(min(x) > ymd("2020-03-25"))
        seq.Date(from = ymd(ini_quar),to = ymd(end_quar),by = 4)
      else
        seq.Date(from = ymd(ini_quar),to = ymd(end_quar),by = 5)
    }

    for(c in unique(dados$reg_name)){
      tmp <- dados %>% filter(day %in% dias_quar & reg_name == c) %>% dplyr::select(day,value,cor,indice) %>% na.omit()
      if(min(tmp$value[tmp$indice == "indice_pre"]) > 0)
        dline <- data.frame("y" = c(0,0),"indice" = c("indice_week","indice_pan"))
      else
        dline <- data.frame("y" = c(0,0,0),"indice" = c("indice_week","indice_pan","indice_pre"))
      tmp$color_line <- NA
      tmp$color_line[tmp$indice == "iso"] <- "white"
      for(d in (min(tmp$day)):(max(tmp$day)-1)){
        for(id in c("indice_week","indice_pan","indice_pre")){
          if(length(tmp$color_line[tmp$indice == id & tmp$day == d]) > 0 & length(as.character(tmp$cor[tmp$indice == id & tmp$day == d+1])) > 0)
            tmp$color_line[tmp$indice == id & tmp$day == d] <- as.character(tmp$cor[tmp$indice == id & tmp$day == d+1])
        }
      }
      tmp$color_line[is.na(tmp$color_line)] <- "white"
      tmp$color_line <- factor(tmp$color_line)

      p <- ggplot(tmp,aes(x = day,y = value,colour = cor)) + theme_solarized(light = FALSE) +
        xlab("Data") + facet_wrap(indice~.,scales = "free",
                                  labeller = as_labeller(c(iso = "Índice de Isolamento Social (%)",
                                                           indice_week = "Variação em relação a semana anterior (%)",
                                                           indice_pan = "Variação em relação ao padrão durante pandemia (%)",
                                                           indice_pre = "Variação em relação ao padrão de Fev/20 (%)"))) +
        ylab(NULL) +
        theme(strip.background = element_blank(),
              strip.text = element_text(size = 15,face = "bold",color = "white")) +
        geom_point() + geom_line(aes(colour = color_line,group = 1)) +
        scale_colour_manual(values = c("green","red","white")) +
        scale_x_date(breaks = breaks_fun) +
        theme(legend.title = element_text(face = "bold"),legend.position = "none") +
        theme(plot.title = element_text(face = "bold",size = 20,color = "white"),
              axis.text.x = element_text(size = 10,face = "bold",color = "white"),
              axis.text.y = element_text(size = 10,face = "bold",color = "white"),
              legend.box.margin = unit(x=c(20,0,0,0),units="mm"),
              legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title = element_text(color = "white",size = 20),
              plot.caption = element_text(face = "bold",color = "white",hjust = 0)) +
        theme(plot.margin = unit(c(1,1,1,1), "lines")) +
        scale_y_continuous(breaks = seq(-500,500,10)) +
        ggtitle(paste("Isolamento Social Comparativo IME - USP\n",c," - ",s,sep = "")) +
        labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
        geom_hline(data = dline,aes(yintercept = y),
                   color = "white",linetype = "dashed")

      pdf(file = paste("./html/plots/isol_",
                       acento(gsub("/","",gsub(pattern = " ",replacement = "",x = c))),
                       "_",s,".pdf",sep = ""),width = 1.2*15,
          height = 1.2*10)
      print(p)
      dev.off()
    }
    cat(" Ok!")
    cat("\n")
  }
  cat("Ok!")
  cat("\n")

  #Convert plots to png
  cat("Converting plots to png...")
  cat("\n")
  system("cd ./html/plots/ && mogrify -verbose -density 200 -format png ./*.pdf")
  cat("OK!")
  cat("\n")
  cat("The end!!")
}



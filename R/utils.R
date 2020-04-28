#Get PNG
get_png <- function(filename) {
  grid::rasterGrob(image = png::readPNG(filename))
}

#Calcula os indices
indice <- function(iso,media,desvio){
  v <- media + 2*desvio
  a <- ifelse(v >= 1,NA,as.numeric(iso > v)*(iso - v)/(1-v))
  return(a)
}

indice_relativo <- function(iso,media){
  a <- 100*(iso/media - 1)
  return(a)
}


#Mean e sd trimming min and max
mean_trim <- function(x){
  x <- x[!(x %in% c(min(x,na.rm = T),max(x,na.rm = T)))]
  return(mean(x = x,na.rm = T))
}

mean_pan <- function(iso,day){
  iso <- iso[order(day,decreasing = F)]
  d <- day[order(day,decreasing = F)]
  m <- unlist(imap(1:length(iso),~ mean(iso[1:(.x - 1)])))
  names(m) <- d
  m <- m[match(x = as.character(names(m)),table = as.character(day))]
  return(m)
}

# sd_pan <- function(iso,day){
#   iso <- iso[order(day,decreasing = F)]
#   m <- unlist(imap(1:length(iso),~ sd(iso[1:(.x - 1)])))
#   return(m)
# }

last_pan <- function(iso,day){
  iso <- iso[order(day,decreasing = F)]
  d <- day[order(day,decreasing = F)]
  m <- c(NA,unlist(imap(1:length(iso),~ iso[.x - 1])))
  names(m) <- d
  m <- m[match(x = as.character(names(m)),table = as.character(day))]
  return(m)
}

#Generate map
generate_map <- function(d,dados,shp,s){
  dados_dia <- dados %>% filter(day == d) %>% select(reg_name,indice)
  names(dados_dia)[1] <- "id"
  dados_mapa <- inner_join(x = shp,y = dados_dia,by = c("id","id"))
  p <- ggplot(dados_mapa,aes(x=long, y = lat, group = group,fill = indice)) +
    geom_polygon(color = "black") +
    theme_bw() + ylab("") + xlab("Índice de Isolamento Social") +
    scale_fill_gradientn("",colours = rc,na.value = "transparent",
                         limits = c(0,max(dados$indice,na.rm = T)),
                         breaks = c(min(dados$indice,na.rm = T),max(dados$indice,na.rm = T)),
                         labels = c("Dentro do Normal","Todos em Isolamento")) +
    ggtitle(paste("Índice de Isolamento Social\n",s,"-",format.Date(d,"%d/%m/%Y"),
                  "\nPara mais informações acesse www.ime.usp.br/~pedrosp/covid19/",
                  sep = "")) +
    theme(legend.title = element_text(face = "bold"),legend.position = "bottom") +
    theme(plot.title = element_text(face = "bold"),
          legend.text = element_text(face = "bold",size = 15,color = c("red","green")),
          legend.box.margin = unit(x=c(0,0,0,0),units="mm"),
          legend.key.width=unit(3.5,"cm"),
          axis.title = element_text(face = "bold",size = 20))
  p
  dados_dia$d <- format.Date(d,"%d/%m/%Y")
  names(dados_dia) <- c("Cidade","Índice de Isolamento Social","Dia")
  return(list("p" = p,"tab" = dados_dia))
}

gerar_mapas <- function(dados,shp,s){
  return(function(d) generate_map(d = d,dados = dados,shp = shp,s = s))
}

#Tirar acento
acento <- function(x) iconv(x, to = "ASCII//TRANSLIT")

#Test if date is valid
is.valid.date <- function(date){
  month_30 <- c(4,6,9,11)
  year <- as.numeric(substr(x = date,start = 1,stop = 4))
  month <- as.numeric(substr(x = date,start = 6,stop = 7))
  day <- as.numeric(substr(x = date,start = 9,stop = 10))
  if(year < 2020 || month == 0 || month > 12 || day < 0 || day > 31)
    return(FALSE)
  else if(month %in% month_30 && day == 31)
    return(FALSE)
  else if(month == 2 && day > 29)
    return(FALSE)
  else if(month == 2 && day == 29 && !is.leap(year))
    return(FALSE)
  else
    return(TRUE)
}

#Test if a year is leap year
is.leap <- function(year){
  return(year%%4 == 0)
}

#Positive log
mod_log <- function(x){
  return(ifelse(x >= 1,log(x),0))
}


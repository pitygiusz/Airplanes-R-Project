#PD4 - Wpływ ataków z 11 września 2001 roku na lotnictwo.
#Ewa Rożek, Piotr Jurczyk


####import danych####
lot1999 <- read.csv("1999.csv.bz2")
lot1999['Id'] <- seq(1,length(lot1999$ArrDelay))
lot2000 <- read.csv("2000.csv.bz2")
lot2000['Id'] <- seq(1,length(lot2000$ArrDelay))
lot2001 <- read.csv("2001.csv.bz2")
lot2001['Id'] <- seq(1,length(lot2001$ArrDelay))
lot2002 <- read.csv("2002.csv.bz2")
lot2002['Id'] <- seq(1,length(lot2002$ArrDelay))
lot2003 <- read.csv("2003.csv.bz2")
lot2003['Id'] <- seq(1,length(lot2003$ArrDelay))


####instalacja pakietów####
install.packages("ggplot2")
install.packages("zoo")
library(zoo)
library(ggplot2)
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
install.packages("dplyr")
library(dplyr)
install.packages("rsconnect")


####Analiza danych do problemów badawczych####

##analiza 2001 roku##
lot2001['Date']<- paste(sprintf("%04d", lot2001$Year), sprintf("%02d", lot2001$Month), sprintf("%02d", lot2001$DayofMonth), sep='-')

WTC <- aggregate(Id~Date, lot2001, FUN = function(x) length(x))
WTC <- WTC %>%
  mutate(Average = rollapply(Id, 7, mean, align = "right", fill = NA))
WTC <- WTC[order(WTC$Date), ]
WTC <- as_tibble(WTC)
WTC <- transform(WTC, date=as.Date(Date, frac=0))

wykres2001 <- WTC %>% #2001 rok ilosc lotow na dzien
  ggplot(aes(x=date, y=Average)) +
  geom_line(size=2, colour="#a80000") +
  geom_point(size=5, colour="#a80000")+
  labs(x="Data", y="Ilość lotów", title="Średnia krocząca ilości lotów z ostatnich 7 dni w 2001 roku") + 
  transition_reveal(date)+
  theme_grey(base_size = 22)+
  coord_cartesian(ylim=c(10000,18000))+
  theme(plot.margin=unit(c(1,1,1,1), "cm"), plot.title = element_text(hjust = 0.5))
       

animate(wykres2001, height=700, width=1000, nframes=240, end_pause=60, fps=24)
anim_save("wykres2001.gif", wykres2001, height=700, width=1000, nframes=240, end_pause=60, fps=24)


##analiza 2000 roku##
lot2000['Date']<- paste(sprintf("%04d", lot2000$Year), sprintf("%02d", lot2000$Month), sprintf("%02d", lot2000$DayofMonth), sep='-')

zwykly <- aggregate(Id~Date, lot2000, FUN = function(x) length(x))
zwykly <- zwykly[order(zwykly$Date), ]
zwykly <- zwykly %>%
  mutate(Average = rollapply(Id, 7, mean, align = "right", fill = NA))
zwykly <- as_tibble(zwykly)
zwykly <- transform(zwykly, date=as.Date(Date, frac=0))


wykres2000 <- zwykly %>% #2000 rok ilosc lotow na dzien
  ggplot(aes(x=date, y=Average)) +
  geom_line(size=2, colour="#0c5c00") +
  geom_point(size=5, colour="#0c5c00")+
  labs(x="Data", y="Ilość lotów", title="Średnia krocząca ilości lotów z ostatnich 7 dni w 2000 roku") + 
  transition_reveal(date)+
  theme_grey(base_size = 22) + 
  coord_cartesian(ylim=c(10000,18000))+
  theme(plot.margin=unit(c(1,1,1,1), "cm"), plot.title = element_text(hjust = 0.5))

animate(wykres2000, height=700, width=1000, nframes=240, end_pause=60, fps=24)
anim_save("wykres2000.gif", wykres2000, height=700, width=1000, nframes=240, end_pause=60, fps=24)


##analiza porównawcza 2000 i 2001 roku##
porownanie <- WTC
porownanie <- as.data.frame(WTC)
porownanie['Average2'] <- zwykly$Average[seq(1,365)]
porownanie <- as_data_frame(porownanie)

wykresporownanie <- ggplot()+ #porownanie 2000 i 2001 roku
  geom_line(data=porownanie, aes(x=date, y=Average, color="2001"), size=2)+
  geom_line(data=porownanie, aes(x=date, y=Average2, color="2000"), size=2)+
  geom_point(data=porownanie, aes(x=date, y=Average, color="2001"), size=5)+
  geom_point(data=porownanie, aes(x=date, y=Average2, color="2000"), size=5)+
  labs(x="Data", y="Ilość lotów", title="Porównanie średniej kroczącej ilości lotów w latach 2000 i 2001", color="Rok") + 
  transition_reveal(date)+
  theme_grey(base_size = 22) + 
  coord_cartesian(ylim=c(10000,18000))+
  theme(plot.margin=unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.2, 0.2),
        legend.background = element_rect(fill = "white", color = "black"))+
  scale_color_manual(values = c("2001"="#a80000", "2000"="#0c5c00"))
  
animate(wykresporownanie, height=700, width=1000, nframes=240, end_pause=60, fps=24)
anim_save("wykresporownanie.gif", wykresporownanie, height=700, width=1000, nframes=240, end_pause=60, fps=24)


##analiza lotów w skali dnia - 11.08.2001##
eighteleven['godzina'] < -eighteleven$ArrTime <- substr(data$Tekst, 1, 2)
data$Ostatnie <- substr(data$Tekst, -2)
  paste(sprintf("%02d", lot2000$Month), sprintf("%02d", lot2000$DayofMonth), sep='-')
eighteleven <- lot2001[lot2001$Date=='2001-08-11', ]
eighteleven <- eighteleven[, c("Id", "ArrTime")]
eighteleven <- eighteleven[!is.na(eighteleven$ArrTime), ]
eighteleven["Id"] <- seq(1, length(eighteleven$Id))
rownames(eighteleven) <- NULL
eightelevenwykres <- ggplot(eighteleven, aes(x=Id, y=ArrTime)) + #wszystkie loty 11 sierpnia
  geom_point(size=0.3, colour="#262626") +
  scale_y_continuous(breaks = c(400, 800, 1200, 1600, 2000, 2400), labels = c("4:00", "8:00", "12:00", "16:00", "20:00", "24:00")) + 
  labs(x="Lot", y="Godzina przylotu", title="Godziny wszystkich przylotów w dniu 11.08.2001") +
  coord_cartesian(ylim=c(0,2400))+
  theme(plot.margin=unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))

ggsave("eighteleven.png", eightelevenwykres, width = 10, height = 7, dpi = 300)


##analiza lotów w skali dnia - 11.09.2001##
nineeleven <- lot2001[lot2001$Date=='2001-09-11', ]
nineelevenloty <- nineeleven[, c("Id", "ArrTime")]
nineelevenloty <- nineelevenloty[!is.na(nineelevenloty$ArrTime), ]
nineelevenloty["Id"] <- seq(1, length(nineelevenloty$Id))
rownames(nineelevenloty) <- NULL
nineelevenwykres <- ggplot(nineelevenloty, aes(x=Id, y=ArrTime)) + #wszystkie loty 11 września
  geom_point(size=0.3, colour="#262626") +
  scale_y_continuous(breaks = c(400, 800, 1200, 1600, 2000, 2400), labels = c("4:00", "8:00", "12:00", "16:00", "20:00", "24:00")) + 
  labs(x="Lot", y="Godzina przylotu", title="Godziny wszystkich przylotów w dniu 11.09.2001") +
  coord_cartesian(ylim=c(0,2400))+
  theme(plot.margin=unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))

ggsave("nineeleven.png", nineelevenwykres, width = 10, height = 7, dpi = 300)


##analiza odwolan w 2001 roku##
odwolane2001 <- aggregate(Cancelled ~ Date, lot2001, sum)
odwolane2001 <- transform(odwolane2001, date=as.Date(Date, frac=0))
wykresodwolane2001 <- ggplot(odwolane2001, aes(x=date, y=Cancelled, fill=Cancelled)) + #razem odwolane w 2001
  geom_col()+
  scale_fill_gradient(low = "blue", high = "red")+
  labs(x="Data", y="Odwołane loty", title="Łączna ilość anulowanych lotów danego dnia w 2001 roku", fill="Ilość") + 
  theme(plot.margin=unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        legend.position = c(0.9, 0.7),
        legend.background = element_rect(fill = "white", color = "black"))

ggsave("odwolane2001.png", wykresodwolane2001, width = 10, height = 7, dpi = 300)


##analiza opoznien w 2001 roku##
opoznienie2001 <- aggregate(ArrDelay ~ Date, lot2001, mean)
opoznienie2001 <- transform(opoznienie2001, date=as.Date(Date, frac=0))
wykresopoznienie2001 <- ggplot(opoznienie2001, aes(x=date, y=ArrDelay, fill=ArrDelay))+ #srednie opoznienie w 2001
  geom_col()+
  scale_fill_gradient(low = "blue", high = "red")+
  labs(x="Data", y="Opóźnienie", title="Średnie opóźnienie wszystkich lotów danego dnia w 2001 roku", fill="Minuty") + 
  theme(plot.margin=unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        legend.position = c(0.9, 0.7),
        legend.background = element_rect(fill = "white", color = "black"))

ggsave("opoznienie2001.png", wykresopoznienie2001, width = 10, height = 7, dpi = 300)

  
  
##analiza opoznien we wrzesniu 2001 roku##
nine <- lot2001[lot2001$Month=='9', ]
nine['Date'] <- paste(sprintf("%02d", nine$Month), sprintf("%02d", nine$DayofMonth), sep='-')
opoznienie200109 <- aggregate(ArrDelay ~ Date, nine, mean)
wykresopoznienie200109 <- ggplot(opoznienie200109, aes(x=Date, y=ArrDelay, fill=ArrDelay))+ #srednie opoznienie we wrzesniu 2001
  geom_col()+
  scale_fill_gradient(low = "blue", high = "red")+
  labs(x="Data",
       y="Opóźnienie",
       title="Średnie opóźnienie wszystkich lotów danego dnia we wrześniu 2001 roku",
       fill="Minuty") + 
  theme(plot.margin=unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        legend.position = c(0.9, 0.7),
        legend.background = element_rect(fill = "white", color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("opoznienie200109.png", wykresopoznienie200109, width = 10, height = 7, dpi = 300)


##procentowy udzial przewoznikow w lotach w 1999roku##
linie1999 <- lot1999[, c("Id", "UniqueCarrier")]
linie1999df <- linie1999 %>% 
  group_by(UniqueCarrier) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

linie1999df <- linie1999df %>%
  arrange(desc(perc)) %>%
  mutate(przewoznik=paste(linie1999df$UniqueCarrier,paste(round(linie1999df$perc, 2), "%", sep=""), sep=" - "))

przewoznicy1999wykres <- ggplot(linie1999df, aes(x = "", y = n, fill = przewoznik)) + #porcentowy przewoznicy 1999
  geom_col() +
  geom_bar(width = 1, size = 0.3, color = "white", stat = "identity") +
  labs(title="Procentowy udział przewoźników w lotach w 1999 roku",
       fill="Przewoźnik",
       x="",
       y="") + 
  coord_polar(theta = "y")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key.size = unit(0.45, 'cm'),
        plot.margin=unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = c(0.5, 0.5),
        legend.background = element_rect(fill = "white", color = "black"))+
  scale_fill_discrete(breaks=linie1999df$przewoznik)

ggsave("przewoznicy1999a.png", przewoznicy1999wykres, width = 10, height = 7, dpi = 300)


##procentowy udzial przewosnikow w 2003roku##
linie2003 <- lot2003[, c("Id", "UniqueCarrier")]
linie2003df <- linie2003 %>% 
  group_by(UniqueCarrier) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

linie2003df <- linie2003df %>%
  arrange(desc(perc)) %>%
  mutate(przewoznik=paste(linie2003df$UniqueCarrier,paste(round(linie2003df$perc, 2), "%", sep=""), sep=" - "))

przewoznicy2003wykres <- ggplot(linie2003df, aes(x = "", y = n, fill = przewoznik)) + #porcentowy przewoznicy 2003
  geom_col() +
  geom_bar(width = 1, size = 0.3, color = "white", stat = "identity") +
  labs(title="Procentowy udział przewoźników w lotach w 2003 roku",
       fill="Przewoźnik",
       x="",
       y="") + 
  coord_polar(theta = "y")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key.size = unit(0.45, 'cm'),
        plot.margin=unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = c(0.5, 0.5),
        legend.background = element_rect(fill = "white", color = "black"))+
  scale_fill_discrete(breaks=linie2003df$przewoznik)

ggsave("przewoznicy2003.png", przewoznicy2003wykres, width = 10, height = 7, dpi = 300)


##eksperymentalnie: jaki procent wszystkich lotów był odwołany w danym roku? ##
odwolania1999 <- lot1999[, c("Id", "Cancelled")]
odwolania1999 <- odwolania1999[odwolania1999$Cancelled==1, ]
odw1999 <- length(odwolania1999[, "Id"])/length(lot1999[, "Id"])

odwolania2000 <- lot2000[, c("Id", "Cancelled")]
odwolania2000 <- odwolania2000[odwolania2000$Cancelled==1, ]
odw2000 <- length(odwolania2000[, "Id"])/length(lot2000[, "Id"])

odwolania2001 <- lot2001[, c("Id", "Cancelled")]
odwolania2001 <- odwolania2001[odwolania2001$Cancelled==1, ]
odw2001 <- length(odwolania2001[, "Id"])/length(lot2001[, "Id"])

odwolania2002 <- lot2002[, c("Id", "Cancelled")]
odwolania2002 <- odwolania2002[odwolania2002$Cancelled==1, ]
odw2002 <- length(odwolania2002[, "Id"])/length(lot2002[, "Id"])

odwolania2003 <- lot2003[, c("Id", "Cancelled")]
odwolania2003 <- odwolania2003[odwolania2003$Cancelled==1, ]
odw2003 <- length(odwolania2003[, "Id"])/length(lot2003[, "Id"])



ggplot(linie2003df, aes(x = "", y = perc, fill = UniqueCarrier)) +
  geom_col() +
  labs(title="Procentowy udział przewoźników w lotach w 2003 roku",
       fill="Przewoźnik",
       x="",
       y="") + 
  coord_polar(theta = "y")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key.size = unit(0.45, 'cm'))

odwolania <- c(odw1999, odw2000, odw2001, odw2002, odw2003)
odwolania <- as_data_frame(odwolania)
odwolania["rok"] <- c("1999", "2000", "2001", "2002", "2003")
odowlaniaporownaniewykres <- ggplot(odwolania, aes(x=rok, y=value, fill=value))+#procent odwolanych lotow w latach
  geom_col()+
  labs(x="Rok", y="Odsetek odwołanych lotów", title="Odsetek odwołanych lotów w latach od 1999 do 2003") + 
  theme(plot.margin=unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        legend.position = "none")+
  scale_y_continuous(breaks = c(0.00, 0.01, 0.02, 0.03, 0.04), labels = c("0%", "1%", "2%", "3%", "4%"))

ggsave("odwolaniaporownanie.png", odowlaniaporownaniewykres, width = 10, height = 7, dpi = 300)




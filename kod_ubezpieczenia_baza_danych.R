###########################KOD GENERUJACY DANE O POLISACH -UBEZPIECZENIA MAJATKOWE/ZYCIOWE##########################

##################################################################################################################
####instalacja pakietow
library(dplyr)
library(lubridate)


#################################################################################################################

####Generowanie przykłądowego zbioru danych
DaneSprzedazowe <- data.frame(
  DataSprzedazy = sample(seq(as.Date('2023-01-01'), as.Date('2023-06-30'), by="day"), 25000, replace=TRUE),
  RodzajUbezpieczenia = sample(c("Życiowe", "Majątkowe"), 25000, replace=TRUE),
  KwotaSkladki = runif(25000, 100, 1000),
  Region = sample(c("Północny", "Południowy", "Wschodni", "Zachodni"), 25000, replace=TRUE),
  CzasTrwaniaSprzedazy = sample(1:15, 25000, replace=TRUE),
  PreferowanyKontakt = sample(c("Telefon", "Email", "Osobiście"), 25000, replace=TRUE),
  NowyKlient = sample(c(0, 1), 25000, replace=TRUE),
  SprzedanePolisy = sample(c(0, 1), 25000, replace=TRUE),
  IDSprzedawcy = sample(1:20, 25000, replace=TRUE),
  Kontakt = sample(c("Telefon", "Email", "Osobiście"), 25000, replace=TRUE)
)

#################################################################################################################

#### Modyfikacja czasu trwania sprzedaży dla większej zmienności

##############################################################################################################

####Nowy klient

####Jesli mamy nowego klienta-czas obslugi wydluza sie do 50-60 minut
DaneSprzedazowe$CzasTrwaniaSprzedazy <- ifelse(DaneSprzedazowe$NowyKlient == 1,
                                               sample(50:60, 25000, replace=TRUE),
                                               DaneSprzedazowe$CzasTrwaniaSprzedazy)
#############################################################################################################


####Wprowadzenie wiekszej losowosci w danych,modyfikacja kolumny czas zprzedazy polisy-podzial na  słabych,srednich,wyzej srednich i najlepszych

####slabi sprzedawcy

####Wybieram losowo 3 sprzedawcow i nadaje im gorszy czas trwania obslugi-miedzy 60 a 90 minut

losowi_sprzedawcy <- sample(unique(DaneSprzedazowe$IDSprzedawcy), 3, replace = FALSE)

#### Nadaj gorsze wyniki sprzedawcom
DaneSprzedazowe$SprzedanePolisy[DaneSprzedazowe$IDSprzedawcy %in% losowi_sprzedawcy] <- 0
DaneSprzedazowe$CzasTrwaniaSprzedazy[DaneSprzedazowe$IDSprzedawcy %in% losowi_sprzedawcy] <- sample(60:90, sum(DaneSprzedazowe$IDSprzedawcy %in% losowi_sprzedawcy), replace = TRUE)

####sredni sprzedawcy

#Wybieram 4 losowych sprzedawcow i nadaje im srednie wyniki 40-60 minut

sredni_sprzedawcy <- sample(unique(DaneSprzedazowe$IDSprzedawcy), 4, replace = FALSE)
DaneSprzedazowe$SprzedanePolisy[DaneSprzedazowe$IDSprzedawcy %in% sredni_sprzedawcy] <- 0
DaneSprzedazowe$CzasTrwaniaSprzedazy[DaneSprzedazowe$IDSprzedawcy %in% sredni_sprzedawcy] <- sample(40:60, sum(DaneSprzedazowe$IDSprzedawcy %in% sredni_sprzedawcy), replace = TRUE)


####wyzej sredni
####Wybieram 4 losowych sprzedawcow i nadaje im srednie wyniki 20-30 minut

wyzej_sprzedawcy <- sample(unique(DaneSprzedazowe$IDSprzedawcy), 4, replace = FALSE)
DaneSprzedazowe$CzasTrwaniaSprzedazy[DaneSprzedazowe$IDSprzedawcy %in% wyzej_sprzedawcy] <- sample(20:30, sum(DaneSprzedazowe$IDSprzedawcy %in% sredni_sprzedawcy), replace = TRUE)


####5 najlepszych sprzedawcow 

####Wybierz losowo 5 innych sprzedawców do sztucznego zwiększenia wyników-nadaj srednie wyniki 10-20min

inni_sprzedawcy <- setdiff(unique(DaneSprzedazowe$IDSprzedawcy), losowi_sprzedawcy)
wybrani_sprzedawcy <- sample(inni_sprzedawcy, 5, replace = FALSE)

####Zwiększ wyniki wybranym sprzedawcom:sredni czas trwania sprzedazy 10-20 min 
DaneSprzedazowe$SprzedanePolisy[DaneSprzedazowe$IDSprzedawcy %in% wybrani_sprzedawcy] <- 1
DaneSprzedazowe$CzasTrwaniaSprzedazy[DaneSprzedazowe$IDSprzedawcy %in% wybrani_sprzedawcy] <- sample(10:20, sum(DaneSprzedazowe$IDSprzedawcy %in% wybrani_sprzedawcy), replace = TRUE)

####################################################################################################################

#### Dodanie kolumny "DecyzjaKlienta"

####roznicuje stalych i nowych klientow oraz to czy  klient kupil polise czy zrezygnowal z zakupu badz odnowy pakietu

DaneSprzedazowe$DecyzjaKlienta <- ifelse(DaneSprzedazowe$NowyKlient == 1,
                                         ifelse(DaneSprzedazowe$SprzedanePolisy == 1, "Nowy klient - zakup polisy", "Nowy klient - odmowa zakupu polisy"),
                                         ifelse(DaneSprzedazowe$SprzedanePolisy == 1, "Stały klient - zakup polisy", "Stały klient - rezygnacja"))


#############################################################################################################

####Modyfikacja kolumny-sprzedane polisy
####Modyfikacja danych-dodanie 3 najgorszych wynikow (najmniej sprzedanych polis) losowym sprzedawcom(wygenerowani wczeasniej-ci najgorsi co mieli najdluzszy czas tez sprzedazy), malej liczby sprzedanych polis

####dodatkowi_sprzedawcy <- c(1, 12, 20)
ilosci_wierszy <- c(100, 90, 80)

for (i in 1:length(losowi_sprzedawcy)) {
  indeksy <- sample(which(DaneSprzedazowe$IDSprzedawcy == losowi_sprzedawcy[i]), ilosci_wierszy[i])
  DaneSprzedazowe$SprzedanePolisy[indeksy] <- 1
}
losowi_sprzedawcy
sredni_sprzedawcy

####analogicznie dodanie srednim sprzedawcom liczby sprzedanych polis by zroznicowac troche stawke

ilosci_wierszyy <- c(250, 260, 180,230)

for (i in 1:length(sredni_sprzedawcy)) {
  indeksy <- sample(which(DaneSprzedazowe$IDSprzedawcy == sredni_sprzedawcy[i]), ilosci_wierszyy[i])
  DaneSprzedazowe$SprzedanePolisy[indeksy] <- 1
}
###############################################################################################################

###zapisanie
nazwa_pliku <- "DaneSprzedazowe.xlsx"
write.xlsx(DaneSprzedazowe, file = nazwa_pliku, rowNames = FALSE)


##############################################################################################################
####DANE O SPRZEDAWCACH

####Generuje dane o 20 spzredawcach działających w czterech rejonach

dane_sprzedawcy <- data.frame(
  IDSprzedawcy = 1:20,
  Imie = c("Adam", "Ewa", "Tomasz", "Magdalena", "Piotr", "Anna", "Jan", "Katarzyna", "Marcin", "Alicja",
           "Marek", "Weronika", "Bartłomiej", "Izabela", "Łukasz", "Karolina", "Michał", "Natalia", "Robert", "Klaudia"),
  Nazwisko = c("Nowak", "Kowalska", "Wójcik", "Kowalczyk", "Szymański", "Mazur", "Kamiński", "Zielińska", "Lewandowski", "Jankowska",
               "Kowal", "Czerwinska", "Wozniak", "Kaczmarek", "Krupa", "Sikora", "Adamski", "Duda", "Czarnecki", "Sobczak"),
  ObszarDzialalnosci = c("Północny", "Południowy", "Wschodni", "Zachodni", "Południowy", "Wschodni", "Północny", "Zachodni", "Północny", "Południowy",
                         "Zachodni", "Północny", "Południowy", "Wschodni", "Północny", "Zachodni", "Wschodni", "Południowy", "Północny", "Zachodni")
)


###################################################################################################################

####zapisanie
nazwa_pliku <- "DaneSprzedawcy.xlsx"
write.xlsx(dane_sprzedawcy, file = nazwa_pliku, rowNames = FALSE)

####################################################################################################################


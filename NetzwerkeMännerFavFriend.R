# Freundschaftsnetzwerk im Sport (Männermannschaft)
## Gruppenmitglieder: Eileen Breuer (eb053), Anna von Teuffel (at059), Merle Flachsbarth (mf136), Nina Büchs (nb087), Selina Spieß (ss420)

# COMMMENT SWS
# modified 2018-05-07 / 9:48
# fully functional visualization

library("igraph")
library("visNetwork")

###Fav/Friend Netzwerk Männer

favfriend <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/EdgelistMenFavFriend.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/NodelistMenFavFriend.csv", header=T, as.is=T, sep = ",")
# head(favfriend)
wties <-as.matrix(favfriend)
favfriend <- graph_from_data_frame(d=wties, vertices=nodes, directed=T)
favfriend

### Attribute zur Visualisierung festlegen

# Kantenattribute dauerhaft festlegen
E(favfriend)$arrow.size <- .1
E(favfriend)$color="black"
E(favfriend)$curved=.3

# Visualisierung nach degrees in Abstufung von gelb nach rot
hd <- degree(favfriend, mode = "all")
fine = 4
palette = colorRampPalette(c('yellow','red'))
ired = palette(fine)[as.numeric(cut(hd, breaks = fine))]

# Vertexattribute dauerhaft festlegen
V(favfriend)$size=hd #degree
V(favfriend)$color=ired
V(favfriend)$label.dist=2
V(favfriend)$label.degree=0
V(favfriend)$label.cex=.5
V(favfriend)$label.family="Helvetica"
V(favfriend)$label.color="black"
V(favfriend)$frame.color="white"

# Visualisierung nach Degree-Verteilung

# Falls notwendig: zurücksetzen der Vergleichsnetzwerke
par(mfrow=c(1,1))

fav_degree <- plot(favfriend, layout = layout_with_kk, main="Populäre Spieler und Bekannte")


# Farbe nach Geschlecht festlegen -> ist ja witzlos hier, weil es eine Männermannschaft ist, oder?

Frauen<-V(favfriend)[sex=="w"]
Frauen
V(favfriend)[Frauen]$color="maroon1"

Men<-V(favfriend)[sex=="m"]
Men
V(favfriend)[Men]$color="skyblue1"

fav_sex <- plot(favfriend, layout = layout_with_kk, main="Geschlechtsverteilung")

# Knotenform nach Typ festlegen
Spieler<-V(favfriend)[person=="Player"]
Spieler
V(favfriend)[Spieler]$shape="square"

Bekannte<-V(favfriend)[person=="Friend"]
Bekannte
V(favfriend) [Bekannte]$shape="circle"

fav_freunde <- plot(favfriend, layout = layout_with_kk, main="Freunde und Bekannte")

# Art der Beziehung festlegen

Freunde<- E(favfriend)[friendship == "1"]
Freunde
E(favfriend)[Freunde]$lty = "dotted"

Favoriten<- E(favfriend)[favorite == "1"]
Favoriten
E(favfriend)[Favoriten]$lty = "solid"

# Visualisierung der Freundschaftsbeziehuungen
fav_edges <- plot(favfriend, layout = layout_with_kk, main="Freundschaftsbzeziehungen")

### ANMERKUNG: Statt der Visualisierung über die Linienart könnten Sie auch über das Liniengewicht arbeiten, aber das nur am Rande.

# Gesamtvisualisierung nur mit igraph
men_friends_all <- plot(favfriend, layout = layout_with_kk, vertex.size=hd, vertex.color=ired, main="Freundschaftsnetzwerk Männermannschaft gesamt", sub="Verteilung nach Degree")


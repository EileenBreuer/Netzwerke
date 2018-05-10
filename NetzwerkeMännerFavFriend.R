# Freundschaftsnetzwerk im Sport (Männermannschaft)
## Gruppenmitglieder: Eileen Breuer (eb053), Anna von Teuffel (at059), Merle Flachsbarth (mf136), Nina Büchs (nb087), Selina Spieß(ss420)

#1. Im ersten Schritt müssen die Basis-Pakete installiert und geladen werden. Diese Schritte können bei Bedarf überprungen werden.

#I-Graph Installation
install.packages("igraph") # installiert das Paket igraph
install.packages("igraphdata") # installiert das Paket igraphdata
library("igraph") # lÃ¤dt das Paket "igraph" zum Verwenden
?igraph # liefert die Hilfefunktion für igraph

library("visNetwork")

###Fav/Friend Netzwerk Männer einlesen

favfriend <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/EdgelistMenFavFriend.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/NodelistMenFavFriend.csv", header=T, as.is=T, sep = ",")
# head(favfriend)
wties <-as.matrix(favfriend)
favfriend <- graph_from_data_frame(d=wties, vertices=nodes, directed=T)
favfriend

#### Attribute zur Visualisierung festlegen

# Kantenattribute dauerhaft festlegen
E(favfriend)$arrow.size <- .1
E(favfriend)$color="black"

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

fav_degree <- plot(favfriend, layout = layout_with_kk, main="Freundschaftsnetzwerk innerhalb des Vereins", sub="Knoten nach Degree eingefärbt")

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
E(favfriend)[Freunde]$color = "snow3"

Favoriten<- E(favfriend)[favorite == "1"]
Favoriten
E(favfriend)[Favoriten]$color = "black"

Freunde<- E(favfriend)[friendship == "1"]
Freunde
E(favfriend)[Freunde]$lty = "dotted"

Favoriten<- E(favfriend)[favorite == "1"]
Favoriten
E(favfriend)[Favoriten]$lty = "solid"

# Visualisierung der Freundschaftsbeziehuungen
fav_edges <- plot(favfriend, layout = layout_with_kk, main="Freundschaftsbzeziehungen")

# Gesamtvisualisierung nur mit igraph
men_friends_all <- plot(favfriend, layout = layout_with_kk, vertex.size=hd, vertex.color=ired, main="Freundschaftsnetzwerk Männermannschaft", sub="Nodes:  nach Degree - Edges: nach Beziehungsart")

######### Analyse des fav/friend-Netzwerks
betweenness(favfriend, directed = TRUE)
# Nico (SpielerA) höchster Wert=35,75
closeness (favfriend, mode ="out")
# geht nicht, weil nicht verbunden
eigen_centrality(favfriend, directed = TRUE)
# wie liest man das aus?
# Anzahl	der	Dyaden im Netzwerk "Fav/Friend-MÃ¤nner"
dyad.census(favfriend)
# 10 mutuelle - 30 asymetrische - 131 null
triad_census(favfriend)
# Anzahl und Art der Triaden im Netzwerk "Männer" 
# 34 mal den Out Star 021D und 4 mal eine Linie 021C
# 003-516 012-241 102-119 021D-20 021U-20 021C-6 111D-22 111U-5 030T-2 030C-0 201-5 120D-7 120U-0 120C-1 210-3 300-2

vertex_attr(favfriend)
#Auflistung der Knoten Attribute 
components(favfriend)
# Anzahl der Componenten: 19
is_connected(favfriend)
#sind diese Componenten verbunden? TRUE (ja)
diameter(favfriend)
# Durchmesser des Netzwerks: 9 (Schritte maximal von einem zum anderen Ende)
mean_distance(favfriend)
# Pfaddistanz des Netzwerks: 2,18 (Schritte durchschnittlich von einem zum anderen Ende)
edge_density(favfriend)
# Dichte des Netzwerks: 14% (fast 15) (% der möglichen Beziehungen sind realisiert)
cluster_walktrap(favfriend)
# Anzahl der Cluster: 4 ("Kleingruppen")

degree(favfriend, mode = "out")
# Anzeigen der Outdegrees pro Akteur Freundschaftsnetzwerks, Dino max. Outdegrees, sonst noch Max, Constantin, Zagros, Pius, Jacob, Darly, Petros, Ferdinand und Daniel-->liegt aber daran, dass jeder so viele angeben musste!
degree(favfriend, mode="out", normalized = TRUE)
# Anzeigen der prozentualen (d.h. normalisierten) Outdegrees des Freundschaftsnetzwerks der Männer an

degree(favfriend, mode = "in")
# Anzeigen der Indegrees pro Akteur Freundschaftsnetzwerks, maximal bei Spieler D, Spieler E, Spieler K und Spieler L
degree(favfriend, mode="in", normalized = TRUE)
# Anzeigen der prozentualen (d.h. normalisierten) Outdegrees des Freundschaftsnetzwerks der Männer an

# der Wert der Zentralisierung, der Closeness und der Betweenness interessiert hier wenig, 
# da es sich um mehrere Ego Netze handelt, 

#Anzeigen der Cluster im Bild
favfriendcluster <- cluster_walktrap(favfriend)
modularity(favfriendcluster)
membership(favfriendcluster)
plot(favfriendcluster, favfriend, edge.arrow.size = 0.2)

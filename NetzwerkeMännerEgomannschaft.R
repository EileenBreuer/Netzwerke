### Gesamtnetzwerk erstellen
# COMMMENT SWS
# modified 2018-05-07 / 9:48
# fully functional visualization

# KOMMENTAR
# hier müssen Sie aufpassen, dass sie die richtigen Netzwerke verwenden.
# die Netzwerke müssen auch zuvor im Zwischenspeicher geladen sein.
# Sie müssen die Netzwerke zuvor als variable definieren, sonst klappt die Zuordnung nicht.

library("igraph")

# liest favfriend netzwerk ein
favfriend <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/EdgelistMenFavFriend.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/NodelistMenFavFriend.csv", header=T, as.is=T, sep = ",")
wties <-as.matrix(favfriend)
favfriend <- graph_from_data_frame(d=wties, vertices=nodes, directed=T)
favfriend

# liest support netzwerk ein
support <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/EdgelistMenSupport.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/NodelistMenSupport.csv", header=T, as.is=T, sep = ",")
hties <-as.matrix(support) # Umwandlung der Edgelist in eine Matrix
support <- graph_from_data_frame(d=hties, vertices=nodes, directed=T)

# Kombination der Edge- und Nodelist in ein GEMEINSAMES igraph-Objekt umwandeln
# da sie unterschiedliche Attribute haben, muss das über den Befehl graph.union umgesetzt werden. Lassen Sie sich von der Fehlermeldung nicht stören, das Netzwerk funktioniert.

m <- print_all(support %u% favfriend)
plot(m)

# Kantenattribute dauerhaft festlegen
E(m)$arrow.size <- .1
E(m)$color="black"
E(m)$curved=.3

# Visualisierung nach degrees in Abstufung von gelb nach rot
hd <- degree(m, mode = "all")
fine = 10
palette = colorRampPalette(c('yellow','red'))
ired = palette(fine)[as.numeric(cut(hd, breaks = fine))]

# Vertexattribute dauerhaft festlegen
V(m)$size=hd #degree
V(m)$color=ired
V(m)$label.dist=2
V(m)$label.degree=0
V(m)$label.cex=.5
V(m)$label.family="Helvetica"
V(m)$label.color="black"
V(m)$frame.color="white"

plot(m, layout = layout_with_kk, main="Gesamtnetzwerk Beziehung- und Freundschaftsnetzwerk", sub="Visualisierung nach Degrees")

## COMMENT SWS
# auch die vergleichende Darstellung funktioniert.
# Wenn sie allerdings die Netzwerke "schöner" darstellen wollen, müssen Sie die Edge- und Kantenattribute aus den anderen Skripten verwenden.

# vergleichende Darstellung der beiden Teilnetze
par(mfrow=c(1,2), mar=c(0,0,2,0))
plot(support, layout=layout_with_kk, edge.arrow.size=0.2, main="Beziehungsnetzwerk MÃ¤nnermannschaft - UnterstÃ¼tzer")
plot(favfriend, layout=layout_with_kk, edge.arrow.size=0.2, main="Freundschaftsnetzwerk MÃ¤nnermmannschaft - Freunde")

### EGO-NETZWERKE ERSTELLEN UND VISUALISIEREN

# Achtung: Alle Visualisierungen, die wir auf das Netzwerk "m" angewendet haben, werden nun auch genau so weiter an die Unternetzwerke ausgegeben.

# Zurücksetzen der Visualisierungsparameter
par(mfrow=c(1,1)

# Ego Netzwerke der stark und wenig unterstützten Spieler erstellen

# Visualisierung
me <- subgraph<-make_ego_graph(m, order=1, c("SpielerE"))
me #ignorieren Sie die Fehlermeldung hier.
plot(me[[1]], edge.arrow.size=.1, layout=layout_with_kk, layout=layout_nicely, edge.curved=0.2, main="Ego-Netzwerk Spieler E")

mk<- subgraph<-make_ego_graph(m, order=1, c("SpielerK"))
mk
plot(mk[[1]], edge.arrow.size=.1, layout=layout_with_kk, layout=layout_nicely, edge.curved=0.2)

#erstellt Egonetzwerk von Knoten mit ID 18, zeigt alle Knoten an, die innerhalb eines Schrittes mit 18 verbunden sind

mm <- subgraph<-make_ego_graph(m, order=1, c("SpielerM"))
mm
plot(mm[[1]], edge.arrow.size=.1, layout=layout_with_kk, layout=layout_nicely, edge.curved=0.2)

mb <- subgraph<-make_ego_graph(m, order=1, c("SpielerB"))
mb
plot(mb[[1]], edge.arrow.size=.1, layout=layout_with_kk, layout=layout_nicely, edge.curved=0.2)

# vergleichende Darstellung der vier MÃ¤nner
par(mfrow=c(2,2), mar=c(0,0,2,0))
plot(me[[1]], layout=layout_with_kk, main="Spieler E")
plot(mk[[1]], layout=layout_with_kk, main="Spieler K")
plot(mm[[1]], layout=layout_with_kk, main="Spieler M")
plot(mb[[1]], layout=layout_with_kk, main="Spieler B")


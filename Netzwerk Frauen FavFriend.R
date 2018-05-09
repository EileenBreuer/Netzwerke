# Sportskanonen
# Freundschaftsnetzwerk im Sport (Frauenmannschaft)
# Gruppenmitglieder: Eileen Breuer (eb053), Anna von Teuffel (at059), Merle Flachsbarth (mf136), Nina Büchs (nb087), Selina Spieß (ss420)

# I-Graph Installation
install.packages("igraph") #installiert das Paket igraph
install.packages("igraphdata") #installiert das Paket igraphdata
library("igraph") #laedt das Paket "igraph" zum Verwenden

# visNetwork Installation
install.packages("visNetwork") # installiert das Paket visNetwork
library("visNetwork")

# Netzwerk von GitHub eingebunden: 
favfriend <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/Edgelist%20Frauen%20FavFriend.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/Nodelist%20Frauen%20FavFriend.csv", header=T, as.is=T, sep = ",")
head(favfriend)
wties <-as.matrix(favfriend)
favfriend <- graph_from_data_frame(d=wties, vertices=nodes, directed=T)
favfriend #zeigt das irgraph-Objekt an

# Visualisierung

# Visualisierung nach degrees in Abstufung von gelb nach rot
hd <- degree(favfriend, mode = "all")
fine = 4
palette = colorRampPalette(c('yellow','red'))
ired = palette(fine)[as.numeric(cut(hd, breaks = fine))]

# Kantenattribute dauerhaft festlegen
E(favfriend)$arrow.size <- .1
E(favfriend)$color="black"

# Knotenattribute dauerhaft festlegen
V(favfriend)$size=hd*2
V(favfriend)$color=ired
V(favfriend)$label.dist=2
V(favfriend)$label.degree=0
V(favfriend)$label.cex=.7
V(favfriend)$label.family="Helvetica"
V(favfriend)$label.color="black"
V(favfriend)$frame.color="white"
# V(f)$label <- NA # bei Bedarf zum Entfernen der Labels

fav_degree <- plot(favfriend, layout = layout_with_kk, main="Freundschaftsnetzwerk innerhalb des Vereins", sub="Knoten nach Degree eingefaerbt")

# Knotenformen nach Typ festlegen --> in diesem Netzwerk koennte man sich diesen Schritt sparen, da alle genannten Personen Spielerinnen sind
Spieler<-V(favfriend)[person=="Player"]
Spieler
V(favfriend)[Spieler]$shape="circle"

Bekannte<-V(favfriend)[person=="Friend"]
Bekannte
V(favfriend) [Bekannte]$shape="square"

fav_freunde <- plot(favfriend, layout = layout_with_kk, main="Freunde und Bekannte ?")

# Art der Beziehung offenlegen
Freunde<- E(favfriend)[friendship == "1"]
Freunde
E(favfriend)[Freunde]$lty = "dotted"

Favoriten<- E(favfriend)[favorite == "1"]
Favoriten
E(favfriend)[Favoriten]$lty = "solid"

# Visualisierung der Freundschaftsbeziehuungen
fav_edges <- plot(favfriend, layout = layout_with_kk, main="Freundschaftsbzeziehungen")

# Gesamtvisualisierung nur mit igraph
women_friends_all <- plot(favfriend, layout = layout_with_kk, vertex.size=hd*2, vertex.color=ired, main="Freundschaftsnetzwerk Frauenmannschaft", sub="Nodes: nach Degree -  Edges: nach Beziehungsart")

#Anzeigen der Cluster im Bild
favfriendcluster <- cluster_walktrap(favfriend)
modularity(favfriendcluster)
membership(favfriendcluster)
plot(favfriendcluster, favfriend, edge.arrow.size = 0.2)

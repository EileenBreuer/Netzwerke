# Sportskanonen
# Besondere Ego Netze (Frauenmannschaft)
# Gruppenmitglieder: Eileen Breuer (eb053), Anna von Teuffel (at059), Merle Flachsbarth (mf136), Nina Büchs (nb087), Selina Spieß (ss420)

# I-Graph Installation
install.packages("igraph") #installiert das Paket igraph
install.packages("igraphdata") #installiert das Paket igraphdata
library("igraph") #laedt das Paket "igraph" zum Verwenden

# visNetwork Installation
install.packages("visNetwork") # installiert das Paket visNetwork
library("visNetwork")

# Netzwerk aus GitHub eingebunden
f <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/Edgelist%20Frauen%20Gesamtnetzwerk.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/Nodelist%20Frauen%20Gesamtnetzwerk.csv", header=T, as.is=T, sep = ",")
head(f)

hties <-as.matrix(f) # Umwandlung der Edgelist in eine Matrix
f <- graph_from_data_frame(d=hties, vertices=nodes, directed=T)

edge_attr(f) #zeigt die Verteilung des Attributs "weight" an.
vertex_attr(f) #zeigt die Verteilung der Vertex-Attribute an.
class(f) #Test, ob ein igraph-Objekt vorliegt
f #zeigt das igraph-Objekt an

### Visualisierung

# Kantenattribute dauerhaft festlegen
E(f)$arrow.size <- .1
E(f)$color="black"
E(f)$lty="solid"

## Knotenattribute
hd <- degree(f, mode = "all") # Groesse der Knoten wird nach dem Degree Wert bestimmt
V(f)$size=hd*2 # Knotengroesse
V(f)$label.dist=2 
V(f)$label.degree=0 # Labelposition
V(f)$label.cex=1
V(f)$label.family="Helvetica" # Labelschriftart
V(f)$label.color="black"
V(f)$frame.color="white"
# V(f)$label <- NA # bei Bedarf zum Entfernen der Labels

plot(f, layout = layout_with_kk, main="Gesamtnetzwerk Frauenmannschaft", sub="Beziehungen zwischen Spielern und Alteri nach Visualisierungsparametern")

women_f <- visIgraph(f, layout = "layout_with_kk") #erstellt das VisNetwork-Objekt, basierend auf Kamada-Kawai
women_f # ruft den Plot auf

# Visualisierung nach Geschlecht:
Frauen<-V(f)[sex=="w"] #waehlt alle Knoten aus, die das Knoten-Attribut $sex gleich "w" haben
Frauen
V(f)[Frauen]$color="maroon1" #weist allen Werten Weiblich die Farbe "maroon1" zu

Men<-V(f)[sex=="m"] #waehlt alle Knoten aus, die das Knotenattribut $sex gleich "m" haben
Men
V(f)[Men]$color="skyblue1"

# Visualisierung nach Art:
Spieler<-V(f)[person=="Player"]
Spieler
V(f)[Spieler]$shape="circle"

Bekannte<-V(f)[person=="Friend"]
Bekannte
V(f) [Bekannte]$shape="square"

## Kantenattribute
# Art der Unterstuetzung durch Edge-Attribute darstellen
Instrumental <- E(f)[instrumental == "1"] #waehlt alle Kanten aus, die das Kanten-Attribut "1" bei $instrumental gesetzt haben
Instrumental
E(f)[Instrumental]$color = "yellow" #weist allen Werten von Instrumental die Farbe "yellow" zu.

Medizinisch <- E(f)[medical == "1"] #waehlt alle Kanten aus, die das Kanten-Attribut "1" bei $medical gesetzt haben
Medizinisch
E(f)[Medizinisch]$color = "red" #weist allen Werten von Medizinisch die Farbe "red" zu.

Finanziell <- E(f)[financial == "1"] #waehlt alle Kanten aus, die das Kanten-Attribut "1" bei $financial gesetzt haben
Finanziell
E(f)[Finanziell]$color = "darkorange" #weist allen Werten von Finanziell die Farbe "orange" zu.

Emotional <- E(f)[emotional == "1"] #waehlt alle Kanten aus, die das Kanten-Attribut "1" bei $medical gesetzt haben
Emotional
E(f)[Emotional]$color = "firebrick3" #weist allen Werten von Medizinisch die Farbe "red" zu.

plot(f, layout = layout_with_kk, main="Gesamtnetzwerk Beziehung- und Freundschaftsnetzwerk", sub="Visualisierung nach Degrees")

# Art der Freundschaft durch Edge-Attribute darstellen
Freunde<- E(f)[friendship == "1"]
Freunde
E(f)[Freunde]$lty = "dotted"

Favoriten<- E(f)[favorite == "1"]
Favoriten
E(f)[Favoriten]$lty = "solid"

## falls dotted eine Fehlermeldung gibt, Art der Beziehung durch Farben dargestellt: 
# Freunde<- E(f)[friendship == "1"]
# Freunde
# E(f)[Freunde]$color = "snow3"
# Favoriten<- E(f)[favorite == "1"]
# Favoriten
# E(f)[Favoriten]$color = "black"

# Visualisierung der Freundschaftsbeziehuungen
fav_edges <- plot(f, layout = layout_with_kk, main="Freundschaftsbzeziehungen")

ego_women_gender_f <- plot(f, layout = layout_with_kk, main="Gesamtnetzwerk Frauenmannschaft", sub="Nodes: nach Geschlecht, Edges: nach Beziehungsart")
ego_women_gender_f

# ! Visualisierungsparameter erstrecken sich auch auf die folgenden Ego-Netzwerke !

### Ego-Netzwerke

## Ego Netzwerke der stark und wenig unterstuetzten Spielerinnen
# Spielerin 7 - hohe In-Degree Zahl
fa <- subgraph <- make_ego_graph(f, order=1, c("Spielerin7")) 
fa
plot(fa[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 7")

# Spielerin 5 - keine In-Degrees
fz <- subgraph <- make_ego_graph(f, order=1, c("Spielerin5")) 
fz
plot(fz[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 5")

## Ego Netzwerke und deren Vergleich aufgrund ihrer Struktur
# Spielerin 4
fs <- subgraph <- make_ego_graph(f, order=1, c("Spielerin4")) 
fs
plot(fs[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 4")

# Spielerin 8
fr <- subgraph <- make_ego_graph(f, order=1, c("Spielerin8")) 
fr
plot(fr[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 8")

# Spielerin 2
fj <- subgraph <- make_ego_graph(f, order=1, c("Spielerin2")) 
fj
plot(fj[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 2")

# Spielerin 1
fc <- subgraph <- make_ego_graph(f, order=1, c("Spielerin1")) 
fc
plot(fc[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 1")

# vergleichende Darstellung der vier Frauen
par(mfrow=c(2,2), mar=c(2,0,2,0))   
plot(fs[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 4")
plot(fr[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 8")
plot(fj[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 2")
plot(fc[[1]], layout=layout_nicely, edge.arrow.size=0.2, main="Ego Spielerin 1")

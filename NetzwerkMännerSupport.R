# Unterstützernetzwerke im Sport (MÃ¤nnermannschaft)
## Gruppenmitglieder: Eileen Breuer (eb053), Anna von Teuffel (at059), Merle Flachsbarth (mf136), Nina BÃ¼chs (nb087), Selina SpieÃŸ (ss420)

# COMMMENT SWS
# modified 2018-05-07 / 9:48
# fully functional visualization

#1. Im ersten Schritt müssen die Basis-Pakete installiert und geladen werden. Diese Schritte kÃ¶nnen bei Bedarf Ã¼berprungen werden.

#I-Graph Installation
install.packages("igraph") # installiert das Paket igraph
install.packages("igraphdata") # installiert das Paket igraphdata
library("igraph") # lÃ¤dt das Paket "igraph" zum Verwenden
?igraph # liefert die Hilfefunktion für igraph

### Unterstützernetzwerk Männer ###

# Die Edge- und Nodelist werden direkt aus dem GitHub Verzeichnis ausgelesen.
support <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/EdgelistMenSupport.csv", header=T, as.is=T, sep = ",")

# lädt die edgelist aus github in der working directory. Auf Trennzeichen achten!
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/NodelistMenSupport.csv", header=T, as.is=T, sep = ",")

# lÃ¤dt die Nodelist aus github in die working directory

head(support) # überprüfen von Edge- und Nodelist in R
hties <-as.matrix(support) # Umwandlung der Edgelist in eine Matrix
support <- graph_from_data_frame(d=hties, vertices=nodes, directed=T)

# Kombination der Edge- und Nodelist in ein igraph-Objekt

support #zeigt das igraph-Objekt an
edge_attr(support) #zeigt die Verteilung des Attributs "weight" an.
vertex_attr(support) #zeigt die Verteilung der Vertex-Attribute an.
class(support) #Test, ob ein igraph-Objekt vorliegt

### Visualisierung Support-Netzwerk ###

# Dieser Befehl wird ganz am Ende ausgeführt. Durch ihn wird das Netzwerk visualisiert.

#Ausrechnen der Degrees für Größe der Knoten
dh<-degree(support)
dh

plot(support, layout = layout_with_kk, main="UnterstÃ¼tzernetzwerk MÃ¤nnermannschaft", sub="Formen der UnterstÃ¼tzerbeziehungen zwischen Spielern und Alteri", vertex.frame.color="grey", edge.arrow.size=0.1, edge.curved=0.3, vertex.label.dist=1, vertex.label.degree=0, vertex.label.cex=.4, vertex.label.font=1, vertex.label.color="dimgrey", vertex.size=dh*2, , edge.lty="solid")

# Etwas schöner mit VisNetwork:

# Kantenattribute dauerhaft festlegen
E(support)$arrow.size <- .1
E(support)$color="darkgrey"
E(support)$curved=.3

# Visualisierung nach degrees in Abstufung von gelb nach rot
hd <- degree(support, mode = "all")
fine = 4
palette = colorRampPalette(c('yellow','red'))
ired = palette(fine)[as.numeric(cut(hd, breaks = fine))]

# Vertexattribute dauerhaft festlegen
V(support)$size=hd #degree
V(support)$color=ired
V(support)$label.dist=2
V(support)$label.degree=0
V(support)$label.cex=.5
V(support)$label.family="Helvetica"
V(support)$label.color="dimgrey"
V(support)$frame.color="white"

# Visualisierung nur mit igraph, verwendet alle Attribute, die wir oben bei Edge und Vertex-Attributen festgelegt haben.

plot(support, layout = layout_with_kk, main="UnterstÃ¼tzernetzwerk MÃ¤nnermannschaft", sub="Formen der UnterstÃ¼tzerbeziehungen zwischen Spielern und Alteri")


# Visualisierung mit VisNetwork

# Der Vorteil ist, dass hier die Netzwerke teilweise besser zu erkennen sind. Ausserdem lÃ¤sst sich das Netzwerk auch leicht als Website sichern:

library(visNetwork)

#erstellt das VisNetwork-Objekt, basierend auf Kamada-Kawai
men_support <- visIgraph(support, layout = "layout_with_kk")

# ruft den Plot auf
men_support

# Speziell fÃ¼r HTML-Ausgaben: fÃ¼gt dem Netzwerk noch Navigationshilfen hinzu:

men_support_int <- visIgraph(support, layout = "layout_with_kk")%>%
  visInteraction(navigationButtons = TRUE)

men_support_int

# exportiert das Netzwerk als Datei zur weiteren Verwendung im Web in ihrer Working-Directory
visSave(men_support, file = "men_support.html", background = "black")

### Geschlechtsbeziehungen offenlegen

#Knoten einfÃ¤rben je nach männlich/weiblich
Frauen<-V(support)[sex=="w"] #wählt alle Knoten aus, die das Knoten-Attribut $sex gleich "w" haben
Frauen
V(support)[Frauen]$color="maroon1" #weist allen Werten Weiblich die Farbe "maroon1" zu

Men<-V(support)[sex=="m"]#wählt alle Knoten aus, die das Knotenattribut $sex gleich "m" haben
Men
V(support)[Men]$color="skyblue1"

### Art der Unterstützung festlegen

#Knoten verschiedene Formen zuweisen je nach Spieler/Unterstützer
Spieler<-V(support)[person=="Player"] #wählt alle Knoten aus, die das Knoten-Attribut $person gleich "player" haben
Spieler
V(support)[Spieler]$shape="circle" #weist allen ausgewÃ¤hlten Knoten die Â§Form gleich "circle" zu

Bekannte<-V(support)[person=="Friend"] #wÃ¤hlt alle Knoten aus, die das Knotenattribut $person gleich "friend" haben
Bekannte
V(support) [Bekannte]$shape="square"


### COMMENT SWS: ###

# Visualisierung nach Männern und Frauen

ego_men_gender <- plot(support, layout = layout_with_kk, main="UnterstÃ¼tzernetzwerk Männermannschaft", sub="Formen der Unterstützerbeziehungen zwischen Spielern und Alteri, visualisiert nach Männern und Frauen")

ego_men_gender

## Art der Unterstützung durch Edge-Attribute festlegen

# NUR Für EDGELIST, IN DER DER SUPPORT NUR EINE ZEILE UMFASST Edges einfÃ¤rben je nach Unterstützerform

# wählt alle Kanten aus, die das Kanten-Attribut "1" bei $instrumental gesetzt haben
Instrumental<- E(support)[support == "1"]
Instrumental
E(support)[Instrumental]$color = "yellow"
# weist allen Werten von Instrumental die Farbe "yellow" zu.

# wählt alle Kanten aus, die das Kanten-Attribut "2" bei $medical gesetzt haben
Medical<- E(support)[support == "2"]
Medical
E(support)[Medical]$color = "red"
# weist allen Werten von Medizinisch die Farbe "red" zu.

# wählt alle Kanten aus, die das Kanten-Attribut "3" bei $financial gesetzt haben
Financial<- E(support)[support == "3"]
Financial
E(support)[Financial]$color = "orange"
# weist allen Werten von Finanziell die Farbe "orange" zu.

#wählt alle Kanten aus, die das Kanten-Attribut "4" bei $medical gesetzt haben
Emotional<- E(support)[support == "4"]
Emotional
E(support)[Emotional]$color = "tomato"
# weist allen Werten von Medizinisch die Farbe "red" zu.


# Visualisierung nach Männern und Frauen
# Visualisierung nach Art der Unterstützung

ego_men_gender_support <- plot(support, layout = layout_with_kk, main="Unterstützernetzwerk Männermannschaft", sub="Formen der Unterstützerbeziehungen zwischen Spielern und Alteri, Nodes visualisiert nach Männern und Frauen, Edges nach Art der Unterstützung")

ego_men_gender_support



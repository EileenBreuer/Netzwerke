#Gesamtnetzwerk im Sport (Männermannschaft)
## Gruppenmitglieder: Eileen Breuer (eb053), Anna von Teuffel (at059), Merle Flachsbarth (mf136), Nina Büchs (nb087), Selina Spieß (ss420)

#1. Im ersten Schritt müssen die Basis-Pakete installiert und geladen werden. Diese Schritte kÃ¶nnen bei Bedarf Ã¼berprungen werden.

#I-Graph Installation
install.packages("igraph") # installiert das Paket igraph
install.packages("igraphdata") # installiert das Paket igraphdata
library("igraph") # lädt das Paket "igraph" zum Verwenden
?igraph # liefert die Hilfefunktion für igraph

# Die Edge- und Nodelist werden direkt aus dem GitHub Verzeichnis ausgelesen.
mg <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/EdgelistMenGesamtnetzwerk.csv", header=T, as.is=T, sep = ",")
# lädt die edgelist aus github in der working directory. Auf Trennzeichen achten!
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/NodelistMenGesamtnetzwerk.csv", header=T, as.is=T, sep = ",")
# lädt die Nodelist aus github in die working directory

head(mg) # überprüfen von Edge- und Nodelist in R
hties <-as.matrix(mg) # Umwandlung der Edgelist in eine Matrix
mg <- graph_from_data_frame(d=hties, vertices=nodes, directed=T)

# Kombination der Edge- und Nodelist in ein igraph-Objekt
mg # zeigt das igraph-Objekt an
edge_attr(mg) # zeigt die Verteilung des Attributs "weight" an.
vertex_attr(mg) # zeigt die Verteilung der Vertex-Attribute an.
class(mg) # Test, ob ein igraph-Objekt vorliegt

#### Visualisierung mg-Netzwerks

# Kantenattribute dauerhaft festlegen
E(mg)$arrow.size <- .05
E(mg)$color="black"
E(mg)$lty="solid"

# Vertexattribute dauerhaft festlegen
V(mg)$size=4
V(mg)$label.dist=2
V(mg)$label.degree=0
V(mg)$label.cex=.5
V(mg)$label.family="Helvetica"
V(mg)$label.color="black"
V(mg)$frame.color="white"

# Visualisierung nur mit igraph, verwendet alle Attribute, die wir oben bei Edge und Vertex-Attributen festgelegt haben.

plot(mg, layout = layout_with_kk, main="Gesamtnetzwerk Männermannschaft", sub="Formen der Beziehungen zwischen Spielern und Alteri")


# Visualisierung mit VisNetwork
# Der Vorteil ist, dass hier die Netzwerke teilweise besser zu erkennen sind. Ausserdem lÃ¤sst sich das Netzwerk auch leicht als Website sichern:

library(visNetwork)
#erstellt das VisNetwork-Objekt, basierend auf Kamada-Kawai
men_mg <- visIgraph(mg, layout = "layout_with_kk")
# ruft den Plot auf
men_mg
# Speziell für HTML-Ausgaben: fÃ¼gt dem Netzwerk noch Navigationshilfen hinzu:
men_mg_int <- visIgraph(mg, layout = "layout_with_kk")%>%
  visInteraction(navigationButtons = TRUE)
men_mg_int

# exportiert das Netzwerk als Datei zur weiteren Verwendung im Web in ihrer Working-Directory
visSave(men_mg, file = "men_mg.html", background = "black")

#Geschlechtsbeziehungen offenlegen

#Knoten einfärben je nach männlich/weiblich
Frauen<-V(mg)[sex=="w"] #wählt alle Knoten aus, die das Knoten-Attribut $sex gleich "w" haben
Frauen
V(mg)[Frauen]$color="maroon1" #weist allen Werten Weiblich die Farbe "maroon1" zu

Men<-V(mg)[sex=="m"]#wählt alle Knoten aus, die das Knotenattribut $sex gleich "m" haben
Men
V(mg)[Men]$color="skyblue1"

# Art der Unterstützung festlegen

#Knoten verschiedene Formen zuweisen je nach Spieler/Unterstützer
Spieler<-V(mg)[person=="Player"] #wählt alle Knoten aus, die das Knoten-Attribut $person gleich "player" haben
Spieler
V(mg)[Spieler]$shape="circle" #weist allen ausgewÃ¤hlten Knoten die §Form gleich "circle" zu

Bekannte<-V(mg)[person=="Friend"] #wÃ¤hlt alle Knoten aus, die das Knotenattribut $person gleich "friend" haben
Bekannte
V(mg) [Bekannte]$shape="square"


# Visualisierung nach Männern und Frauen
ego_men_gender <- plot(mg, layout = layout_with_kk, main="Unterstützernetzwerk Männermannschaft", sub="Formen der Unterstützerbeziehungen zwischen Spielern und Alteri, visualisiert nach Männern und Frauen")
ego_men_gender

# Art der Unterstützung durch Edge-Attribute festlegen

Instrumental<- E(mg)[instrumental == "1"]# wählt alle Kanten aus, die das Kanten-Attribut "1" bei $instrumental gesetzt haben
Instrumental
E(mg)[Instrumental]$color = "yellow"# weist allen Werten von Instrumental die Farbe "yellow" zu.

Medizinisch<- E(mg)[medical == "1"]# wählt alle Kanten aus, die das Kanten-Attribut "1" bei $medical gesetzt haben
Medizinisch
E(mg)[Medizinisch]$color = "red"# weist allen Werten von Medizinisch die Farbe "red" zu.

Finanziell<- E(mg)[financial == "1"]# wählt alle Kanten aus, die das Kanten-Attribut "1" bei $financial gesetzt haben
Finanziell
E(mg)[Finanziell]$color = "darkorange" # weist allen Werten von Finanziell die Farbe "orange" zu.

Emotional<- E(mg)[emotional == "1"]# wählt alle Kanten aus, die das Kanten-Attribut "1" bei $medical gesetzt haben
Emotional
E(mg)[Emotional]$color = "firebrick3"# weist allen Werten von Medizinisch die Farbe "red" zu.

men_mg <- plot(mg, layout = layout_with_kk, main="Gesamtnetzwerk Männermannschaft", sub="Formen der Unterstützerbeziehungen zwischen Spielern und Alteri, visualisiert nach Männern und Frauen")
men_mg

# Art der Beziehung festlegen

Freunde<- E(mg)[friendship == "1"]
Freunde
E(mg)[Freunde]$color = "snow3"

Favoriten<- E(mg)[favorite == "1"]
Favoriten
E(mg)[Favoriten]$color = "black"

Freunde<- E(mg)[friendship == "1"]
Freunde
E(mg)[Freunde]$lty = "dotted"

Favoriten<- E(mg)[favorite == "1"]
Favoriten
E(mg)[Favoriten]$lty = "solid"

# Visualisierung der Freundschaftsbeziehuungen
fav_edges <- plot(mg, layout = layout_with_kk, main="Freundschaftsbzeziehungen")

# Visualisierung nach Männern und Frauen
# Visualisierung nach Art der Unterstützung

ego_men_gender_mg <- plot(mg, layout = layout_with_kk, main="Gesamtnetzwerk Männermannschaft", sub="Nodes visualisiert nach Geschlecht, Edges nach Beziehungsart")

ego_men_gender_mg

# Visualisierung ohne Labels
V(mg)$label <- NA # überschreibt alle Labels mit dem Wert "NA", der nicht angezeigt wird.
gesamtnetzwerkmg <- plot(mg, layout = layout_with_kk, main="Gesamtnetzwerk Männermannschaft", sub="Nodes visualisiert nach Geschlecht, Edges nach Beziehungsart")

# Wiederherstellung der Labels
V(mg)$label <- V(mg)$name # weist dem Vertex-Attribut "label" wieder das Vertex-Attribut "name" zu.
gesamtnetzwerkmg <- plot(mg, layout = layout_with_kk, main="Gesamtnetzwerk Männermannschaft", sub="Nodes visualisiert nach Geschlecht, Edges nach Beziehungsart")

###### Analyse des Gesamtnetzwerks der Männermannschaft

betweenness(mg, directed = TRUE)
# Spieler A gleich höchster Wert mit 170
closeness (mg, mode ="out")
# gibt es nicht, weil unverbunden
# Anzahl	der	Dyaden im Netzwerk "mg-Männer"
dyad.census(mg)
# 13 Mutuelle, 105 asymetrische, 1208 null
# Anzahl und Art der Triaden im Netzwerk "MÃ¤nner" 
triad_census(mg)
# 003-19007 012-1480 102-1323 021D-106 021U-19 021C-54 111D-25 111U-64 030T-0 030C-0 201-5 120D-9 120U-2 120C-0 210-2 300-4
# Auflistung der Knoten Attribute 
vertex_attr(mg)
# Anzahl der Componenten: 52
components(mg)
# sind diese Componenten verbunden? FALSE(true)
is_connected(mg)
# Durchmesser des Netzwerks: 13 (Schritte maximal von einem zum anderen Ende)
diameter(mg)
# Pfaddistanz des Netzwerks: 2,76 (Schritte durchschnittlich von einem zum anderen Ende)
mean_distance(mg)
# Dichte des Netzwerks: 0,049% (% der möglichen Beziehungen sind realisiert)
edge_density(mg)
# Anzahl der Cluster: 10 ("Kleingruppen")
cluster_walktrap(mg)
# Anzeigen der Cluster im Bild
gc <- cluster_walktrap(mg)
modularity(gc)
membership(gc)
plot(gc, mg, edge.arrow.size = 0.2)

degree(mg, mode = "out")
# Anzeigen der Outdegrees pro Akteur des Netzwerk "Männermg"
degree(mg, mode="out", normalized = TRUE)
# Anzeigen der prozentualen (d.h. normalisierten) Outdegrees des Netzwerk "Männermg" an


# ! Visualisierungsparameter erstrecken sich auch auf die folgenden Ego-Netzwerke !
#####Egonetzwerke

# Ego Netzwerke der stark und wenig unterstützten Spieler erstellen

# Visualisierung
me <- subgraph<-make_ego_graph(mg, order=1, c("SpielerE"))
me #ignorieren Sie die Fehlermeldung hier.
plot(me[[1]], edge.arrow.size=.05, layout=layout_with_kk, layout=layout_nicely, main="Egonetzwerk Spieler E")

mk<- subgraph<-make_ego_graph(mg, order=1, c("SpielerK"))
mk
plot(mk[[1]], edge.arrow.size=.05, layout=layout_with_kk, layout=layout_nicely, main="Egonetzwerk Spieler K")

# erstellt Egonetzwerk von Knoten mit ID 18, zeigt alle Knoten an, die innerhalb eines Schrittes mit 18 verbunden sind

mm <- subgraph<-make_ego_graph(mg, order=1, c("SpielerM"))
mm
plot(mm[[1]], edge.arrow.size=.05, layout=layout_with_kk, layout=layout_nicely, main="Egonetzwerk Spieler M")

mb <- subgraph<-make_ego_graph(mg, order=1, c("SpielerB"))
mb
plot(mb[[1]], edge.arrow.size=.05, layout=layout_with_kk, layout=layout_nicely, main="Egonetzwerk Spieler B")

# vergleichende Darstellung der vier MÃ¤nner
par(mfrow=c(2,2), mar=c(0,0,2,0))
plot(me[[1]], layout=layout_with_kk, main="Spieler E")
plot(mk[[1]], layout=layout_with_kk, main="Spieler K")
plot(mm[[1]], layout=layout_with_kk, main="Spieler M")
plot(mb[[1]], layout=layout_with_kk, main="Spieler B")

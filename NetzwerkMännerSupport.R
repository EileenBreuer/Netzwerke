# Unterstützernetzwerke im Sport (Männermannschaft)
## Gruppenmitglieder: Eileen Breuer (eb053), Anna von Teuffel (at059), Merle Flachsbarth (mf136), Nina Büchs (nb087), Selina Spieß (ss420)

#1. Im ersten Schritt müssen die Basis-Pakete installiert und geladen werden. Diese Schritte kÃ¶nnen bei Bedarf Ã¼berprungen werden.

#I-Graph Installation
install.packages("igraph") # installiert das Paket igraph
install.packages("igraphdata") # installiert das Paket igraphdata
library("igraph") # lädt das Paket "igraph" zum Verwenden
?igraph # liefert die Hilfefunktion für igraph

# Die Edge- und Nodelist werden direkt aus dem GitHub Verzeichnis ausgelesen.
support <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/EdgelistMenSupport.csv", header=T, as.is=T, sep = ",")
# lädt die edgelist aus github in der working directory. Auf Trennzeichen achten!
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/NodelistMenSupport.csv", header=T, as.is=T, sep = ",")
# lädt die Nodelist aus github in die working directory

head(support) # überprüfen von Edge- und Nodelist in R
hties <-as.matrix(support) # Umwandlung der Edgelist in eine Matrix
support <- graph_from_data_frame(d=hties, vertices=nodes, directed=T)

# Kombination der Edge- und Nodelist in ein igraph-Objekt
support #zeigt das igraph-Objekt an
edge_attr(support) #zeigt die Verteilung des Attributs "weight" an.
vertex_attr(support) #zeigt die Verteilung der Vertex-Attribute an.
class(support) #Test, ob ein igraph-Objekt vorliegt

#### Visualisierung Support-Netzwerks

# Kantenattribute dauerhaft festlegen
E(support)$arrow.size <- .1
E(support)$color="black"
# E(support)$width <- E(support)$weight/2 # bei Bedarf der Darstellung der Beziehungsstärke hinzufügen

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
V(support)$label.color="black"
V(support)$frame.color="white"

# Visualisierung nur mit igraph, verwendet alle Attribute, die wir oben bei Edge und Vertex-Attributen festgelegt haben.

plot(support, layout = layout_with_kk, main="Unterstützernetzwerk Männermannschaft", sub="Formen der Unterstützerbeziehungen zwischen Spielern und Alteri")


# Visualisierung mit VisNetwork
# Der Vorteil ist, dass hier die Netzwerke teilweise besser zu erkennen sind. Ausserdem lÃ¤sst sich das Netzwerk auch leicht als Website sichern:

library(visNetwork)
#erstellt das VisNetwork-Objekt, basierend auf Kamada-Kawai
men_support <- visIgraph(support, layout = "layout_with_kk")
# ruft den Plot auf
men_support
# Speziell für HTML-Ausgaben: fÃ¼gt dem Netzwerk noch Navigationshilfen hinzu:
men_support_int <- visIgraph(support, layout = "layout_with_kk")%>%
  visInteraction(navigationButtons = TRUE)
men_support_int

# exportiert das Netzwerk als Datei zur weiteren Verwendung im Web in ihrer Working-Directory
visSave(men_support, file = "men_support.html", background = "black")

#Geschlechtsbeziehungen offenlegen

#Knoten einfärben je nach männlich/weiblich
Frauen<-V(support)[sex=="w"] #wählt alle Knoten aus, die das Knoten-Attribut $sex gleich "w" haben
Frauen
V(support)[Frauen]$color="maroon1" #weist allen Werten Weiblich die Farbe "maroon1" zu

Men<-V(support)[sex=="m"]#wählt alle Knoten aus, die das Knotenattribut $sex gleich "m" haben
Men
V(support)[Men]$color="skyblue1"

# Art der Unterstützung festlegen

#Knoten verschiedene Formen zuweisen je nach Spieler/Unterstützer
Spieler<-V(support)[person=="Player"] #wählt alle Knoten aus, die das Knoten-Attribut $person gleich "player" haben
Spieler
V(support)[Spieler]$shape="circle" #weist allen ausgewÃ¤hlten Knoten die §Form gleich "circle" zu

Bekannte<-V(support)[person=="Friend"] #wÃ¤hlt alle Knoten aus, die das Knotenattribut $person gleich "friend" haben
Bekannte
V(support) [Bekannte]$shape="square"


# Visualisierung nach Männern und Frauen
ego_men_gender <- plot(support, layout = layout_with_kk, main="Unterstuetzernetzwerk Männermannschaft", sub="Formen der Unterstützerbeziehungen zwischen Spielern und Alteri, visualisiert nach Männern und Frauen")
ego_men_gender

# Art der Unterstützung durch Edge-Attribute festlegen

Instrumental<- E(support)[instrumental == "1"]# wählt alle Kanten aus, die das Kanten-Attribut "1" bei $instrumental gesetzt haben
Instrumental
E(support)[Instrumental]$color = "yellow"# weist allen Werten von Instrumental die Farbe "yellow" zu.

Medizinisch<- E(support)[medical == "1"]# wählt alle Kanten aus, die das Kanten-Attribut "1" bei $medical gesetzt haben
Medizinisch
E(support)[Medizinisch]$color = "red"# weist allen Werten von Medizinisch die Farbe "red" zu.

Finanziell<- E(support)[financial == "1"]# wählt alle Kanten aus, die das Kanten-Attribut "1" bei $financial gesetzt haben
Finanziell
E(support)[Finanziell]$color = "darkorange" # weist allen Werten von Finanziell die Farbe "orange" zu.

Emotional<- E(support)[emotional == "1"]# wählt alle Kanten aus, die das Kanten-Attribut "1" bei $medical gesetzt haben
Emotional
E(support)[Emotional]$color = "firebrick3"# weist allen Werten von Medizinisch die Farbe "red" zu.


# Visualisierung nach Männern und Frauen
# Visualisierung nach Art der Unterstützung

ego_men_gender_support <- plot(support, layout = layout_with_kk, main="Supportnetzwerk Männermannschaft", sub="Nodes: nach Geschlecht und Art - Edges: nach Art der Unterstützung")

ego_men_gender_support

###### Analyse des support-Netzwerks

betweenness(support, directed = TRUE)
# Spieler A gleich hÃ¶chste mit 6
closeness (support, mode ="out")
#gibt es nicht, weil unverbunden
# Anzahl	der	Dyaden im Netzwerk "Support-MÃ¤nner"
dyad.census(support)
#  2 Mutuelle, 77 asymetrische, 1002 null
# Anzahl und Art der Triaden im Netzwerk "MÃ¤nner" 
triad_census(support)
# 003-14474 012-760 102-924 021D-42 021U-0 021C-3 111D-0 111U-10 030T-0 030C-0 201-0 120D-0 120U-2 120C-0 210-0 300-0
#Auflistung der Knoten Attribute 
vertex_attr(support)
# Anzahl der Componenten: 52
components(support)
#sind diese Componenten verbunden? FALSE(nein)
is_connected(support)
# Durchmesser des Netzwerks: 13 (Schritte maximal von einem zum anderen Ende)
diameter(support)
# Pfaddistanz des Netzwerks: 1,327 (Schritte durchschnittlich von einem zum anderen Ende)
mean_distance(support)
# Dichte des Netzwerks: 3% (fast 4) (% der MÃ¶glichen Beziehungen sind realisiert)
edge_density(support)
# Anzahl der Cluster: 11 ("Kleingruppen")
cluster_walktrap(support)
#Anzeigen der Cluster im Bild
gc <- cluster_walktrap(support)
modularity(gc)
membership(gc)
plot(gc, support, edge.arrow.size = 0.2)

degree(support, mode = "out")
# Anzeigen der Outdegrees pro Akteur des Netzwerk "MÃ¤nnerSupport",Spieler E und Spieler C hÃ¶chsten Outdegree
degree(support, mode="out", normalized = TRUE)
# Anzeigen der prozentualen (d.h. normalisierten) Outdegrees des Netzwerk "MÃ¤nnerSupport" an

# der Wert der Zentralisierung, der Closeness und der Betweenness interessiert hier wenig, 
# da es sich um mehrere Ego Netze handelt, 
# diese Werte werden im Spieler Netzwerk spannender

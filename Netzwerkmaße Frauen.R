# Sportskanonen
# Skript von Anna von Teuffel - 04.04.
# Die Netzwerk- und Akteursmaße werden nur für das Netzwerk der Frauenmannschaft bestimmt 
# (nicht für das der Unterstuetzung) da nur hier eine Analyse Sinn macht um die Struktur innerhalb 
# der Mannschaft aufzudecken. 

# I-Graph Installation
install.packages("igraph") # installiert das Paket igraph
install.packages("igraphdata") # installiert das Paket igraphdata

library("igraph") # lädt das Paket "igraph" zum Verwenden

?igraph # liefert die Hilfefunktion für igraph

# Freundschaftsnetzwerk
favfriend <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/Edgelist%20Frauen%20FavFriend.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/Nodelist%20Frauen%20FavFriend.csv", header=T, as.is=T, sep = ",")
head(favfriend)
wties <-as.matrix(favfriend)
favfriend <- graph_from_data_frame(d=wties, vertices=nodes, directed=T)
favfriend #zeigt das irgraph-Objekt an

# einfache Darstellung des Freundschaftsnetzes
plot(favfriend, layout = layout_with_kk, edge.arrow.size=0.2, edge.curved=0.2, 
     main = "Netzwerk Frauenmannschaft")

### Analyse ### Infos zum Netzwerk

# Anzahl der Knoten und Kanten im Netzwerk:
ecount(favfriend) # Anzahl der Kanten: 26
vcount(favfriend) # Anzahl der Knoten: 12

# Netzwerkmaße

# Dichte des Netzwerks: 19,6
edge_density(favfriend)

# Anzahl der Cluster: 6 "Kleingruppen" - 3 mit mehr al einem Knoten
cluster_walktrap(favfriend)

# Plotten der Cluster des Netzwerks in Farben 
gc <- cluster_walktrap(favfriend)
modularity(gc)
membership(gc)
plot(gc, favfriend, edge.arrow.size = 0.2)

# Pfaddistanz des Netzwerks: 2
mean_distance(favfriend)

# Durchmesser des Netzwerks: 5 
diameter(favfriend)

# Anzahl	der	Dyaden im Netzwerk "Frauen": 5 mutuelle, 16 asymetrische und 45 nicht realisierte Beziehungen
dyad.census(favfriend)

# Anzahl und Art der Triaden im Netzwerk "Frauen": 92 54 35  9  5 10  2  7  0  0  3  2  1  0  0  0
triad_census(favfriend)

### Akteurs-Maße
# Spielerinnen 7, 9-12 werden ausgeklammert, das sie nicht befragt werden konnten

# Am Besten vernetzte Person: Out-Degrees = Kommunikationsaktivität, In-Degrees = Erreichbarkeit
degree(favfriend) # highest: Spielerin 6 (82%), lowest: Spielerin 5 (36%)
degree(favfriend, mode="all", normalized = TRUE) # prozentualer Wert zum besseren Vergleich
# "Beliebteste" Person: nach In Degress
degree(favfriend, mode ="in") # Spielerin 6
degree(favfriend, mode="in", normalized = TRUE) # prozentualer Wert zum besseren Vergleich
# Zentralisierter Degree-Wert
centr_degree(favfriend)$centralization # durchschnittlicher Wert: 23%

# Closeness: Anzahl Verbindungen um alle Alteri zu erreichen (out) bzw. um von allen Alteri erreicht zu werden (in)
closeness (favfriend)
closeness (favfriend, mode ="out") # highest: Spielerin 5 
closeness (favfriend, mode ="in") # highest: Spielerin 7

# Betweenness (Broker Positions): hohe Zentralität = Kontrolle über Kommunikation unter den Alteri
betweenness(favfriend) # highest: (mit Abstand) Spielerin 1
edge_betweenness(favfriend)
# Zentralisierter Betweenness-Wert:
centr_betw(favfriend, directed = FALSE)$centralization

# Eigenvektor: hohe Zentralität = Kontakt zu "einflussreichen" Akteuren
# leider nicht verständlich: vermutlich Spielerin 7 mit einem Wert von 8.756692e-01
eigen_centrality(favfriend, directed = TRUE)
# Zentralisierter Eigenvector-Wert:
centr_eigen(favfriend, directed = FALSE)$centralization

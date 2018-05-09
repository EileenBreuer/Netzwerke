# Sportskanonen
# Unterstuetzernetzwerke im Sport (Frauenmannschaft)
# Gruppenmitglieder: Eileen Breuer (eb053), Anna von Teuffel (at059), Merle Flachsbarth (mf136), Nina Buechs (nb087), Selina Spie√ü (ss420)

# I-Graph Installation
install.packages("igraph") #installiert das Paket igraph
install.packages("igraphdata") #installiert das Paket igraphdata
library("igraph") #laedt das Paket "igraph" zum Verwenden

# Visualisierung mit visNetwork
install.packages("visNetwork") #installiert das Paket visNetwork
library(visNetwork)

# Unterstuetzernetzwerk von GitHub eingebunden: 
support <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/Edgelist%20Frauen%20Support.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/EileenBreuer/Netzwerke/master/Nodelist%20Frauen%20Support.csv", header=T, as.is=T, sep = ",")
head(support)
hties <-as.matrix(support)
support <- graph_from_data_frame(d=hties, vertices=nodes, directed=T)
support #zeigt das igraph-Objekt an

# Visualisierung 
# Kantenattribute zur besseren Lesbarkeit
E(support)$arrow.size <- .1
E(support)$color="black"
# E(support)$width <- E(support)$weight/2 # bei Bedarf der Darstellung der Beziehungsst‰rke hinzuf¸gen

plot(support) # bis hierhin Standardvisualisierung mit bestimmter Pfeilgroesse und Farbe

# Visualisierung der Knoten
# Visualisierung nach degrees in Abstufungen von rot
hd <- degree(support, mode = "all")
fine = 4
palette = colorRampPalette(c('yellow','red'))
ired = palette(fine)[as.numeric(cut(hd, breaks = fine))]

# Vertexattribute festlegen
V(support)$size=hd*3
V(support)$color=ired
V(support)$label.dist=2
V(support)$label.degree=0
V(support)$label.cex=.7
V(support)$label.family="Helvetica"
V(support)$label.color="black"
V(support)$frame.color="white"

plot(support) # bis hierhin Bestimmung der Farbe und Groesse der Knoten nach dem Degree-Wert

# Geschlecht offenlegen offenlegen - Knoten einfaerben je nach maennlich/weiblich
Frauen<-V(support)[sex=="w"] #waehlt alle Knoten aus, die das Knoten-Attribut $sex gleich "w" haben
Frauen
V(support)[Frauen]$color="maroon1" #weist allen Werten Weiblich die Farbe "maroon1" zu

Men<-V(support)[sex=="m"] #waehlt alle Knoten aus, die das Knotenattribut $sex gleich "m" haben
Men
V(support)[Men]$color="skyblue1"

plot(support) # bis hierhin Groesse der Knoten nach Degree-Wert, Farbe je nach Eigenschaft (male / female)

# Art der Unterstuetzung offenlegen - verschiedene Formen zuweisen je nach Spieler oder Unterstuetzer
Spieler<-V(support)[person=="Player"] #waehlt alle Knoten aus, die das Knoten-Attribut $person gleich "player" haben
Spieler
V(support)[Spieler]$shape="circle" #weist allen ausgewaehlten Knoten die ¬ßForm gleich "circle" zu

Bekannte<-V(support)[person=="Friend"] #waehlt alle Knoten aus, die das Knotenattribut $person gleich "friend" haben
Bekannte
V(support) [Bekannte]$shape="square"

plot(support) # bis hierhin Groesse der Knoten nach Degree-Wert, Farbe je nach Eigenschaft (male / female), Form je nach Spieler / Unterstuetzer

# Visualisierung der Kanten - Die Art der Unterstuetzung wird durch Edge-Attribute dargestellt
Instrumental<- E(support)[instrumental == "1"] #w√§hlt alle Kanten aus, die das Kanten-Attribut "1" bei $instrumental gesetzt haben
Instrumental
E(support)[Instrumental]$color = "yellow" #weist allen Werten von Instrumental die Farbe "yellow" zu.

Medizinisch<- E(support)[medical == "1"] #waehlt alle Kanten aus, die das Kanten-Attribut "1" bei $medical gesetzt haben
Medizinisch
E(support)[Medizinisch]$color = "red" #weist allen Werten von Medizinisch die Farbe "red" zu.

Finanziell<- E(support)[financial == "1"] #waehlt alle Kanten aus, die das Kanten-Attribut "1" bei $financial gesetzt haben
Finanziell
E(support)[Finanziell]$color = "darkorange" #weist allen Werten von Finanziell die Farbe "orange" zu.

Emotional<- E(support)[emotional == "1"] #waehlt alle Kanten aus, die das Kanten-Attribut "1" bei $medical gesetzt haben
Emotional
E(support)[Emotional]$color = "firebrick3" #weist allen Werten von Medizinisch die Farbe "red" zu.

# Plot mit Beschriftung:
ego_women_gender_support <- plot(support, layout = layout_with_kk, main="Unterstuetzernetzwerk Frauenmannschaft", sub="Nodes: visualisiert nach Geschlecht - Edges: nach Art der Unterstuetzung")

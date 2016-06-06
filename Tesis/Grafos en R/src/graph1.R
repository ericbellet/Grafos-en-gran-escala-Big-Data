
g<-read.table(file="g1.csv", header=TRUE, row.names=1, sep=";")
g<-as.network(as.matrix(g)) 
g
plot(g)
g<-read.table(file="g2.csv", header=FALSE, sep=";") 
g<-as.network(g)
g
plot(g)

rm(list=ls())
#install.packages("igraph")
#install.packages("statnet")
#install.packages("rgl")
#install.packages("tnet")
#install.packages("coda")

setwd("C:/Users/eric.bellet/Desktop") 

library(coda)
library(statnet)
library(rgl)
library(tnet)

#création d’un graphe aléatoire de 20 sommets, densité 0.25
g <- rgraph(20, tprob = 0.25)
#densité - résultat prévisible ici...
g
plot(g)
gden(g)
#centralités
din<-degree(g, cmode="indegree") 
dout<-degree(g, cmode="outdegree") 
table(din) 
table(dout)
plot(g)
#pour un graphe non orienté
table(degree(g))
#afficher les résultats triés
x<-sort(degree(g, cmode="freeman")) 
dotchart(x, main="Degree")
#proximité closeness(g)
#intermédiarité betweenness(g)
#vecteurs propres evcent(g)
#réciprocité et transitivité
grecip(g) 
gtrans(g)

#dyades et triades
dyad.census(g) 
triad.census(g)
#distances géodésiques et voisins d’ordre 1, 2...
geodist(g) 
g1<-neighborhood(g,1) 
plot.sociomatrix(g1) 
g2<-neighborhood(g,2) 
plot.sociomatrix(g2)
#points d’articulation
cutpoints(g)
#groupes
kcores(g)
#blockmodels #k = nombre de classes désirées
eq<-equiv.clust(g)
#matrices bloquées
bm3<-blockmodel(g, eq, k=3) 
bm3$block.model 
bm4<-blockmodel(g, eq, k=4) 
bm4$block.model
#extraction d’un égo network
eg<-ego.extract(g1, ego=7) 
gplot(eg)
#visualisation
#création d’un graphe de 20 sommets #partition en 3 blocs #taille des sommets = in-degree #couleurs des sommets = blockmodels




#effacer objets créés précédemment rm(list=ls())
g <- rgraph(20, tprob = 0.25) 
eq<-equiv.clust(g) 
bm <- blockmodel(g, eq, k=3) 
groupes <- bm$block.membership[order(bm$order.vector)]
#couleur sommet = blockmodels #taille = in-degree
gplot(g, displayisolates=FALSE, vertex.cex=degree(g, cmode="indegree")/4, vertex.col=groupes, mode="fruchtermanreingold")
#visualisation 3D
gplot3d(g)

gplot3d(rgws(1,5,3,1,0.2))


data(flo) 
gtrans(flo)
gg<-rgraph(16, m=1000, tprob=gden(flo)) 
tr<-apply(gg,1,gtrans) 
hist(tr, main="Transitivité",ylab="Fréquence")


#---------------------------
rm(list=ls()) 
library(igraph)
g<-read.csv("g2.csv", sep=";", header=TRUE)
g<-as.matrix(g)
g<-graph.edgelist(g, directed=TRUE)
#contrôle
class(g)
plot(g)

#création d’un graphe aléatoire orienté
g <- erdos.renyi.game(20, 1/20, directed=TRUE)
#densité et diamètre
graph.density(g) diameter(g)
#centralités
#degré #choix entre "in", "out" et "total"
table(degree(g, mode=c("in")))
#intermédiarité des sommets et des liens
betweenness(g) 
edge.betweenness(g) 
closeness(g) 
evcent(g)
#distance et plus court chemin
average.path.length(g) 
shortest.paths(g)
#réciprocité
reciprocity(g, ignore.loops=TRUE)
#transitivité locale ou globale
transitivity(g, type=c("local"), vids=NULL) 
transitivity(g)
#dyades et triades
dyad.census(g) 
triad.census(g)
#points d’articulation

articulation.points(g)
#recherche de cliques #à éviter sur les graphes de grande taille
cliques(g) 
largest.cliques(g) 
maximal.cliques(g) 
clique.number(g)
#k-cores
graph.coreness(g)
#suppression boucles et isolés
g1 <- delete.vertices(g, V(g)[degree(g)==0]) 
g2 <- simplify(g1, remove.loops = TRUE)
#composantes "weak" ou "strong"
is.connected(g, mode=c("strong")) 
clusters(g, mode=c("strong"))
#décompose en composantes connexes
compco <- decompose.graph(g, mode = c("weak"), max.comps = NA, min.vertices = 3) 
plot(compco[[1]]) 
plot(compco[[2]])
#arbre couvrant minimum
plot(minimum.spanning.tree(g))
#recherche des voisins d’ordre n
neighborhood.size(g, 1, nodes=V(g), mode=c("all", "out", "in")) 
neighborhood.size(g, 2, nodes=V(g), mode=c("all", "out", "in"))
#3 modes de visualisation
plot(g) #non interactif tkplot(g) #interactif

rglplot(g) #3D non interactif
#choix des algorithmes
help(package="igraph", "layout")
#graphe circulaire
g <- graph.ring(10) 
g$layout <- layout.circle 
plot(g)
#graphe scale-free
g <- barabasi.game(100) 
plot(g, layout=layout.fruchterman.reingold, vertex.size=4, vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
#graphe small-world
g <-watts.strogatz.game(1, 100, 5, 0.05)
plot(g, layout=layout.kamada.kawai, vertex.size=4, vertex.label.dist=0.5, vertex.color="green", edge.arrow.size=0.5)
#graphe aléatoire et une couleur par composante
g <- erdos.renyi.game(60, 1/20)
comps <- clusters(g)$membership 
colbar <- rainbow(max(comps)+1) 
V(g)$color <- colbar[comps+1]
plot(g, layout=layout.fruchterman.reingold, vertex.size=5, vertex.label=NA)
#visualisation des communautés
g <- graph.full(5) %du% graph.full(5) %du% graph.full(5)
g <- add.edges(g, c(0,5, 0,10, 5,10)) 
com <- spinglass.community(g, spins=5)
V(g)$color <- com$membership+1 
g <- set.graph.attribute(g, "layout", layout.kamada.kawai(g))
rglplot(g, vertex.label.dist=1.5)

igraph.par("plot.layout", layout.reingold.tilford) 
plot(graph.tree(20, 2)) 
plot(graph.tree(50, 3), vertex.size=3, vertex.label=NA) 
tkplot(graph.tree(50, 2, mode="undirected"), vertex.size=10, vertex.color="green")
################3

g <- rbind( c(1,2,4), c(1,3,2), c(2,1,4), c(2,3,4), c(2,4,1), c(2,5,2), c(3,1,2), c(3,2,4),c(4,2,1), c(5,2,2), c(5,6,1), c(6,5,1))
g <- as.tnet(g)
#degré pondéré #out degree
degree_w(net=g, measure=c("degree","output","alpha"), alpha=0.5)
#in-degree
degree_w(net=g, measure=c("degree","output","alpha"), alpha=0.5, type="in")
#closeness - booléen
closeness_w(net=g, alpha=0)
#centralité de proximité valuée, alpha = 1
closeness_w(net=g, alpha=1)
#centralité de proximité valuée, alpha= 0.5
closeness_w(net=g, alpha=0.5)
#intermédiarité booléenne
betweenness_w(g, alpha=0)
#intermédiarité valuée
betweenness_w(g, alpha=1)
#intermédiarité valuée
betweenness_w(g, alpha=0.5)

#clustering coefficient global #bi: booléen; am: moyenne arithmétique; gm: moyenne géométrique #ma: maximum method; mi: minimum method
clustering_w(g, measure=c("bi", "am", "gm", "ma", "mi"))
#clustering coefficient local #seulement pour non orienté
clustering_local_w(g, measure=c("bi", "am", "gm", "ma", "mi"))
#rich-club coefficient basé sur le degré #parameter and link-reshuffled random networks #sans intérêt ici étant donnée l’ordre du graphe
out <- weighted_richclub_w(g, rich="k", reshuffle="links", NR=1000, seed=1)
#visualisation
plot(out[,c("x","y")], type="b", log="x", xlim=c(1, max(out[,"x"]+1)), ylim=c(0,max(out[,"y"])+0.5), xaxs = "i", yaxs = "i", ylab="Weighted Rich-club Effect", xlab="Prominence (degree greater than)") lines(out[,"x"], rep(1, nrow(out)))
#distance moyenne
bg <- dichotomise_w(g)
#distance moyenne dans le graphe booléen
mean(distance_w(bg),na.rm=T)
#distance moyenne dans le graphe valué
mean(distance_w(g),na.rm=T)


g2m<- rbind( c(1,1,3), c(2,1,2), c(1,2,2), c(3,2,1), c(3,3,2), c(4,1,2), c(4,3,1))
g2 <- as.tnet(g2m, type="weighted two-mode tnet")
distance_tm(g2) 
clustering_tm(g2) 
clustering_local_tm(g2)

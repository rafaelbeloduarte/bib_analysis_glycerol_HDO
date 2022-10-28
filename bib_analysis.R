# Program to perform bibliometric analysis
# Copyright (C) 2022  Rafael Belo Duarte
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# contact me at rafaelbeloduarte@pm.me

library(bibliometrix)
library(wordcloud2)

# for a shiny UI for bibliometrix package run
# biblioshiny()

# to have better control of the charts generation we run our on commands
# they are documented on the bibliometrix manual: https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
file <- ("scopus_DB_ANDed.csv");

# Import and Convert bibliographic scopus file
M <- convert2df(file, dbsource = "scopus", format = "csv");

# author collaboration network
M_AU <- metaTagExtraction(M, Field = "AU", sep = ";");
NetMatrix_AU <- biblioNetwork(M_AU, analysis = "collaboration", network = "authors", sep = ";");
net_AU=networkPlot(NetMatrix_AU,
                   n = 50,
                   normalize = "association",
                   Title = NULL,
                   type = "fruchterman",
                   size = 10,
                   size.cex = TRUE,
                   remove.multiple=FALSE,
                   label = TRUE,
                   labelsize= 2,
                   label.color = TRUE,
                   cluster = "edge_betweenness",
                   community.repulsion = 0.0001,
                   label.cex = TRUE,
                   remove.isolates = TRUE,
                   edgesize = 1,
                   alpha = 0.7,
                   halo = FALSE);

# authors keywords co-occurrence network
removed_terms = c("1",
                  "article",
                  "catalysis",
                  "catalysts",
                  "glycerol",
                  "catalyst",
                  "effect",
                  "catalytic",
                  "selected",
                  "supported",
                  "selective",
                  "selectivity",
                  "highly",
                  "conversion",
                  "performance",
                  "reaction",
                  "efficient",
                  "study",
                  "glycerol",
                  "biodiesel",
                  "glycerol valorization",
                  "biomass",
                  "heterogeneous catalysis",
                  "synergistic effect",
                  "glycol",
                  "catalytic conversion",
                  "x-ray absorption spectroscopy",
                  "x-ray photoelectron spectroscopy",
                  "promoter",
                  "reduction temperature",
                  "bioglycerol",
                  "pretreatment",
                  "interface"
                  )
synonyms_list=c("1,2-propanediol;1,2-pdo;1 2-pdo;1  2-propanediol;1 2 propanediol;1 2-propylene glycol;1,2-propanediols (1,2-pdo);1,2 propanediol;1, 2-propanediol;propylene-glycol;1 2-propanediol;bio-propylene glycol;propanediol;2 propanediol;propanediol;propylene glycol;1,2-propanediol;1,2-propanediol;2-propanediol;1,2-propylene glycol;propylene glycol",
                "1,3-propanediol;1,3-pdo;1 3-pdo;1  3-propanediol;1,3-propanediols (1,3-pdo);1, 3-propanediol;1 3-propanediol;1 3-propanediol production",
                "zeolite;sba-15;zeolites;hzsm-5 zeolite;y-zeolite",
                "hydrogenolysis;selective hydrogenolysis;glycerol hydrogenolysis;aqueous-phase hydrogenolysis;vapor-phase hydrogenolysis;phase hydrogenolysis;c-o hydrogenolysis;catalytic hydrogenolysis;direct hydrogenolysis;reforming-hydrogenolysis;chemoselective hydrogenolysis;phase glycerol hydrogenolysis;hydrogenolysis of glycerol",
                "hydrodeoxygenation;glycerol hydrodeoxygenation;aqueous-phase hydrodeoxygenation;bifunctional hydrodeoxygenation;phase hydrodeoxygenation",
                "ruthenium;ruthenium catalyst;supported ru;ru catalysts;ru/c;ruthenium catalysts;supported ru catalysts;carbon-supported ru;ru nanoclusters;ru/sio2 catalysts;ru/tio2;ru/tio2 catalyst;sulfided ruthenium catalysts",
                "copper;cu/mgo catalysts;cu/sio2;cu-based catalyst;copper catalysts;metallic copper;cu/sba-15;copper-catalysts;supported copper;copper chromite;supported copper-catalysts;silica-supported copper;copper chromite catalysts;copper ferrites;copper metal-catalysts;copper zirconia catalysts;copper-based catalysts;copper-containing catalysts;promoted copper;cu/sio2 catalysts;cu;cu-zno catalysts;cu/sio2 catalyst;cu/zno catalysts;cu-cr catalysts;cu/mgo;cu/al2o3;cu/zno;cu catalysts;cu-zno catalyst;cu-zno-al2o3 catalyst;cu/zn-based catalysts;cu/zno/al2o3;cu/zno/al2o3 catalysts;cu2o;cucr2o4;cucr2o4 catalysts;cufe2o4;cuo/ceo2 catalysts;ni-cu/al2o3 catalysts;copper catalyst",
                "rhenium;surface rhenium oxide;supported rhenium catalyst;rhenium catalysts;ir-reox/sio2;ir-re catalysts;ir-reox/sio2 catalyst;re catalysts;pt-re;pt-re catalysts;pt-re/al2o3 catalysts;pt-re/zro2 catalysts;re bimetallic catalysts;reox",
                "tungsten oxide;supported tungsten-oxide;tungsten-oxide catalysts;wox/zro2;pt/wox/al2o3;wox domain;pt-wox",
                "pt;platinum;platinum catalyst;carbon-supported platinum;platinum catalysts;platinum particles;pt-h4siw12o40/zro2 catalysts;pt/al2o3;pt-based catalysts;pt-ni;pt-re;pt-re catalysts;pt-re/al2o3 catalysts;pt-re/zro2 catalysts;pt/nb2o5;pt/wo3/zro2;pt/wox/al2o3",
                "ion-exchange;ion-exchange-resin;ion-exchange method",
                "nickel;ni/ceo2 catalyst;nickel;nickel catalyst;nickel-catalysts;ni;ni catalysts;ni2p/sio2;supported nickel-catalysts;modified ni catalysts;ni catalyst;ni(111) surface;ni-cu/al2o3 catalysts;ni/al2o3;ni/al2o3 catalyst;ni/al2o3 catalysts;ni/mgo catalysts;ni3p;pt-ni;supported ni catalysts;supported nickel",
                "mechanism;catalytic mechanism;reaction mechanisms;mechanism",
                "aqueous phase reforming;apr",
                "catalyst deactivation;deactivation;catalyst stability",
                "bimetallic;bimetallic catalyst",
                "propylene; propene",
                "kinetics;kinetic study",
                "palladium;pd promoter",
                "acetol;1-Hydroxy-2-propanone;hydroxyacetone",
                "tio2;titania;titanium dioxide",
                "support;supported catalysts;support effect",
                "bifunctional catalysis;bifunctional catalyst;bifunctional catalyst"
                );
NetMatrix_kw <- biblioNetwork(M, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";",
                              synonyms = synonyms_list,
                              remove.terms = removed_terms);
net_kw=networkPlot(NetMatrix_kw,
                   n = 50,
                   normalize = "association",
                   Title = NULL,
                   type = "auto",
                   size = 3,
                   size.cex = FALSE,
                   remove.multiple=FALSE,
                   labelsize= 1,
                   label.cex = FALSE,
                   cluster = "walktrap",
                   community.repulsion = 0.05,
                   remove.isolates = TRUE,
                   edgesize = 10,
                   alpha = 0.50,
                   halo = FALSE);
net2VOSviewer(net_kw);

# word cloud
Tab <- tableTag(M, Tag = "DE", sep = ";",
                synonyms = synonyms_list, remove.terms = removed_terms);
Tab <- log(Tab);
wordcloud2(Tab, size = 0.3, minSize = 0, gridSize =  10,
           fontFamily = 'Liberation Serif', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = FALSE,
           rotateRatio = 0, shape = 'circle', ellipticity = 0.8,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL);

# country collaboration network
M_CO <- metaTagExtraction(M, Field = "AU_CO", sep = ";");
NetMatrix_CO <- biblioNetwork(M_CO, analysis = "collaboration", network = "countries", sep = ";");
net_CO=networkPlot(NetMatrix_CO,
                n = dim(NetMatrix_CO)[1],
                normalize = "association",
                Title = NULL,
                type = "fruchterman",
                size = 2,
                size.cex = TRUE,
                remove.multiple=FALSE,
                labelsize=2,
                cluster = "edge_betweenness",
                community.repulsion = 0.01,
                label.cex = TRUE,
                remove.isolates = TRUE,
                edgesize = 5,
                alpha = 0.7);
net2VOSviewer(net_CO);

# collaboration network between institutions
M_AFF <- metaTagExtraction(M, Field = "AU1_UN", sep = ";");
NetMatrix_AFF <- biblioNetwork(M_AFF, analysis = "collaboration", network = "universities", sep = ";");
net_AFF=networkPlot(NetMatrix_AFF,
                n = 30,
                normalize = "association",
                Title = NULL,
                type = "auto",
                size = 10,
                size.cex = TRUE,
                remove.multiple=FALSE,
                labelsize=2,
                cluster = "edge_betweenness",
                community.repulsion = 0.01,
                label.cex = TRUE,
                remove.isolates = FALSE,
                edgesize = 20,
                alpha = 0.7);

# historical citation network
histResults <- histNetwork(M, sep = ";")
# Plot a historical co-citation network
net <- histPlot(histResults, size = 10)

# reference publication year spectrocopy
rpys(M, sep = ";", timespan = c(1950,2022), graph = T)

# historiograph
histResults <- histNetwork(M, min.citations = 1, sep = ";", network = TRUE, verbose = TRUE);
histPlot(
  histResults,
  n = 20,
  size = 5,
  labelsize = 5,
  title_as_label = FALSE,
  verbose = TRUE
);

# co-citation analysis
NetMatrix_coc <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";");
net_coc=networkPlot(NetMatrix_coc, n = 30, Title = "Co-Citation Network", type = "fruchterman", 
                    size=10, size.cex = TRUE,
                remove.multiple=FALSE, labelsize=0.7,edgesize = 5, label.cex = TRUE, community.repulsion = 0.01);

# bib coupling
NetMatrix_bib_coup <- biblioNetwork (M, analysis = "coupling", network = "references", sep = ";");
net_coc=networkPlot(NetMatrix_bib_coup, n = 50, Title = "Bibliographic coupling", type = "fruchterman", 
                    size=10, size.cex = TRUE,
                    remove.multiple=FALSE, labelsize=2,edgesize = 1, label.cex = TRUE, community.repulsion = 0.01);

# conceptual structure
CS <- conceptualStructure(M, field="DE", method="CA", 
                          stemming=TRUE, minDegree=10, k.max = 5, synonyms = synonyms_list);

years=c(2005,2010,2015)
nexus <- thematicEvolution(M,field="DE", years=years, n=100,minFreq=2)
plotThematicEvolution(nexus$Nodes,nexus$Edges)


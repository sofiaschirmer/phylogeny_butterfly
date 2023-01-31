# Manuscript Schirmer et al. 2022 - Phylogeny
library(readxl)
library(ape)
library(phytools)
#data from Chazot et al. (2019)
butterfly_new <- read.nexus("S_3b_core_analysis_median_ages.tre")
summary(butterfly_new)
is.ultrametric(butterfly_new)
is.rooted(butterfly_new)
butterfly_new$tip.label
species <- c("KK_NCS2556_FG_Heliconius_erato","NW66_5_Morpho_helenor","NW70_10_Caligo_telamonius","NW70_10_Caligo_telamonius",
             "KK_8700_Pe_Philaethria_dido","DHJ_02_2492_Parides_iphidamas","PM14_20_Pierella_lamia", 
             "NW106_1_Fountainea_ryphea","NW122_21_Brassolis_sophorae","LEP_00435_Taygetina_banghaasi", "PM06_12_Opsiphanes_cassina", "NS0358_Nymphidium_cf_olinda",
             "NS0420_Stalachtis_calliope", "DW_92_Z084_Eurema_mexicana",
             "MFB_00_P197_Phoebis_sennae", "NW68_5_Anartia_amathea", "435_ADW_Helias_phalaenoides","NW152_12_Marpesia_eleuchea","NW107_16_Adelpha_californica","NW108_3_Taygetis_virgilia", "NW81_9_Archaeoprepona_demophon",
             "Calycopis_caulonia","NW62_3_Hamadryas_februa", "NW63_21_Vanessa_atalanta", "PM14_20_Pierella_lamia", "21470_Ithomia_nsp",
             "NW107_16_Adelpha_californica", "KK_NCS2556_FG_Heliconius_erato", "NS0420_Stalachtis_calliope",
             "NW85_11_Nica_flavilla", "DHJ_02_2492_Parides_iphidamas","NW152_19_Protesilaus_zonarius", "NW127_3_Mechanitis_lysimnia",
             "NW106_1_Fountainea_ryphea","MFB_00_P277_Itaballia_demophile","DHJ_02_2492_Parides_iphidamas","DHJ_02_2492_Parides_iphidamas",
             "CP06_93_Hermeuptychia_harmonia","CP12_06_Pharneuptychia_innocentia","NW127_11_Hypna_clytemnestra","NW106_1_Fountainea_ryphea", "bb19_Eunica_bechina",
             "MFB_00_P211_Pyrisitia_proterpia","NW122_19_Lycorea_halia","NS0108_Melanis_aegates","NW115_3_Pyrrhogyra_crameri")
species
bad.species <- setdiff(butterfly_new$tip.label, species)
backbone <- drop.tip(butterfly_new, tip = bad.species)
backbone
backbone$tip.label
plot(backbone)
backbone_supermatrix <- drop.tip(butterfly_new, tip = bad.species)
backbone_supermatrix
backbone_supermatrix$tip.label
plot(backbone_supermatrix)
plot(backbone_supermatrix, show.node.label = TRUE, show.tip.label = TRUE, font = 1, cex=0.4, root.edge= TRUE, use.edge.length = TRUE)
write.tree(backbone_supermatrix, file = "backbone_supermatrix.tre")
write.nexus(backbone_supermatrix, file="backbone_supermatrix.nex")
is.binary(backbone)
backbone.binary <- multi2di(backbone)
is.binary(backbone)
plot(backbone.binary, show.node.label = TRUE, show.tip.label = TRUE, font = 1, cex=0.4, root.edge= TRUE, use.edge.length = TRUE)
write.tree(backbone.binary, file = "backbone_binary.tre")
write.nexus(backbone.binary, file="backbone_binary.nex")
backbone_NOBL <- backbone_supermatrix
head(backbone_NOBL)
branch_lenghts <- backbone_NOBL$edge.length
write.tree(backbone_NOBL, file = "backbone_NOBL.tre")
nodes<-sapply(backbone_NOBL$tip.label,function(x,y) which(y==x),y=backbone_NOBL$tip.label)
edge.lengths<-setNames(backbone_NOBL$edge.length[sapply(nodes,                                                     function(x,y) which(y==x),y=tree$edge[,2])],names(nodes))
phy <- data.frame(edge.lengths)
plotTree(backbone_NOBL)
edgelabels(round(backbone_NOBL$edge.length,3),cex=0.7)
tree <- read.tree("backbone_NOBL.tre")
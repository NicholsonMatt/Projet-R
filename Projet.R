rm(list=objects());graphics.off()

music = read.table("Music_2026.txt", header=TRUE, sep=";", dec=".")
p = ncol(music)
n = nrow(music)

#Q1

summary(music)

table(music$GENRE) / n

library(ggplot2)
ggplot(music) + geom_boxplot(aes(x=PAR_SC_V))
ggplot(music) + geom_boxplot(aes(x=PAR_ASC_V))

music$PAR_SC_V = log(music$PAR_SC_V)
music$PAR_ASC_V = log(music$PAR_ASC_V)

music = music[, -(148:167)]
p = ncol(music)

C = cor(music[, -p])
high_cor = which(C > 0.99 & C < 1, arr.ind=TRUE)
pairs_idx = high_cor[high_cor[, 1] < high_cor[, 2], ]
data.frame(Var1=rownames(C)[pairs_idx[, 1]], Var2=colnames(C)[pairs_idx[, 2]])

#Q2

library(FactoMineR)

res_pca = PCA(music, quali.sup=p, graph=FALSE)

plot.PCA(res_pca, axes=c(1, 2), choix="ind", habillage=p)
plot.PCA(res_pca, axes=c(2, 3), choix="ind", habillage=p)


#Q3
library(cluster)

X = music[, -p]
d = dist(X)
cah = hclust(d, method="ward.D2")
k_genres = length(unique(music$GENRE))
groupes_cah = cutree(cah, k=k_genres)

sil_cah = silhouette(groupes_cah, d)
plot(sil_cah, border=NA)

genre_num = as.numeric(as.factor(music$GENRE))
sil_genre = silhouette(genre_num, d)
plot(sil_genre, border=NA)

X_norm = scale(X)
d_norm = dist(X_norm)

cah_norm = hclust(d_norm, method="ward.D2")
groupes_norm = cutree(cah_norm, k=k_genres)

sil_norm = silhouette(groupes_norm, d_norm)
plot(sil_norm, border=NA)

set.seed(103)
train = sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(2/3, 1/3))

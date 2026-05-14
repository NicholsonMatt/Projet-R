rm(list=objects());graphics.off()

music = read.table("Music_2026.txt", header=TRUE, sep=";", dec=".")
p = ncol(music)
n = nrow(music)

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
rm(list=objects());graphics.off()

music = read.table("Music_2026.txt", header=TRUE, sep=";", dec=".")
p = ncol(music)
n = nrow(music)

#Partie 1

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

#Q4
set.seed(103)
train = sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(2/3, 1/3))

#Partie 2
#Q1

train_bin = music[train,]
train_bin = train_bin[train_bin$GENRE == "Classical" | train_bin$GENRE == "Jazz",]
test_bin = music[!train,]
test_bin = test_bin[test_bin$GENRE == "Classical" | test_bin$GENRE == "Jazz",]


train_bin$GENRE = factor(train_bin$GENRE)
test_bin$GENRE = factor(test_bin$GENRE)

nrow(train_bin)
nrow(test_bin)

ModT = glm(GENRE~., family=binomial, data=train_bin)
summary(ModT)

trouver_variables_significatives = function(modele, seuil) {
  p_valeurs = summary(modele)$coefficients[-1, 4]
  variables = names(p_valeurs)[p_valeurs < seuil]
  formule = as.formula(paste("GENRE ~", paste(variables, collapse=" + ")))
  return(formule)
}



Mod1 = glm(trouver_variables_significatives(ModT, 0.05), family=binomial, data=train_bin)
summary(Mod1)
plot(Mod1)

Mod2 = glm(trouver_variables_significatives(ModT, 0.20), family=binomial, data=train_bin)
plot(Mod2)

library(MASS)
ModAIC = stepAIC(ModT)

#Q2
library(ROCR)

predproba_train_ModT = predict(ModT, type="response")
pred_train_ModT = prediction(predproba_train_ModT, train_bin$GENRE)
ROC_train_ModT = performance(pred_train_ModT, "sens", "fpr")
AUC_train_ModT = round(unlist(performance(pred_train_ModT, "auc")@y.values), 4)

predproba_test_ModT = predict(ModT, newdata=test_bin, type="response")
pred_test_ModT = prediction(predproba_test_ModT, test_bin$GENRE)
ROC_test_ModT = performance(pred_test_ModT, "sens", "fpr")
AUC_test_ModT = round(unlist(performance(pred_test_ModT, "auc")@y.values), 4)

predproba_test_Mod1 = predict(Mod1, newdata=test_bin, type="response")
pred_test_Mod1 = prediction(predproba_test_Mod1, test_bin$GENRE)
ROC_test_Mod1 = performance(pred_test_Mod1, "sens", "fpr")
AUC_test_Mod1 = round(unlist(performance(pred_test_Mod1, "auc")@y.values), 4)

predproba_test_Mod2 = predict(Mod2, newdata=test_bin, type="response")
pred_test_Mod2 = prediction(predproba_test_Mod2, test_bin$GENRE)
ROC_test_Mod2 = performance(pred_test_Mod2, "sens", "fpr")
AUC_test_Mod2 = round(unlist(performance(pred_test_Mod2, "auc")@y.values), 4)

predproba_test_ModAIC = predict(ModAIC, newdata=test_bin, type="response")
pred_test_ModAIC = prediction(predproba_test_ModAIC, test_bin$GENRE)
ROC_test_ModAIC = performance(pred_test_ModAIC, "sens", "fpr")
AUC_test_ModAIC = round(unlist(performance(pred_test_ModAIC, "auc")@y.values), 4)

plot(ROC_train_ModT, col=1, main="Courbes ROC")
plot(ROC_test_ModT, col=2, add=TRUE)
plot(ROC_test_Mod1, col=3, add=TRUE)
plot(ROC_test_Mod2, col=4, add=TRUE)
plot(ROC_test_ModAIC, col=5, add=TRUE)

legend("bottomright",
       legend=c(
         paste("ModT Train AUC: ", AUC_train_ModT),
         paste("ModT Test AUC: ", AUC_test_ModT),
         paste("Mod1 Test AUC: ", AUC_test_Mod1),
         paste("Mod2 Test AUC: ", AUC_test_Mod2),
         paste("ModAIC Test AUC: ", AUC_test_ModAIC)
       ),
       col=c(1, 2, 3, 4, 5),
       lty=c(1, 1, 1, 1, 1),
       cex=0.8)

anova(ModAIC, ModT, test="Chisq")

#Q3
library(glmnet)

x_train = as.matrix(train_bin[, -ncol(train_bin)])
y_train = train_bin$GENRE

grid = 10^seq(10, -2, length=100)

ridge.fit = glmnet(x_train, y_train, alpha=0, lambda=grid, family="binomial")

plot(ridge.fit)

#Q4
set.seed(103)
cv.out = cv.glmnet(x_train, y_train, alpha=0, nfolds=10, lambda=grid, family="binomial")

plot(cv.out)

bestlam = cv.out$lambda.min
bestlam

x_test = as.matrix(test_bin[, -ncol(test_bin)])
y_test = test_bin$GENRE

ridge.pred = predict(ridge.fit, s=bestlam, newx=x_test, type="response")

pred_test_ridge = prediction(ridge.pred, y_test)
AUC_test_ridge = round(unlist(performance(pred_test_ridge, "auc")@y.values), 4)
AUC_test_ridge

#Q5
ROC_test_ridge = performance(pred_test_ridge, "sens", "fpr")

plot(ROC_train_ModT, col=1, main="Courbes ROC")
plot(ROC_test_ModT, col=2, add=TRUE)
plot(ROC_test_Mod1, col=3, add=TRUE)
plot(ROC_test_Mod2, col=4, add=TRUE)
plot(ROC_test_ModAIC, col=5, add=TRUE)
plot(ROC_test_ridge, col=6, add=TRUE)

legend("bottomright",
       legend=c(
         paste("ModT Train AUC: ", AUC_train_ModT),
         paste("ModT Test AUC: ", AUC_test_ModT),
         paste("Mod1 Test AUC: ", AUC_test_Mod1),
         paste("Mod2 Test AUC: ", AUC_test_Mod2),
         paste("ModAIC Test AUC: ", AUC_test_ModAIC),
         paste("Ridge Test AUC: ", AUC_test_ridge)
       ),
       col=c(1, 2, 3, 4, 5, 6),
       lty=c(1, 1, 1, 1, 1, 1),
       cex=0.8)

#Q6
recap = data.frame(
  Modele = c("ModT", "Mod1", "Mod2", "ModAIC", "Ridge"),
  AUC_Test = c(AUC_test_ModT, AUC_test_Mod1, AUC_test_Mod2, AUC_test_ModAIC, AUC_test_ridge)
)

print(recap)

music_final_test = read.table("Music_test_2026.txt", header=TRUE, sep=";", dec=".")

music_final_test$PAR_SC_V = log(music_final_test$PAR_SC_V)
music_final_test$PAR_ASC_V = log(music_final_test$PAR_ASC_V)
music_final_test = music_final_test[, -(148:167)]

predproba_final = predict(ModAIC, newdata=music_final_test, type="response")
pred_genres_final = ifelse(predproba_final > 0.5, "Jazz", "Classical")

write.table(pred_genres_final, file="PANAZZOLO-NICHOLSON_test.txt")

#Partie 3
#Q4
library(nnet)

train_multi = music[train, ]
test_multi = music[!train, ]

train_multi$GENRE = factor(train_multi$GENRE)
test_multi$GENRE = factor(test_multi$GENRE)

ModMulti = multinom(GENRE ~ ., data=train_multi)

pred_train_multi = predict(ModMulti, train_multi)
erreur_app = mean(pred_train_multi != train_multi$GENRE)

pred_test_multi = predict(ModMulti, newdata=test_multi)
erreur_test = mean(pred_test_multi != test_multi$GENRE)

#Q5
ModNnet = nnet(GENRE ~ ., data=train_multi, size=0, skip=TRUE, trace=FALSE)

pred_train_nnet = predict(ModNnet, type="class")
erreur_app_nnet = mean(pred_train_nnet != train_multi$GENRE)

pred_test_nnet = predict(ModNnet, newdata=test_multi, type="class")
erreur_test_nnet = mean(pred_test_nnet != test_multi$GENRE)

#Q6
probs_test = predict(ModMulti, newdata=test_multi, type="probs")
niveaux_genres = levels(test_multi$GENRE)

y_bin_1 = as.numeric(test_multi$GENRE == niveaux_genres[1])
pred_1 = prediction(probs_test[, 1], y_bin_1)
roc_1 = performance(pred_1, "sens", "fpr")
plot(roc_1, col=1)

y_bin_2 = as.numeric(test_multi$GENRE == niveaux_genres[2])
pred_2 = prediction(probs_test[, 2], y_bin_2)
roc_2 = performance(pred_2, "sens", "fpr")
plot(roc_2, col=2, add=TRUE)

y_bin_3 = as.numeric(test_multi$GENRE == niveaux_genres[3])
pred_3 = prediction(probs_test[, 3], y_bin_3)
roc_3 = performance(pred_3, "sens", "fpr")
plot(roc_3, col=3, add=TRUE)

y_bin_4 = as.numeric(test_multi$GENRE == niveaux_genres[4])
pred_4 = prediction(probs_test[, 4], y_bin_4)
roc_4 = performance(pred_4, "sens", "fpr")
plot(roc_4, col=4, add=TRUE)

y_bin_5 = as.numeric(test_multi$GENRE == niveaux_genres[5])
pred_5 = prediction(probs_test[, 5], y_bin_5)
roc_5 = performance(pred_5, "sens", "fpr")
plot(roc_5, col=5, add=TRUE)

legend("bottomright", legend=niveaux_genres, col=1:5, lty=1, cex=0.8)


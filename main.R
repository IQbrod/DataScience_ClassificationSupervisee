# ANALYSE NON SUPERVISEE

# 1 Interpreter les résultats
spam7 = read.table(file="spam7.txt")
attributes(spam7)$names
summary(spam7)
spam.sample <- spam7[sample(seq(1,4601),500,replace=FALSE), ]
par(mfrow=c(2,3))
boxplot(split(spam.sample$crl.tot,spam.sample$yesno), main="crl.tot")
boxplot(split(spam.sample$dollar,spam.sample$yesno), main="dollar")
boxplot(split(spam.sample$bang,spam.sample$yesno), main="bang")
boxplot(split(spam.sample$money,spam.sample$yesno), main="money")
boxplot(split(spam.sample$n000,spam.sample$yesno), main="n000")
boxplot(split(spam.sample$make,spam.sample$yesno), main="make")

### KMEANS ###
spam7[1:6]<-scale(spam7[1:6])
# Attributs utilisés pour le Kmeans
attributes(spam7[1:6])$name
# On pose k=2 pour obtenir les emails spam et non spam
# De plus on ne traite pas la donnée yesno => Puisqu'on cherche à la deviner
spam.km<-kmeans(spam7[1:6],2)

# Taille des clusters obtenus
spam.km$size
#Taille des clusters attendus
length(spam7$yesno[spam7$yesno == 'y'])
length(spam7$yesno[spam7$yesno == 'n'])
# Donc KMeans avec les données fournies ne semble pas révèler des clusters perninents

### HCLUST ###
# Calcul du dendogramme
D <- dist(spam7[1:6])
res <- hclust(D,method="ward.D")
plot(res,hang = -1)

res2 <- cutree(res, k=2)
# Taille des clusters obtenus
res3 <- as.matrix(res2)
length(res3[res3[,1] == 1])
length(res3[res3[,1] == 2])
#Taille des clusters attendus
length(spam7$yesno[spam7$yesno == 'y'])
length(spam7$yesno[spam7$yesno == 'n'])
# Donc hclust est plus proche des resultats attendus mais reste inpertinent


### PAM ###
library(cluster)
res <- pam(D,2)
# Taille des clusters obtenus
res$clusinfo[,1]
#Taille des clusters attendus
length(spam7$yesno[spam7$yesno == 'y'])
length(spam7$yesno[spam7$yesno == 'n'])

plot(res)
# Finalement, on se rend rapidement compte que les distances sont biaisées par des
# attributs qui ne sont pas révèlateurs de la propriété recherchée (spam ou non).


### ANALYSE SUPERVISEE ###
library(rpart)
# On crée un jeu de donnée
S <- read.table(file="spam7.txt")
# On séléctionne 80% de sous ensemble du dataset en conservant les proportions des spam / non spam (par id)
sub <- c(sample(1:max(which(S$yesno=="y")), round(0.8*max(which(S$yesno=="y")),digits=0)),sample((max(which(S$yesno=="y"))+1):4601,round(0.8*length(which(S$yesno=="n")),digits=0)))
# On les récupère (en entier) dans l'ensemble fit
fit <- rpart(S$yesno~ ., data=S, subset=sub)
# On trace les propriétés du jeu d'apprentissage
plot(fit)
text(fit)
# On apprend avec le jeu d'apprentissage et on essaye de prédire pour le jeu restant (20%)
err <- table(predict(fit, S[-sub,], type="class"), S[-sub, "yesno"])
# On peut observer la totalité des règles en observant le fit
fit
# Calcul de l'erreur
round((err[1,2]+err[2,1])/sum(err)*100,digits=2)
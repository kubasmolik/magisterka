#########################################################################################################
####                                  BADANIE ZLOZONOSCI ZBIORU                                      ####
#########################################################################################################

#load("./dane/dane_preproc.RData")

# budowa i wizualizacja macierzy korelacji predyktorow
cormat <- cor(select(train, one_of(zm_numeric), ends_with("woe")))
corrplot::corrplot(cormat, method = "circle", type = "full", order = "hclust", tl.col = "black")

# skalowanie i centrowanie zmiennych
pre_proc_train <- preProcess(train, method = c("center","scale"))
train <- predict(pre_proc_train, train)

# tworzenie listy z wyskalowanymi woe
lista_woe_scaled <- list()
i <- 1
for(x in zm_factor){
   x2 <- paste(x, "woe", sep = "_")
   temp <- train %>% select(., starts_with(x)) %>% group_by(.data[[x]], .data[[x2]]) %>% count() %>% 
      ungroup() %>% select(., x,x2)
   temp$x2 <- as.numeric(temp$x2)
   names(temp) <- c(x,x2)
   lista_woe_scaled[[i]] <- temp
   i <- i + 1
}

names(lista_woe_scaled) <- zm_factor
rm(i, temp, x2)

# przygotowanie zbiorow pomocniczych do analizay glownych skladowych
train_num <- train[,names(train) %in% zm_numeric]
for(i in 1:ncol(train_num)){
   train_num[[i]] <- as.numeric(train_num[[i]])
}
train_fac <- data.frame(train[,names(train) %in% zm_factor])

#### ANALIZA GLOWNYCH SKLADOWYCH ####
library(PCAmixdata)
set.seed(1993)
pcamix_train <- PCAmix(X.quanti = as.matrix(train_num),
                       X.quali = train_fac,
                       rename.level = T,
                       ndim = 15)

opis_pca <- data.frame(pcamix_train$eig)
opis_pca$pca <- stringr::str_replace(rownames(pcamix_train$eig), pattern = "dim ", replacement = "")

# wizualizacja PCA
kolor <- RColorBrewer::brewer.pal(n = 3, name = "Set1")[2]
p <- ggplot(opis_pca, 
            aes(x = forcats::fct_reorder(forcats::as_factor(pca), Cumulative), y = Cumulative)) + 
   geom_col(fill = kolor) + 
   geom_vline(aes(xintercept = 13.5), color = "green", size = 1.5, linetype = 2) +
   geom_hline(aes(yintercept = 70), color = "red", size = 1.5, linetype = 2) + 
   labs(x = "Glowne skladowe",
        y = "Skumulowany % odwzorowanej wariancji",
        title = "Skumulowany procent odwzorowanej wariancji dla kolejnych glownych skladowych") +
   annotate(geom = "text", x = 3, y = 75, label = "70% wariancji", color = "red") + 
   theme_bw()

p

# glowne skladowe a zmienna objasniana
train_pca <- predict(pcamix_train, X.quanti = as.matrix(train_num), X.quali = train_fac, rename.level = T)
train_pca <- data.frame(train_pca)
train_pca <- bind_cols(select(train, default), train_pca)

set.seed(1993)
probka <- runif(nrow(train_pca))
p <- ggplot(train_pca[probka <= 0.1,], aes(x = dim1, y = dim2, fill = default, col = default))
p + geom_point(alpha = 0.2) +
   labs(
      title = "Zmienna objasniana vs dwie pierwsze skladowe",
      subtitle = "Probka 10% obserwacji",
      x = "Pierwsza glowna skladowa",
      y = "Druga glowna skladowa") +
   theme_bw()

#### KLASTROWANIE ZMIENNYCH ####
library(ClustOfVar)
set.seed(1993)
drzewo_zmiennych <- hclustvar(X.quali = train_fac,
                              X.quanti = as.matrix(train_num))

plot(drzewo_zmiennych)
summary(drzewo_zmiennych)

plot(drzewo_zmiennych, main = "Klastrowanie zmiennych - dendrogram", ylab = "")
rect.hclust(drzewo_zmiennych, k=10,  border = 1:10)

# podzial na 10 klastrow
drzewo_zm_cut <- cutreevar(drzewo_zmiennych, k = 10)
print(drzewo_zm_cut)
drzewo_zm_cut$var
klastry <- drzewo_zm_cut$cluster

# pomocniczy data frame z lista zmiennych
lista_zmiennych <- data.frame(
   zmienna = names(klastry),
   klaster = klastry,
   stringsAsFactors = F)

#### SZACOWANIE MOCY PREDYKCYJNEJ ZMIENNYCH ####

# pomocniczy zbior - metoda undersampling
#library(unbalanced)
prop.table(table(train$default))
temp <- train[,1]
levels(temp$default) <- c("1","0")
set.seed(1993)
bal <- unbalanced::ubUnder(X = train[,-1], Y = temp$default, perc = 50)
train_under <- bind_cols(data.frame(default = bal$Y), bal$X)
table(train_under$default)
rm(bal, temp)

# f. pomocnicza do oceny mocy predykcyjnej 
library(ROCR)
moc_predykcyjna <- function(zbior, x){
   
   df <- zbior %>% select(., default, .data[[x]])
   names(df)[2] <- "zmienna"
   
   if(is.factor(df$zmienna) == T){
      model <- glm(default ~ zmienna, data = df, family = "binomial")
   } else{
      model <- glm(default ~ zmienna + I(zmienna^2), data = df, family = "binomial")
   }
   
   prog <- predict(model, newdata = df, type = "response")
   pred <- prediction(prog, df$default, label.ordering = levels(df$default))
   auc <- attr(performance(pred,"auc"),"y.values")[[1]]
   
   return(auc)
}

# moc predykcyjna zmiennych
moc_pred <- c()
for(x in append(zm_factor, zm_numeric)){
   moc_pred <- append(moc_pred, moc_predykcyjna(train_under, x))
}

temp <- data.frame(zmienna = append(zm_factor, zm_numeric),
                   AUC = moc_pred,
                   stringsAsFactors = F)
lista_zmiennych <- lista_zmiennych %>% left_join(temp)
rm(temp)

# wizualizacja mocy predykcyjnej zmiennych
p <- ggplot(lista_zmiennych, 
            aes(x = forcats::fct_reorder(forcats::as_factor(zmienna), AUC), 
                y = AUC, 
                fill = forcats::as_factor(as.character(klaster))))

p + geom_col() + coord_flip(ylim=c(0.5, 0.6)) + 
   scale_fill_manual(name = "Klaster", values = RColorBrewer::brewer.pal(n = 10, name = "Paired")) +
   labs(
      title = "Moc predykcyjna zmiennych",
      x = "Zmienne",
      y = "AUC"
   ) + 
   geom_hline(aes(yintercept = 0.525), color = "black", size = 1.5, linetype = 2) +
   theme_bw()
   
# porzadkowanie srodowiska
save(list = c("drzewo_zmiennych","drzewo_zm_cut"), file = "./dane/klastry_zmiennych.RData")
save(list = c("pcamix_train", "opis_pca"), file = "./dane/PCA.RData")

rm(opis_pca, temp, train_fac, train_num, train_pca, train_under, bal, drzewo_zm_cut, i, zmienna,
   drzewo_zmiennych, klastry, pcamix_train, moc_pred, p, stab, probka, x, pre_proc_train, cormat)

# save(list = ls(), file = "./dane/dane_modelowanie.RData")

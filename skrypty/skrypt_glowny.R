#-------------------------------------------------------------------------------------------------------#
####                    ZASTOSOWANIA METOD DATA-MINING W SCORINGU KREDYTOWYM                         ####
####                                         SKRYPT GLOWNY                                           ####
####                                         JAKUB SMOLIK                                            ####
####                                          09.11.2017                                             ####
#-------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------#
####                                   PRZYGOTOWANIE SRODOWISKA                                      ####
#-------------------------------------------------------------------------------------------------------#

source("C:/Users/Kuba/Documents/magisterka/skrypty/przygotowanie_srodowiska.R")

#-------------------------------------------------------------------------------------------------------#
####                                      WCZYTANIE DANYCH                                           ####
#-------------------------------------------------------------------------------------------------------#

source("./skrypty/wczyt_danych.R")
#load("./dane/dane.RData")

#-------------------------------------------------------------------------------------------------------#
####                 KONSTRUKCJA ZMIENNEJ OBJASNIANEJ I WYBOR ZMIENNYCH DO ANALIZY                   ####
#-------------------------------------------------------------------------------------------------------#

# konstrukcja zmiennej objasnianej default
dane %>% group_by(loan_status) %>% count()
bad_status <- c("Charged Off","Default","Late (31-120 days)")
dane <- dane %>% mutate(default = if_else(loan_status %in% bad_status,1,0))
dane$default <- factor(dane$default, levels = c("0","1"), labels = c("no_default","default"))

#save(list = ls(), file = "./dane/dane.Rdata")

zmienne_do_analizy <- c(
"id",
"member_id",
"default",
"loan_amnt",
"term",
"installment",
"emp_title",
"emp_length",
"home_ownership",
"annual_inc",
"verification_status",
"issue_d",
"desc",
"purpose",
"title",
"addr_state",
"dti",
"delinq_2yrs",
"earliest_cr_line",
"inq_last_6mths",
"mths_since_last_delinq",
"open_acc",
"pub_rec",
"revol_bal",
"revol_util",
"total_acc",
"initial_list_status",
"tot_cur_bal"
)

dane <- dane %>% select(. , one_of(zmienne_do_analizy))

rm(zmienne_do_analizy)

#-------------------------------------------------------------------------------------------------------#
####                                   TWORZENIE NOWYCH ZMIENNYCH                                    ####
#-------------------------------------------------------------------------------------------------------#

source("./skrypty/nowe_zmienne.R")
#save(list = ls() ,file = "./dane/dane_wej.Rdata")

#-------------------------------------------------------------------------------------------------------#
####                                  UZUPELNIANIE BRAKOW DANYCH                                     ####
#-------------------------------------------------------------------------------------------------------#

#load("./dane/dane_wej.RData")
source("./skrypty/braki_danych.R")

#-------------------------------------------------------------------------------------------------------#
####                             PODZIAL NA ZBIOR TRENINGOWY I TESTOWY                               ####
#-------------------------------------------------------------------------------------------------------#

dane$addr_state <- as.factor(dane$addr_state)

set.seed(1993)
train_ind <- createDataPartition(y = dane$default, times = 1, p = 0.7, list = F)

train <- dane[train_ind,]
test <- dane[-train_ind,]

save(train, file = "./dane/train.RData")
save(test, file = "./dane/test.RData")

rm(dane, train_ind)

#-------------------------------------------------------------------------------------------------------#
####                                   ANALIZA ZBIORU DANYCH                                         ####
#-------------------------------------------------------------------------------------------------------#

source("./skrypty/analiza_zb_danych.R")

#-------------------------------------------------------------------------------------------------------#
####                               PRZYGOTOWANIE ZBIORU TESTOWEGO                                    ####
#-------------------------------------------------------------------------------------------------------#

source("./skrypty/przygotowanie_test.R")

#save(list = ls(), file = "./dane/dane_preproc.RData")

#-------------------------------------------------------------------------------------------------------#
####                                  BADANIE ZLOZONOSCI ZBIORU                                      ####
#-------------------------------------------------------------------------------------------------------#

source("./skrypty/analiza_zb_2.R")

#-------------------------------------------------------------------------------------------------------#
####                                  CZYSZCZENIE I PROBKOWANIE                                      ####
#-------------------------------------------------------------------------------------------------------#

source("./skrypty/czyszcz_prob.R")

#-------------------------------------------------------------------------------------------------------#
####                            PRZYGOTOWANIE SRODOWISKA DO MODELOWANIA                              ####
#-------------------------------------------------------------------------------------------------------#

kolor <- RColorBrewer::brewer.pal(n = 3, name = "Set1")[2]

#load("./dane/dane_modelowanie.RData")

# wczytanie bibliotek
library(caret)
library(MLmetrics)
library(parallel)
library(foreach)
library(doParallel)
library(pROC)
library(PRROC)
library(forcats)
#library(caretEnsemble)

# centrowanie i skalowanie zbioru testowego
pre_proc_test <- preProcess(test, method = c("center","scale"))
test <- predict(pre_proc_test, test)

# nadpisanie zmiennych typu WOE w zbiorze testowym - musza miec te same wartosci co w zbiorze train
test <- test %>% select(., - ends_with("woe"))

# dolaczenie zmiennych typu woe do zbioru testowego
for(i in names(lista_woe_scaled)){
   test$temp <- as.character(test[[i]])
   test <- test %>% left_join(., lista_woe_scaled[[i]], by = c("temp" = paste(i)))
   test$temp <- NULL
}
sum(is.na(test))

test <- test %>% select(., one_of(names(train_clean)))

# generowanie bootstrapowych prob testowych
set.seed(1993)
probka <- runif(nrow(test)) <= 0.7
set.seed(1993)
temp <- createResample(test$default, times = 50)
wez_pol <- function(x, pol){ return(x[pol])}
bootstrap <- lapply(temp, wez_pol, pol = probka)

rm(temp)

# funkcje pomocnicze do oceny modeli
source("./skrypty/funkcje/f_pomoc_modelowanie.R")

# zmienne do modeli pojedynczych - benchmark AUC zmiennej >= 0.525 (GINI >= 5%)
zmienne_model_poj <- lista_zmiennych$zmienna[lista_zmiennych$AUC >= 0.525]
zmienne_model_poj
zmienne_model_poj_woe <- zmienne_model_poj
zmienne_model_poj_woe[zmienne_model_poj %in% names(lista_woe)] <- 
   paste(zmienne_model_poj[zmienne_model_poj %in% names(lista_woe)], "woe", sep = "_")

# funkcja kontrolna do modelowania
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = custom_summary,
                     search = "grid",
                     classProbs = T,
                     verboseIter = T,
                     sampling = "up",
                     savePredictions = "final",
                     allowParallel = T)

#-------------------------------------------------------------------------------------------------------#
####                                       MODELE POJEDYNCZE                                         ####
#-------------------------------------------------------------------------------------------------------#

#### MODEL LOGITOWY ZE ZMIENNYMI WOE - logit ####
source("./skrypty/modele/logit.R")

#### ELASTIC NET  - enet_full ####
#source("./skrypty/modele/enet_full.R")

#### LAS LOSOWY - las_losowy ####
source("./skrypty/modele/las_losowy.R")

#### XGBOOST - xgboost ####
source("./skrypty/modele/xgboost.R")

#### RADIAL_SVM - svm_rad ####
source("./skrypty/modele/svm_rad.R")

#### BAGGED NEURAL NET - nnet ####
source("./skrypty/modele/nnet.R")

#-------------------------------------------------------------------------------------------------------#
####                                    MODELE DO ENSEMBLERA 1                                       ####
#-------------------------------------------------------------------------------------------------------#

#### ZMIENNE DO PIERWSZEGO ENSEMBLERA ####
# najlepsze z klastra wg AUC 
zmienne_ens1 <- lista_zmiennych %>% dplyr::group_by(klaster) %>% dplyr::arrange(dplyr::desc(AUC)) %>% 
   top_n(1) %>% ungroup() %>% select(zmienna)
zmienne_ens1 <- zmienne_ens1$zmienna
zmienne_ens1_woe <- zmienne_ens1
zmienne_ens1_woe[zmienne_ens1_woe %in% zm_factor] <-  paste(zmienne_ens1_woe[zmienne_ens1_woe %in% zm_factor],
                                                            "woe", sep = "_")

#### MODEL LOGITOWY ZE ZMIENNYMI WOE - logit_ens1 ####
source("./skrypty/modele/logit_ens1.R")

#### LAS LOSOWY - las_losowy_ens1 ####
source("./skrypty/modele/las_losowy_ens1.R")

#### XGBOOST - xgb_ens1 ####
source("./skrypty/modele/xgb_ens1.R")

#### SVM RADIALNY - svm_rad_ens1 ####
source("./skrypty/modele/svm_rad_ens1.R")

#### NNET - nnet_ens1 ####
source("./skrypty/modele/nnet_ens1.R")

#-------------------------------------------------------------------------------------------------------#
####                                    MODELE DO ENSEMBLERA 2                                       ####
#-------------------------------------------------------------------------------------------------------#

#### ZMIENNE DO DRUGIEGO ENSEMBLERA ####
# najlepsze z klastra wg AUC + tot_cur_bal  + rti z najwiekszego klastra
zmienne_ens2 <- lista_zmiennych %>% group_by(klaster) %>% arrange(dplyr::desc(AUC)) %>% top_n(2) %>% 
   ungroup() %>% select(zmienna)
zmienne_ens2 <- zmienne_ens2$zmienna
zmienne_ens2 <- zmienne_ens2[!(zmienne_ens2 %in% zmienne_ens1)]
zmienne_ens2 <- append(zmienne_ens2, c("tot_cur_bal", "rti"))
zmienne_ens2_woe <- zmienne_ens2
zmienne_ens2_woe[zmienne_ens2_woe %in% zm_factor] <-  paste(zmienne_ens2_woe[zmienne_ens2_woe %in% zm_factor],
                                                            "woe", sep = "_")

#### MODEL LOGITOWY ZE ZMIENNYMI WOE - logit_ens2 ####
source("./skrypty/modele/logit_ens2.R")

#### LAS LOSOWY - las_losowy_ens2 ####
source("./skrypty/modele/las_losowy_ens2.R")

#### XGBOOST - xgb_ens2 ####
source("./skrypty/modele/xgb_ens2.R")

#### NNET - nnet_ens2 ####
source("./skrypty/modele/nnet_ens2.R")

#### SVM RADIALNY - svm_rad_ens2 ####
source("./skrypty/modele/svm_rad_ens2.R")

#-------------------------------------------------------------------------------------------------------#
####                                       BUDOWA ENSEMBLEROW                                        ####
#-------------------------------------------------------------------------------------------------------#

#### SKLEJANIE PROGNOZ ####

#### TRAIN ####
lista <- list.files(path = "./dane/modele/",pattern = "_train.RData")
lista <- paste("./dane/modele/", lista, sep = "")
lapply(lista, load, .GlobalEnv)
rm(lista)

ens_prog_train <- logit_ens1_train %>% left_join(las_losowy_ens1_train) %>% left_join(xgb_ens1_train) %>%
   left_join(svm_rad_ens1_train) %>% left_join(nnet_ens1_train) %>% left_join(logit_ens2_train) %>%
   left_join(las_losowy_ens2_train) %>% left_join(xgb_ens2_train) %>% left_join(svm_rad_ens2_train) %>%
   left_join(nnet_ens2_train)

ens_prog_train <- bind_cols(default = train_clean$default[ens_prog_train$rowIndex], ens_prog_train)

#### TEST ####

lista <- list.files(path = "./dane/modele/",pattern = "_ocena.RData")
lista <- paste("./dane/modele/", lista, sep = "")
lapply(lista, load, .GlobalEnv)
rm(lista)

ens_prog_test <- data.frame(default = test$default,
                            logit_ens1 = logit_ens1_prognoza$default, 
                            las_losowy_ens1 = las_losowy_ens1_prognoza$default,
                            xgb_ens1 = xgb_ens1_prognoza$default,
                            svm_rad_ens1 = svm_rad_ens1_prognoza$default,
                            nnet_ens1 = nnet_ens1_prognoza$default,
                            logit_ens2 = logit_ens2_prognoza$default, 
                            las_losowy_ens2 = las_losowy_ens2_prognoza$default,
                            xgb_ens2 = xgb_ens2_prognoza$default,
                            svm_rad_ens2 = svm_rad_ens2_prognoza$default,
                            nnet_ens2 = nnet_ens2_prognoza$default
)

# analiza korelacji miedzy prognozami z modeli 
cormat <- cor(ens_prog_train[,-c(1:2)])
corrplot::corrplot(cormat, method = "circle", type = "full", tl.col = "black")

####                          USREDNIENIE WSZYSTKICH MODELI - ENS_AVG_ALL                           ####

# obliczenia na zbiorze treningowym
temp <- apply(ens_prog_train[,-c(1:2)], MARGIN = 1, FUN = mean)
prognoza <- prognoza_ensemblera(score = temp, true_y = ens_prog_train$default)

# ocena na zbiorze treningowym
custom_summary(prognoza)

# obliczenia na zbiorze testowym
temp <- apply(ens_prog_test[,-1], MARGIN = 1, FUN = mean)
prognoza <- prognoza_ensemblera(score = temp, true_y = ens_prog_test$default)

ens_avg_all <- ocena_ensembler(prognoza = prognoza, nazwa_modelu = "ens_avg_all")
ens_avg_all

ens_avg_all_eval_boot <- wyniki_boot_ensembler(df = prognoza, proby_boot = bootstrap, 
                                               nazwa_modelu = "ens_avg_all")

####                             USREDNIENIE 2 NAJLEPSZYCH - ENS_AVG_2                               ####

lista1 <- names(ens_prog_train)[3:7]
lista2 <- names(ens_prog_train)[8:12]

wyniki <- data.frame(Model1 = character(length = 25),
                     Model2 = character(length = 25),
                     AUPRC = numeric(length = 25), 
                     F2 = numeric(length = 25), 
                     ROC = numeric(length = 25), 
                     Accuracy = numeric(length = 25),
                     stringsAsFactors = F)

x <- 1
for(i in 1:5){
   for(i2 in 1:5){
      df <- ens_prog_train %>% dplyr::select(., default, one_of(lista1[i]), one_of(lista2[i2]))
      temp <- apply(df[,-1], MARGIN = 1, FUN = mean)
      prognoza <- prognoza_ensemblera(score = temp, true_y = df$default)
      wyniki[x,3:6] <- custom_summary(prognoza)
      wyniki[x,1:2] <- c(lista1[i], lista2[i2])
      x <- x + 1
   }
}
wyniki
rm(lista1, lista2)

# wybor najlepszego zestawu wg AUPRC
wyniki %>% dplyr::arrange(., dplyr::desc(AUPRC)) %>% dplyr::top_n(1, wt = AUPRC)
#wyniki %>% dplyr::arrange(., dplyr::desc(F2)) %>% dplyr::top_n(1, wt = F2)

# obliczenia na zbiorze testowym
df <- ens_prog_test %>% select(., default, nnet_ens1, xgb_ens2)
temp <- apply(df[,-1], MARGIN = 1, FUN = mean)
prognoza <- prognoza_ensemblera(score = temp, true_y = df$default)

ens_avg_2 <- ocena_ensembler(prognoza = prognoza, nazwa_modelu = "ens_avg_2")
ens_avg_2

ens_avg_2_eval_boot <- wyniki_boot_ensembler(df = prognoza, proby_boot = bootstrap, 
                                             nazwa_modelu = "ens_avg_2")

####                          WAZONA SREDNIA 2 NAJLEPSZYCH - ENS_W_AVG_2                             ####

temp <- 0.6 * df[,2] + 0.4 * df[,3]
prognoza <- prognoza_ensemblera(score = temp, true_y = df$default)

ens_w_avg_2 <- ocena_ensembler(prognoza = prognoza, nazwa_modelu = "ens_w_avg_2")
ens_w_avg_2$ocena_1

ens_w_avg_2_eval_boot <- wyniki_boot_ensembler(df = prognoza, proby_boot = bootstrap, 
                                               nazwa_modelu = "ens_w_avg_2")

####               STACKED ENSEMBLER (SIEC NEURONOWA) NA 2 NAJLEPSZYCH - ENS_STACK_NNET              ####

df <- ens_prog_train %>% select(., default, nnet_ens1, xgb_ens2)

set.seed(1993)
probka <- runif(nrow(df))

# wykres default versus prawdopodobienstwa
p <- ggplot(df[probka <= 0.2,], aes(x = nnet_ens1, y = xgb_ens2, color = default, fill = default)) +
   geom_point(alpha = 0.3) + 
   labs(title = "Zmienna objasniana vs prognozy 2 najlepszych modeli bazowych",
        subtitle = "Probka 20% obserwacji",
        x = "Prawdopodobienstwo default'u wg nnet_ens1",
        y = "Prawdopodobienstwo default'u wg xgb_ens2") +
   theme_bw()
p

#### BAGGED NEURAL NET - ENS_STACK_NNET ####

ctrl_nnet <- ctrl
ctrl_nnet$sampling <- "down"

# parametry
parametry <- expand.grid(
   size = c(2,3,4),
   decay = c(10^seq(-4, -1, 1)),
   bag = TRUE
)

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
ens_stack_nnet <- caret::train(default ~ ., 
                               data = df,
                               method = "avNNet", 
                               metric = "AUPRC",
                               trControl = ctrl_nnet,
                               tuneGrid = parametry)
stopCluster(cl)

ens_stack_nnet
plot(ens_stack_nnet)
ens_stack_nnet$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(ens_stack_nnet)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- ens_prog_test %>% select(., default, nnet_ens1, xgb_ens2)

test_eval <- ocena_test(zb_testowy = df, model = ens_stack_nnet, nazwa_modelu = "ens_stack_nnet")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = ens_stack_nnet, proby_boot = bootstrap,
                              nazwa_modelu = "ens_stack_nnet")
boot_eval$wykres_boot
ens_stack_nnet_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
ens_stack_nnet_prognoza <- predict(ens_stack_nnet, df, type = "prob")
ens_stack_nnet_prognoza$obs <- df$default
ens_stack_nnet_prognoza$pred <- forcats::as_factor(
   ifelse(ens_stack_nnet_prognoza$default >= 0.5, "default", "no_default")
)
ens_stack_nnet_prognoza$pred <- relevel(ens_stack_nnet_prognoza$pred, ref = "default")

# zapis gotowego modelu
save(ens_stack_nnet, file = "./dane/modele/ens_stack_nnet.RData")

# zapis prognozy i oceny bootstrapowej
save(ens_stack_nnet_eval_boot, ens_stack_nnet_prognoza, file = "./dane/modele/ens_stack_nnet_ocena.RData")

# czyszczenie srodowiska
rm(train_eval, test_eval, boot_eval, cl, df)

####                          WAZONA SREDNIA 2 POJEDYNCZYCH - ENS_POJ_2                            ####

temp <- 0.6 * nnet_prognoza$default + 0.4 * svm_rad_prognoza$default
prognoza <- prognoza_ensemblera(score = temp, true_y = test$default)

ens_poj_2 <- ocena_ensembler(prognoza = prognoza, nazwa_modelu = "ens_w_avg_2")
ens_poj_2$ocena_1

ens_poj_2_eval_boot <- wyniki_boot_ensembler(df = prognoza, proby_boot = bootstrap, 
                                             nazwa_modelu = "ens_poj_2")
cor(nnet_prognoza$default, svm_rad_prognoza$default)

#-------------------------------------------------------------------------------------------------------#
####                                    PODSUMOWANIE WYNIKOW                                         ####
#-------------------------------------------------------------------------------------------------------#

# laczenie ocen bootstrapowych w jeden data-frame

ocena_modeli <- bind_rows(logit_eval_boot, las_losowy_eval_boot, xgboost_eval_boot, svm_rad_eval_boot, 
                          nnet_eval_boot, nnet_ens1_eval_boot, xgb_ens2_eval_boot, 
                          ens_avg_all_eval_boot$wyniki_df, ens_avg_2_eval_boot$wyniki_df, 
                          ens_w_avg_2_eval_boot$wyniki_df, ens_stack_nnet_eval_boot, 
                          ens_poj_2_eval_boot$wyniki_df)

ocena_modeli_long <- tidyr::gather(ocena_modeli, key = "Model", value = "Wartosc")
names(ocena_modeli_long)[2] <- "Metryka"
ocena_modeli_long <- ocena_modeli_long %>% mutate( Grupa_modeli = 
                                                      if_else(Model %in% c("logit","las_losowy","xgboost","svm_rad","nnet"), "Modele pojedyncze",
                                                              if_else(Model %in% c("nnet_ens1", "xgb_ens2"), "Modele bazowe", 
                                                                      "Modele zespolowe"))
)
ocena_modeli_long$Model <- forcats::as_factor(ocena_modeli_long$Model)
ocena_modeli_long$Model <- forcats::fct_relevel(ocena_modeli_long$Model, 
                                                c("logit","las_losowy","xgboost","svm_rad","nnet","ens_poj_2",
                                                  "nnet_ens1", "xgb_ens2", "ens_avg_all", "ens_avg_2", 
                                                  "ens_w_avg_2", "ens_stack_nnet"))

ocena_wykres <- function(df, tytul){
   p <- ggplot(df, aes(x = Model, y = Wartosc, fill = Grupa_modeli)) +
      geom_boxplot() +
      labs(title = paste(tytul)) +
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank())
   p
   
} 

# wykresy
df <- filter(ocena_modeli_long, Metryka == "AUPRC")
ocena_wykres(df, tytul = "Pole pod krzywą Precision-Recall")

df <- filter(ocena_modeli_long, Metryka == "F2")
ocena_wykres(df, tytul = "Wartosc metryki F2")

df <- filter(ocena_modeli_long, Metryka == "ROC")
ocena_wykres(df, tytul = "Pole pod krzywą ROC")

df <- filter(ocena_modeli_long, Metryka == "Accuracy")
#ocena_wykres(df, tytul = "Wartosc Accuracy. Rozklady na podstawie 50 prob bootstrapowych")
ocena_wykres(df, tytul = "Wartosc Accuracy")

# tabela ze srednimi wartosciami
ocena_podsumowanie <- ocena_modeli_long %>% group_by(Grupa_modeli, Model, Metryka) %>% 
   summarise_all(., .funs = mean) %>% tidyr::spread(., key = 3, value = 4) %>% 
   select(., Grupa_modeli, Model, AUPRC:ROC, Accuracy)

write.csv(ocena_podsumowanie, file = "./dane/raporty modeli/ocena.csv", row.names = F)

#### TESTY POROWNUJACE PARAMETRY MODELI #### 

# f. pomocnicza - jesli rozklady normalne to t.test srednich, jesli nie to test wilcoxona
check_AUPRC <- function(model1, model2){
   p1 <- shapiro.test(ocena_modeli$AUPRC[ocena_modeli$Model == model1])$p.value
   p2 <- shapiro.test(ocena_modeli$AUPRC[ocena_modeli$Model == model2])$p.value
   
   if(p1 > 0.05 & p2 > 0.05){
      wynik <-  with(data = ocena_modeli, expr = 
      {t.test(x = AUPRC[Model == model1], y = AUPRC[Model == model2], 
              alternative = "greater", mu = 0)})
   } else{
      wynik <- with(data = ocena_modeli, expr = 
      {wilcox.test(x = AUPRC[Model == model1], y = AUPRC[Model == model2], 
                   alternative = "greater", mu = 0)})
   }
   return(wynik)
}

#### AUPRC ####
# nnet > svm_rad
check_AUPRC("nnet", "svm_rad")

# nnet > logit
check_AUPRC("nnet", "logit")

# nnet > ens_poj_2
check_AUPRC("nnet", "ens_poj_2")

# nnet > ens_w_avg_2
check_AUPRC("nnet", "ens_w_avg_2")

# svm_rad > logit
check_AUPRC("svm_rad", "logit")

# ens_avg_all > nnet_ens1
check_AUPRC("ens_avg_all", "nnet_ens1")

# ens_w_avg_2 > ens_stack_nnet
check_AUPRC("ens_w_avg_2", "ens_stack_nnet")

# ens_w_avg_2 > ens_avg_2
check_AUPRC("ens_w_avg_2", "ens_avg_2")

# ens_w_avg_2 > ens_avg_all
check_AUPRC("ens_w_avg_2", "ens_avg_all")

# logit > ens_w_avg_2
check_AUPRC("logit", "ens_w_avg_2")

# logit > xgboost
check_AUPRC("logit", "xgboost")

#### F2 ####

# f. pomocnicza - jesli rozklady normalne to t.test srednich, jesli nie to test wilcoxona
check_F2 <- function(model1, model2){
   p1 <- shapiro.test(ocena_modeli$F2[ocena_modeli$Model == model1])$p.value
   p2 <- shapiro.test(ocena_modeli$F2[ocena_modeli$Model == model2])$p.value
   
   if(p1 > 0.05 & p2 > 0.05){
      wynik <-  with(data = ocena_modeli, expr = 
      {t.test(x = F2[Model == model1], y = F2[Model == model2], 
              alternative = "greater", mu = 0)})
   } else{
      wynik <- with(data = ocena_modeli, expr = 
      {wilcox.test(x = F2[Model == model1], y = F2[Model == model2], 
                   alternative = "greater", mu = 0)})
   }
   return(wynik)
}

# nnet > svm_rad
check_F2("nnet", "svm_rad")

# nnet > ens_poj_2
check_F2("nnet", "ens_poj_2")

# nnet > ens_stack_nnet
check_F2("nnet", "ens_stack_nnet")

# svm_rad > logit
check_F2("svm_rad", "logit")

# ens_stack_nnet > nnet_ens1
check_F2("ens_stack_nnet", "nnet_ens1")

#### AUC ####

# nnet > svm_rad
with(data = ocena_modeli, expr = 
{t.test(x = ROC[Model == "nnet"], y = ROC[Model == "svm_rad"], 
        alternative = "greater", mu = 0)})

# svm_rad > logit
with(data = ocena_modeli, expr = 
{t.test(x = ROC[Model == "svm_rad"], y = ROC[Model == "logit"], 
        alternative = "greater", mu = 0)})

# zapis calosci
save(list = ls(), file = "./dane/dane_final.RData")

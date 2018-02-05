#########################################################################################################
####                          USREDNIENIE WSZYSTKICH MODELI - ENS_AVG_ALL                           ####
#########################################################################################################

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

#########################################################################################################
####                             USREDNIENIE 2 NAJLEPSZYCH - ENS_AVG_2                               ####
#########################################################################################################

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

#########################################################################################################
####                          WAZONA SREDNIA 2 NAJLEPSZYCH - ENS_W_AVG_2                             ####
#########################################################################################################

temp <- 0.6 * df[,2] + 0.4 * df[,3]
prognoza <- prognoza_ensemblera(score = temp, true_y = df$default)

ens_w_avg_2 <- ocena_ensembler(prognoza = prognoza, nazwa_modelu = "ens_w_avg_2")
ens_w_avg_2$ocena_1

ens_w_avg_2_eval_boot <- wyniki_boot_ensembler(df = prognoza, proby_boot = bootstrap, 
                                             nazwa_modelu = "ens_w_avg_2")

#########################################################################################################
####               STACKED ENSEMBLER (SIEC NEURONOWA) NA 2 NAJLEPSZYCH - ENS_STACK_NNET              ####
#########################################################################################################

df <- ens_prog_train %>% select(., default, nnet_ens1, xgb_ens2)

set.seed(1993)
probka <- runif(nrow(df))

# wykres default versus prawdopodobienstwa
p <- ggplot(df[probka <= 0.5,], aes(x = nnet_ens1, y = xgb_ens2, color = default, fill = default)) +
   geom_point(alpha = 0.4) + 
   labs(title = "Zmienna objasniana vs prognozy modeli w ensemblerze",
      subtitle = "Probka 50% obserwacji",
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

#########################################################################################################
####                          WAZONA SREDNIA 2 POJEDYNCZYCH - ENS_POJ_2                            ####
#########################################################################################################

temp <- 0.6 * nnet_prognoza$default + 0.4 * svm_rad_prognoza$default
prognoza <- prognoza_ensemblera(score = temp, true_y = test$default)

ens_poj_2 <- ocena_ensembler(prognoza = prognoza, nazwa_modelu = "ens_w_avg_2")
ens_poj_2$ocena_1

ens_poj_2_eval_boot <- wyniki_boot_ensembler(df = prognoza, proby_boot = bootstrap, 
                                             nazwa_modelu = "ens_poj_2")
cor(nnet_prognoza$default, svm_rad_prognoza$default)


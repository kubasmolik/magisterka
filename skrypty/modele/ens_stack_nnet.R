

ctrl_nnet <- ctrl
ctrl_nnet$sampling <- "down"

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

#########################################################################################################
####                                  PRZYGOTOWANIE ENSEMBLEROW                                      ####
#########################################################################################################

#### ZMIENNE DO PIERWSZEGO ENSEMBLERA ####
# najlepsze z klastra wg AUC + home_ownership z najwiekszego klastra
zmienne_ens1 <- lista_zmiennych %>% group_by(klaster) %>% arrange(dplyr::desc(AUC)) %>% top_n(1) %>% 
   ungroup() %>% select(zmienna)
zmienne_ens1 <- zmienne_ens1$zmienna
zmienne_ens1 <- append(zmienne_ens1, "home_ownership")
zmienne_ens1_woe <- zmienne_ens1
zmienne_ens1_woe[zmienne_ens1_woe %in% zm_factor] <-  paste(zmienne_ens1_woe[zmienne_ens1_woe %in% zm_factor],
                                                            "woe", sep = "_")

#### ZMIENNE DO DRUGIEGO ENSEMBLERA ####
# najlepsze z klastra wg AUC + loan_amnt z najwiekszego klastra
zmienne_ens2 <- lista_zmiennych %>% group_by(klaster) %>% arrange(dplyr::desc(AUC)) %>% top_n(2) %>% 
   ungroup() %>% select(zmienna)
zmienne_ens2 <- zmienne_ens2$zmienna
zmienne_ens2 <- zmienne_ens2[!(zmienne_ens2 %in% zmienne_ens1)]
zmienne_ens2 <- append(zmienne_ens2, "loan_amnt")
zmienne_ens2_woe <- zmienne_ens2
zmienne_ens2_woe[zmienne_ens2_woe %in% zm_factor] <-  paste(zmienne_ens2_woe[zmienne_ens2_woe %in% zm_factor],
                                                            "woe", sep = "_")



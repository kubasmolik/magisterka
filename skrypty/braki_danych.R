#### SPRAWDZANIE I UZUPELNIANIE BRAKOW DANYCH ####

braki <- c()
for(i in 1:dim(dane)[2]){
   braki[i] <- round(sum(is.na(dane[[i]])) / dim(dane)[1], digits = 3)
   names(braki)[i] <- names(dane[i])
}
braki[braki>0]

#### desc_emo ####
# dodatkowa kategoria - "brak_danych"

dane$desc_emo <- forcats::fct_expand(dane$desc_emo, "brak_danych")
dane$desc_emo[is.na(dane$desc_emo)] <- "brak_danych"

#### desc_afinn_score ####
# usuniecie zmiennej

dane$desc_afinn_score <- NULL

#### mths_since_last_delinq ####
# zamiana na zmienna typu factor mths_since_l_delinq_cat

dane$mths_since_l_delinq_cat <- ifelse(dane$mths_since_last_delinq <= 12, 1, NA)
dane$mths_since_l_delinq_cat <- ifelse(dane$mths_since_last_delinq > 12 & dane$mths_since_last_delinq <= 24, 2, dane$mths_since_l_delinq_cat)
dane$mths_since_l_delinq_cat <- ifelse(dane$mths_since_last_delinq > 24 & dane$mths_since_last_delinq <= 36, 3, dane$mths_since_l_delinq_cat)
dane$mths_since_l_delinq_cat <- ifelse(dane$mths_since_last_delinq > 36 & dane$mths_since_last_delinq <= 48, 4, dane$mths_since_l_delinq_cat)
dane$mths_since_l_delinq_cat <- ifelse(dane$mths_since_last_delinq > 48 & dane$mths_since_last_delinq <= 60, 5, dane$mths_since_l_delinq_cat)
dane$mths_since_l_delinq_cat <- ifelse(dane$mths_since_last_delinq > 60 & dane$mths_since_last_delinq <= 72, 6, dane$mths_since_l_delinq_cat)
dane$mths_since_l_delinq_cat <- ifelse(dane$mths_since_last_delinq > 72, 7, dane$mths_since_l_delinq_cat)
dane$mths_since_l_delinq_cat[is.na(dane$mths_since_l_delinq_cat)] <- 8

dane$mths_since_l_delinq_cat <- factor(x = dane$mths_since_l_delinq_cat, 
                                       levels = c("1","2","3","4","5","6","7","8"),
                                       labels = c("ponizej_1","1_plus","2_plus","3_plus",
                                                  "4_plus","5_plus","6_plus","brak_danych"))
dane$mths_since_last_delinq <- NULL

#### revol_util ####
# wypelnienie mediana

mediana_revol_util <- median(dane$revol_util, na.rm = T)
dane$revol_util[is.na(dane$revol_util)] <- mediana_revol_util

#### tot_cur_bal ####
# wypelnienie mediana

mediana_tot_cur_bal <- median(dane$tot_cur_bal, na.rm = T)
dane$tot_cur_bal[is.na(dane$tot_cur_bal)] <- mediana_tot_cur_bal

rm(braki, i, mediana_revol_util, mediana_tot_cur_bal)

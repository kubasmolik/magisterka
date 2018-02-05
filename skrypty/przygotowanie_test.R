#############################################################################
######             PRZYGOTOWANIE ZBIORU TESTOWEGO                      ######
#############################################################################

#load("./dane/test.RData")

test$default <- relevel(test$default, ref = "no_default")

# usuniecie zbednych kolumn
test <- test %>% select(., -id, -member_id, - emp_title, - issue_d, - desc, - title, - earliest_cr_line, 
                        - desc_flg, - delinq_2yrs, - pub_rec, - mths_since_l_delinq_cat, - installment)

# zmiana rodzaju
test$issue_month <- factor(test$issue_month, ordered = F)
test$title_len <- as.numeric(test$title_len)

# zmiana kolejnosci kolumn
test <- select(test, default, loan_amnt:issue_month)

#### POPRAWKI ZMIENNYCH ####

#### annual_inc ####
# logarytm naturalny

test$annual_inc <- log(test$annual_inc)

#### emp_length ####
test$emp_length <- forcats::fct_collapse(test$emp_length,
                                          dzies_8_1 = c("10+ years", "8 years", "1 year"),
                                          bd_mniej_1 = c("n/a", "< 1 year"))
test$emp_length <- forcats::fct_other(test$emp_length, keep = c("dzies_8_1","bd_mniej_1"), 
                                       other_level = "inne")

#### home_ownership ####
levels(test$home_ownership) <- c("MORTGAGE","RENT","RENT","OWN","RENT")

#### purpose ####
test$purpose <- forcats::fct_collapse(test$purpose, 
                                       cc_mp = c("credit_card", "major_purchase"))
test$purpose <- forcats::fct_other(test$purpose, 
                                   keep = c("debt_consolidation", "home_improvement", "cc_mp"),
                                   other_level = "other")

#### addr_state ####
test <- test %>% left_join(., klaster)
test$addr_state_group <- ifelse(test$klaster == 1, "klaster_1", "klaster_2")
test$addr_state_group[is.na(test$addr_state_group)] <- "klaster_2"
test$addr_state_group <- as.factor(test$addr_state_group)
test <- test %>% select(., - addr_state, -klaster)

#### inq_last_6mths ####
test$inq_last_6mths <- ifelse(test$inq_last_6mths > 3, 3, test$inq_last_6mths)
test$inq_last_6mths <- as.factor(test$inq_last_6mths)
levels(test$inq_last_6mths) <- c("zero","one","two","three")

#### revol_util ####
test$revol_util[test$revol_util > 100] <- 100

#### emp_title_cl ####
levels(test$emp_title_cl) <- c("brak","inne","sek_publ","inne")

#### desc_len ####
temp <- ifelse(test$desc_len > 0 & test$desc_len < 55.5, "ponizej_55", "inne")
test$desc_len <- temp
test$desc_len <- as.factor(test$desc_len)
rm(temp)

#### desc_nrc_cat ####
test$desc_nrc_cat <- forcats::fct_other(test$desc_nrc_cat, 
                                        keep = c("none", "positive", "neutral", "trust", 
                                                 "negative", "anticipation"),
                                        other_level = "other")
test$desc_nrc_cat <- forcats::fct_collapse(test$desc_nrc_cat,
                                            none_posit = c("none", "positive"),
                                            neutral_trust = c("neutral", "trust"),
                                            anticipation_other = c("anticipation","other"))

#### desc_emo ####
levels(test$desc_emo) <- c("inne","tak","inne")

#### issue_month ####
test$issue_month <- forcats::fct_collapse(test$issue_month, 
                                           sep_to_dec = c("Sep","Oct","Nov","Dec"))
test$issue_month <- forcats::fct_other(test$issue_month, keep = "sep_to_dec", other_level = "inne")

# dostosowanie kolejnosci leveli
for(x in zm_factor){
   test[[x]] <- forcats::fct_relevel(test[[x]], levels(train[[x]]))
}

# dolaczenie zmiennych typu woe do zbioru testowego
for(i in names(lista_woe)){
   test$temp <- as.character(test[[i]])
   test <- test %>% left_join(., lista_woe[[i]], by = c("temp" = paste(i)))
   test$temp <- NULL
}
sum(is.na(test))

# finalne porzadkowanie zbioru train
test$default <- relevel(test$default, ref = "default")

test <- test %>% select(., one_of(names(train)))

for(x in zm_factor){
   print(identical(contrasts(train[[x]]), contrasts(test[[x]])))
}

identical(names(train), names(test))

rm(i, x, opis, klaster)

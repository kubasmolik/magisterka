#########################################################################################################
####                                  CZYSZCZENIE I PROBKOWANIE                                      ####
#########################################################################################################

#### CZYSZCZENIE - NEIGBORHOOD CLEANING RULE ####
# musimy dac tylko zmienne numeryczne - innych nie akceptuje

temp <- train[,1]
levels(temp$default) <- c("1","0")
temp2 <- train %>% select(., one_of(zm_numeric), ends_with("woe"))
set.seed(1993)
bal <- unbalanced::ubNCL(X = temp2, Y = temp$default)
train_clean <- bind_cols(data.frame(default = bal$Y), bal$X)
table(train_clean$default)
prop.table(table(train_clean$default))

# dolaczenie zmiennych typu factor do zbioru treningowego
for(i in names(lista_woe_scaled)){
   i2 <- paste(i, "woe", sep = "_")
   #train_under$temp <- as.character(train_under[[i2]])
   train_clean$temp <- train_clean[[i2]]
   train_clean <- train_clean %>% left_join(., lista_woe_scaled[[i]], by = c("temp" = paste(i2)))
   train_clean$temp <- NULL
}
train_clean <- train_clean %>% select(., one_of(names(train)))

rm(df, temp, temp2, bal, bal2, i, i2)

levels(train_clean$default) <- c("default","no_default")


save(list = ls(), file = "./dane/dane_modelowanie.RData")

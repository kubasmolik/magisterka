#############################################################################
######                  ANALIZA ZBIORU DANYCH                          ######
#############################################################################

#load("./dane/train.RData")

train$default <- relevel(train$default, ref = "no_default")

# usuniecie zbednych kolumn
train <- train %>% select(., -id, -member_id, - emp_title, - issue_d, - desc, - title, 
                          - earliest_cr_line, - installment)

# funkcja spr. rodzaj zmiennych
spr_rodz_zm <- function(dane){
   rodz_zm <- c()
   for(i in 1:ncol(dane)){
      rodz_zm[i] <- class(dane[[i]])
   }
   return(rodz_zm)
}

spr_rodz_zm(train)

# zmiana rodzaju
train$issue_month <- factor(train$issue_month, ordered = F)
train$title_len <- as.numeric(train$title_len)

# dodatkowy default jako numeric
train$default_num <- as.numeric(train$default) - 1
table(train$default, train$default_num)

# zmiana kolejnosci kolumn
train <- select(train, default, default_num, loan_amnt:mths_since_l_delinq_cat)

# odczytanie typow zmiennych
zm_numeric <- which(spr_rodz_zm(train[,-c(1,2)]) == "numeric") + 2
zm_numeric <-  names(train)[zm_numeric]

zm_factor <- which(spr_rodz_zm(train[,-c(1,2)]) == "factor") + 2
zm_factor <- names(train)[zm_factor]

# generowanie raportow zmiennych numerycznych 
source("./skrypty/funkcje/opis_zm_numeric.R")

for(zmienna in zm_numeric){
   opis <- opis_zm_numeric(train, zmienna)
   save(opis, zmienna, file = "./dane/opis.RData")
   
   rmarkdown::render("./skrypty/raport_zm_num.Rmd",
                     output_file = paste(zmienna, ".html", sep = ""),
                     output_dir = "./dane/raporty zmiennych/numeric",
                     params = list(dynamictitle = paste("Raport dla zmiennej ", zmienna, sep = ""),
                                   dynamicdate = Sys.Date())
   )
}

# f. pomocnicza do wykrywania outlierow
detect_outlier <- function(zbior, zmienna){
   temp <- scale(zbior[[zmienna]], center = T, scale = T)
   proc_outlier <- length(which(abs(temp) > 3))/ length(temp)
   outliers <- which(abs(temp) > 3)
   return(list(proc_outlier = proc_outlier, outliers = outliers))
}

#### annual_inc #### 
temp <- detect_outlier(train, "annual_inc")
summary(train$annual_inc[temp$outliers])
train <- train[-temp$outliers,]

#### credit_lines_diff #### 
temp <- detect_outlier(train, "credit_lines_diff")
summary(train$credit_lines_diff[temp$outliers])
train <- train[-temp$outliers,]

#### delinq_2yrs ####
train %>% group_by(delinq_2yrs) %>% count() %>% mutate(procent = round(n / nrow(train), digits = 4))
train$delinq_2yrs <- ifelse(train$delinq_2yrs > 0, 1,0)
train$delinq_2yrs <- as.factor(train$delinq_2yrs)

#### desc_len #### 
temp <- detect_outlier(train, "desc_len")
summary(train$desc_len[temp$outliers])
train <- train[-temp$outliers,]

#### inq_last_6mths ####
train %>% group_by(inq_last_6mths) %>% count() %>% mutate(procent = round(n / nrow(train), digits = 4))
train$inq_last_6mths <- ifelse(train$inq_last_6mths > 3, 3, train$inq_last_6mths)
train$inq_last_6mths <- as.factor(train$inq_last_6mths)

#### open_acc ####
temp <- detect_outlier(train, "open_acc")
summary(train$open_acc[temp$outliers])
train <- train[-temp$outliers,]

#### pub_rec ####
train %>% group_by(pub_rec) %>% count() %>% mutate(procent = round(n / nrow(train), digits = 4))
train$pub_rec <- ifelse(train$pub_rec > 0, 1, 0)
train$pub_rec <- as.factor(train$pub_rec)

#### revol_bal ####
temp <- detect_outlier(train, "revol_bal")
summary(train$revol_bal[temp$outliers])
train <- train[-temp$outliers,]

#### revol_util ####
train <- train[train$revol_util <= 100,]

#### rti ####
temp <- detect_outlier(train, "rti")
summary(train$rti[temp$outliers])
train <- train[-temp$outliers,]

#### title_len ####
temp <- detect_outlier(train, "title_len")
summary(train$title_len[temp$outliers])
train <- train[-temp$outliers,]

#### tot_cur_bal ####
temp <- detect_outlier(train, "tot_cur_bal")
summary(train$tot_cur_bal[temp$outliers])
train <- train[-temp$outliers,]

#### total_acc ####
temp <- detect_outlier(train, "total_acc")
summary(train$total_acc[temp$outliers])
train <- train[-temp$outliers,]

# odczytanie typow zmiennych
zm_numeric <- which(spr_rodz_zm(train[,-c(1,2)]) == "numeric") + 2
zm_numeric <-  names(train)[zm_numeric]

zm_factor <- which(spr_rodz_zm(train[,-c(1,2)]) == "factor") + 2
zm_factor <- names(train)[zm_factor]


do.call(file.remove, list(list.files("./dane/raporty zmiennych/numeric", full.names = TRUE)))

for(zmienna in zm_numeric){
   opis <- opis_zm_numeric(train, zmienna)
   save(opis, zmienna, file = "./dane/opis.RData")
   
   rmarkdown::render("./skrypty/raport_zm_num.Rmd",
                     output_file = paste(zmienna, ".html", sep = ""),
                     output_dir = "./dane/raporty zmiennych/numeric",
                     params = list(dynamictitle = paste("Raport dla zmiennej ", zmienna, sep = ""),
                                   dynamicdate = Sys.Date())
   )
}


library(unbalanced)
podzial_rpart <- function(zbior, zmienna){
   temp <- zbior %>% select(., default_num, .data[[zmienna]])
   temp$target <- forcats::as_factor(as.character(temp$default_num))
   
   bal <- unbalanced::ubUnder(X = temp[2], Y = temp$target)
   temp_bal <- data.frame(bal$Y) %>% bind_cols(data.frame(bal$X))
   names(temp_bal) <- c("target", zmienna)
   
   form <- as.formula(paste("target ~ ", zmienna, sep = ""))
   tree <- rpart::rpart(form, data = temp_bal)
   
   return(tree$splits)
}


#### annual_inc ####
# logarytm naturalny

train$annual_inc <- log(train$annual_inc)

#### desc_len ####
temp <- train %>% select(., default, default_num, desc_len) %>% filter(., desc_len > 0)
temp2 <- opis_zm_numeric(temp, "desc_len")
temp2
set.seed(1993)
podzial_rpart(temp, "desc_len")

temp <- ifelse(train$desc_len == 0, "zero", ifelse(train$desc_len >= 55.5, "ponad_55", "ponizej_55"))
table(temp, train$desc_flg)

train$desc_len <- temp
train$desc_len <- as.factor(train$desc_len)
train$desc_flg <- NULL

rm(temp, temp2)

# odczytanie typow zmiennych
zm_numeric <- which(spr_rodz_zm(train[,-c(1,2)]) == "numeric") + 2
zm_numeric <-  names(train)[zm_numeric]

zm_factor <- which(spr_rodz_zm(train[,-c(1,2)]) == "factor") + 2
zm_factor <- names(train)[zm_factor]


# generowanie raportow zmiennych kategorialnych 
source("./skrypty/funkcje/opis_zm_factor.R")


for(zmienna in zm_factor[!(zm_factor %in% "addr_state")]){
   opis <- opis_zm_factor(train, zmienna)
   save(opis, zmienna, file = "./dane/opis.RData")
   
   rmarkdown::render("./skrypty/raport_zm_fac.Rmd",
                     output_file = paste(zmienna, ".html", sep = ""),
                     output_dir = "./dane/raporty zmiennych/factor",
                     params = list(dynamictitle = paste("Raport dla zmiennej ", zmienna, sep = ""),
                                   dynamicdate = Sys.Date())
   )
}

#### delinq_2yrs ####
train$delinq_2yrs <- NULL

#### desc_emo ####
temp <- train %>% filter(., desc_emo != "tak") %>% select(., default, desc_emo)
levels(temp$desc_emo) <- levels(temp$desc_emo)[c(1,3,3)]
with(temp, prop.test(table(desc_emo, default)))

levels(train$desc_emo) <- c("inne","tak","inne")

#### desc_len ####
temp <- train %>% filter(., desc_len != "ponizej_55") %>% select(., default, desc_len)
levels(temp$desc_len) <- levels(temp$desc_len)[c(1,3,3)]
with(temp, prop.test(table(desc_len, default)))

levels(train$desc_len) <- c("inne","ponizej_55","inne")

#### desc_nrc_cat ####
train$desc_nrc_cat <- forcats::fct_lump(train$desc_nrc_cat, prop = 0.01, other_level = "other")
prop.table(table(train$desc_nrc_cat, train$default),1)
train$desc_nrc_cat <- forcats::fct_collapse(train$desc_nrc_cat,
                                            none_posit = c("none", "positive"),
                                            neutral_trust = c("neutral", "trust"),
                                            anticipation_other = c("anticipation","other"))

#### emp_length ####
temp <- train %>% select(., default_num, .data[["emp_length"]])
temp$target <- forcats::as_factor(as.character(temp$default_num))

bal <- unbalanced::ubUnder(X = temp[2], Y = temp$target)
temp_bal <- data.frame(bal$Y) %>% bind_cols(data.frame(bal$X))
names(temp_bal) <- c("target", "emp_length")

form <- as.formula(paste("target ~ ", "emp_length", sep = ""))
tree <- rpart::rpart(form, data = temp_bal)

summary(tree)
table(temp_bal$emp_length)

train$emp_length <- forcats::fct_collapse(train$emp_length,
                                            dzies_8_1 = c("10+ years", "8 years", "1 year"),
                                            bd_mniej_1 = c("n/a", "< 1 year"))

train$emp_length <- forcats::fct_other(train$emp_length, keep = c("dzies_8_1","bd_mniej_1"), 
                                       other_level = "inne")
rm(temp, bal, temp_bal, tree, form)

#### emp_title_cl ####
levels(train$emp_title_cl) <- c("brak","inne","sek_publ","inne")

#### home_ownership ####
levels(train$home_ownership) <- c("MORTGAGE","RENT","RENT","OWN","RENT")

#### issue_month ####
temp <- train %>% select(., default_num, .data[["issue_month"]])
temp$target <- forcats::as_factor(as.character(temp$default_num))

set.seed(1993)
bal <- unbalanced::ubUnder(X = temp[2], Y = temp$target)
temp_bal <- data.frame(bal$Y) %>% bind_cols(data.frame(bal$X))
names(temp_bal) <- c("target", "issue_month")
tree <- rpart::rpart(target ~ issue_month, data = temp_bal)

summary(tree)
table(temp_bal$issue_month)

train$issue_month <- forcats::fct_collapse(train$issue_month, 
                                           sep_to_dec = c("Sep","Oct","Nov","Dec"))
train$issue_month <- forcats::fct_other(train$issue_month, keep = "sep_to_dec", other_level = "inne")
rm(temp, temp_bal, bal, tree)

#### mths_since_l_delinq_cat ####
train$mths_since_l_delinq_cat <- NULL

#### pub_rec ####
train$pub_rec <- NULL

#### purpose ####

train$purpose <- forcats::fct_lump(train$purpose, prop = 0.02, other_level = "other")
prop.table(table(train$purpose))
prop.table(table(train$purpose, train$default),1)

train$purpose <- forcats::fct_collapse(train$purpose, 
                                       cc_mp = c("credit_card", "major_purchase"))

#### addr_state ####
temp <- prop.table(table(train$addr_state, train$default),1)
temp <- data.frame(pr = temp[,2], name = rownames(temp))
temp2 <- temp[is.nan(temp$pr) == F,1]
names(temp2) <- rownames(temp)[is.nan(temp$pr) == F]

set.seed(1993)
klaster <- kmeans(temp2, 3)
klaster

klaster <- klaster$cluster
klaster <- data.frame(klaster = klaster, addr_state = names(klaster))
klaster$klaster[klaster$klaster == 2] <- 1

train <- train %>% left_join(., klaster)
table(train$klaster, useNA = 'always')
train$addr_state_group <- ifelse(train$klaster == 1, "klaster_1", "klaster_2")
train$addr_state_group <- as.factor(train$addr_state_group)

rm(temp, temp2)

# porzadkowanie zbioru
train <- train %>% select(., -addr_state, -klaster)

levels(train$inq_last_6mths) <- c("zero","one","two","three")

# odczytanie typow zmiennych
zm_numeric <- which(spr_rodz_zm(train[,-c(1,2)]) == "numeric") + 2
zm_numeric <-  names(train)[zm_numeric]

zm_factor <- which(spr_rodz_zm(train[,-c(1,2)]) == "factor") + 2
zm_factor <- names(train)[zm_factor]

# szeregowanie poziomow zmiennych wg liczebnosci
for(i in zm_factor){
   train[[i]] <- forcats::fct_infreq(train[[i]])
}

# usuwanie dotychczasowych raportow
do.call(file.remove, list(list.files("./dane/raporty zmiennych/factor", full.names = TRUE)))

# generowanie raportow
for(zmienna in zm_factor){
   opis <- opis_zm_factor(train, zmienna)
   save(opis, zmienna, file = "./dane/opis.RData")
   
   rmarkdown::render("./skrypty/raport_zm_fac.Rmd",
                     output_file = paste(zmienna, ".html", sep = ""),
                     output_dir = "./dane/raporty zmiennych/factor",
                     params = list(dynamictitle = paste("Raport dla zmiennej ", zmienna, sep = ""),
                                   dynamicdate = Sys.Date())
   )
}

# f. pomocnicza do liczenia weight of evidence
licz_woe <- function(x, zbior){
   log_all <- log( table(zbior[["default"]])[1] / table(zbior[["default"]])[2] )
   t <- table(zbior[[x]], zbior[["default"]])
   t2 <- t[,1]/ t[,2]
   t3 <- log(t2) - log_all
   
   t3 <- data.frame(zmienna_woe = t3, zmienna = names(t3))
   t3$zmienna <- as.character(t3$zmienna)
   names(t3) <- c(paste(x, "woe", sep = "_"), x)
   return(t3)
}

# wyliczanie woe
lista_woe <- list()
i <- 1
for(x in zm_factor){
   lista_woe[[i]] <- licz_woe(x, train)
   i <- i + 1
}
names(lista_woe) <- zm_factor

# dolaczenie zmiennych typu woe do zbioru treningowego
for(i in names(lista_woe)){
   train$temp <- as.character(train[[i]])
   train <- train %>% left_join(., lista_woe[[i]], by = c("temp" = paste(i)))
   train$temp <- NULL
}
sum(is.na(train))

# finalne porzadkowanie zbioru train
train$default <- relevel(train$default, ref = "default")
train$default_num <- NULL



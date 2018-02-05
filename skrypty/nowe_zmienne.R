#### TWORZENIE NOWYCH ZMIENNYCH ####

library(stringr)
library(stringi)
library(rebus)
library(tidytext)


#### NOWA ZMIENNA - pti ####
#### payment to income ratio (monthly)

dane$pti <- dane$installment / (dane$annual_inc / 12)

#### NOWA ZMIENNA - rti ####
#### revolving to income ratio (monthly)

dane$rti <- dane$revol_bal / (dane$annual_inc / 12)

#### NOWA ZMIENNA - dti_LC ####
#### DTI tylko dla tej pozyczki
dane$dti_LC <- dane$loan_amnt / dane$annual_inc

#### NOWA ZMIENNA - emp_title_cl ####

# lista top 50 firm w USA
wzorce <- list(
   "walmart",
   "berk" %R% one_or_more(WRD) %R% one_or_more(SPC) %R% 
      "hath" %R% one_or_more(WRD), #berkshire hathaway
   START %R% "apple" %R% or(SPC, END),
   "exxon" %R% optional(SPC) %R% "mobil",
   "mc" %R% optional(SPC) %R% "ke" %R% one_or_more("ss") %R% "on", #mckesson
   or("uhg", "united" %R% optional(SPC) %R% "health" %R% optional(SPC) %R% "group"),
   or("cvs", "cvs" %R% optional(SPC) %R% "health"),
   or(or(START,SPC) %R% "gm" %R% or(END, SPC), 
      "general" %R% optional(SPC) %R% "motors"), # General Motors
   "at&t",
   or("ford" %R% optional(SPC) %R% "motor", START %R% "fm" %R% or(SPC,END)),
   or("amerisource" %R% optional(SPC) %R% "bergen", START %R% "ab" %R% or(SPC,END)),
   "amazon",
   or("general" %R% optional(SPC) %R% "electric", START %R% "ge" %R% or(SPC,END)),
   "verizon",
   "cardinal" %R% optional(SPC) %R% "health",
   "costco",
   "walgr", # walgreens
   "kroger",
   "chevron",
   or("fa" %R% one_or_more("n") %R% "ie" %R% optional(SPC) %R% "mae",
      or(START,SPC) %R% "fm" %R% or(START,SPC)), #fannie mae
   "morgan",
   "express" %R% optional(SPC) %R% "script" %R% optional("s") %R% 
      optional(SPC) %R% optional("holding"), # express scripts
   "home" %R% optional(SPC) %R% "depot",
   "boeing",
   or("wells" %R% optional(SPC) %R% "fargo", or(START, SPC) %R% "wf" %R% or(SPC,END)),
   "bank" %R% optional(one_or_more(SPC)) %R% "of" %R% optional(one_or_more(SPC)) %R% "america",
   "microsoft",
   "anthem",
   or("citi" %R% or(END,SPC), "citi" %R% optional(SPC) %R% "group",
      "citi" %R% optional(SPC) %R% "bank"),
   "comcast",
   "ibm",
   or("state" %R% optional(SPC) %R% "farm" %R% optional(SPC) %R% "ins",
      or(START, SPC) %R% "sfi" %R% or(END,SPC)), 
   or(START,SPC) %R% "phi" %R% one_or_more("l") %R% "ips" %R% or(END,SPC), #phillips
   "johnson" %R% optional(SPC) %R% or("&", "and") %R% optional(SPC) %R% "johnson",
   or(or("procter","proctor") %R% optional(SPC) %R% or("&", "and") %R% optional(SPC) %R% "ga",
      or(START, SPC) %R% "pg" %R% or(END, SPC)),
   "valero",
   "target",
   "fre" %R% one_or_more("d") %R% "ie" %R% optional(SPC) %R% "mac",
   or(START, SPC) %R% "lowe",
   or(START, SPC) %R% "dell",
   "met" %R% optional(SPC) %R% "life",
   "aetna",
   "pepsi",
   or(or(START, SPC) %R% "archer" %R% SPC, or(START, SPC) %R% "adm"  %R% or(END, SPC)),
   or(START, SPC) %R% "ups" %R% or(END, SPC),
   or(START, SPC) %R% "intel" %R% or(END, SPC),
   or(START, SPC) %R% "prudential" %R% or(END, SPC),
   or(START, SPC) %R% "albertsons" %R% or(END, SPC),
   "united" %R% optional(SPC) %R% "tech"
)

# szukanie wzorcow
temp <- purrr::map2(.x = list(str_to_lower(dane$emp_title)), .y = wzorce, str_detect)
df <- data.frame(temp[[1]])
for(i in 2:length(temp)){
   df <- cbind(df,temp[[i]])
}
df_sum <- apply(df, MARGIN = 1, sum)
top50 <- ifelse(df_sum >= 1, 1, 0)

rm(wzorce, temp, df, df_sum)

# szukanie pracujacych w sektorze publicznym
wzorce <- list(
   "army",
   "usaf",
   "military" %R% or(END, SPC),
   "nav" %R% or("y", "al"),
   or(START, SPC) %R% "us" %R% or(END, SPC),
   or(START, SPC) %R% "city" %R% or(END, SPC),
   or(START, SPC) %R% "county" %R% or(END, SPC),
   or(START, SPC) %R% "police" %R% or(END, SPC),
   or(START, SPC) %R% "public" %R% or(END, SPC),
   or(START, SPC) %R% "civil" %R% or(END, SPC),
   or(START, SPC) %R% "government" %R% or(END, SPC),
   or(START, SPC) %R% "fire",
   or(START, SPC) %R% "national" %R% or(END, SPC),
   or(START, SPC) %R% "university" %R% or(END, SPC),
   or(START, SPC) %R% "state" %R% or(END, SPC)
)

# szukanie wzorcow
temp <- purrr::map2(.x = list(str_to_lower(dane$emp_title)), .y = wzorce, str_detect)
df <- data.frame(temp[[1]])
for(i in 2:length(temp)){
   df <- cbind(df,temp[[i]])
}
df_sum <- apply(df, MARGIN = 1, sum)
public <- ifelse(df_sum >= 1, 1, 0)

rm(wzorce, temp, df, df_sum)

# przypadki kiedy nie podano opisu zatrudnienia 
brak <- ifelse(str_length(dane$emp_title) < 2, 1, 0)

# konstruowanie finalnej zmiennej
emp_title_cl <- if_else(brak == 1, 1, 4)
emp_title_cl <- if_else(emp_title_cl == 4 & top50 == 1, 2, emp_title_cl)
emp_title_cl <- if_else(emp_title_cl == 4 & public == 1, 3, emp_title_cl)

dane$emp_title_cl <- factor(emp_title_cl, levels = c("1","2","3","4"), 
                            labels = c("brak","top_50","sek_publ","inne"))

rm(brak, public, top50, emp_title_cl)

#### POMOCNICZY ZBIOR ####

df <- dane %>% select(., id, issue_d, desc, purpose, title, earliest_cr_line)

#### NOWA ZMIENNA - desc_flag ####
#### flaga - czy jest opis pozyczki

df$desc_flg <- ifelse(str_length(df$desc) == 0, 0, 1)
df$desc_flg <-  factor(df$desc_flg, levels = c("0","1"), labels = c("brak","jest_opis"))

dane <- left_join(dane, select(df, id, desc_flg), by = "id")

#### NOWA ZMIENNA - desc_len ####
#### numeric - dlugosc opisu

# zbior pomocniczy
df <- dane %>% filter(., desc_flg == "jest_opis") %>% 
         select(., id, issue_d, desc, purpose, title, earliest_cr_line)

# czyszczenie z fraz technicznych
p <- "Borrower added on " %R% 
   one_or_more(DGT) %R% "/" %R% one_or_more(DGT) %R% "/" %R% one_or_more(DGT) %R% " > "

df$desc_clear <- str_replace_all(df$desc, pattern = p, replacement = "")
df$desc_clear <- str_replace_all(df$desc_clear, pattern = "<br>", replacement = "")
df$desc_clear <- str_trim(df$desc_clear, side = "both")
df$desc_len <- str_length(df$desc_clear)

dane <- left_join(dane, select(df, id, desc_len), by = "id")
dane$desc_len[is.na(dane$desc_len)] <- 0

#### NOWA ZMIENNA - desc_afinn_score ####
#### suma afinn score dla opisu pozyczki - dodatni pozytywny, ujemny negatywny

# tokenizacja opisu
desc_unnested <- tbl_df(unnest_tokens(select(df, id, desc_clear), word, desc_clear))
desc_unnested$word <- str_to_lower(desc_unnested$word)

# usuwanie slow technicznych
data(stop_words)
stop_words
desc_unnested <- desc_unnested %>% anti_join(stop_words)

# analiza sentymentu afinn
sentyment_afinn <- get_sentiments("afinn")
desc_unnested <- left_join(desc_unnested, sentyment_afinn, by = "word")
desc_unnested$score[is.na(desc_unnested$score)] <- 0

# liczenie scoru
temp <- desc_unnested %>% group_by(.,id) %>% summarise(., desc_afinn_score = sum(score))

dane <- left_join(dane, select(temp, id, desc_afinn_score), by = "id")

#### NOWA ZMIENNA - desc_nrc_cat ####
#### kategoria emocjonalna ktora najczesciej pojawila sie w opisie

sentyment_nrc <- get_sentiments("nrc")
temp <- left_join(desc_unnested, sentyment_nrc, by = "word")

temp2 <- temp %>% select(., id, sentiment) %>% filter(., is.na(sentiment) == F) %>%
   group_by(., id, sentiment) %>% count()

temp <- temp2 %>% arrange(., desc(n), sentiment) %>% 
   group_by(.,id) %>% mutate( x = row_number())
temp2 <- temp %>% filter(., x == 1) %>% select(., id, sentiment)

df <- left_join(df, temp2, by = "id")
df$sentiment[is.na(df$sentiment)] <- "neutral"

dane <- left_join(dane, select(df, id, sentiment), by = "id")
dane <- rename(dane, desc_nrc_cat = sentiment)

dane$desc_nrc_cat[is.na(dane$desc_nrc_cat)] <- "none"
dane$desc_nrc_cat <- factor(dane$desc_nrc_cat)

rm(temp,temp2)

#### NOWA ZMIENNA - desc_emo ####
#### flaga - czy w opisie wystepuja zwroty podkreslajace emocje

wzorce <- list(
   "must",
   "really",
   "have to",
   "need",
   "promise",
   "beg",
   "!",
   "always",
   "help"
)

temp <- purrr::map2(.x = list(str_to_lower(df$desc_clear)), .y = wzorce, str_detect)
temp2 <- data.frame(temp[[1]])
for(i in 2:length(temp)){
   temp2 <- cbind(temp2,temp[[i]])
}
temp2_sum <- apply(temp2, MARGIN = 1, sum)
df$desc_emo <- ifelse(temp2_sum >= 1, 1, 0)
table(df$desc_emo)
df$desc_emo <- factor(df$desc_emo, levels = c("0","1"), labels = c("nie","tak"))
dane <- left_join(dane, select(df, id, desc_emo), by="id")

# czyszczenie srodowiska
rm(desc_unnested, temp, temp2, temp2_sum, wzorce, df)

#### NOWA ZMIENNA - title_len ####
#### dlugosc tytulu

dane$title_len <- str_length(str_trim(dane$title, "both"))

#### NOWA ZMIENNA - credit_lines_diff ####
#### ile miesiecy przed issue_date otwarto pierwsza linie kredytowa

dane$credit_lines_diff <- round((as.numeric(dane$issue_d - dane$earliest_cr_line)) / 30)

#### NOWA ZMIENNA - issue_month ####
#### miesiac udzielenia pozyczki

dane$issue_month <- lubridate::month(dane$issue_d, label = T)

# czyszczenie srododwiska
rm(bad_status, p, sentyment_afinn, sentyment_nrc, stop_words, temp, i)

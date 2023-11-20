
#### Data Preparation and Cleaning ####
source("Functions_Orange.R")

giudiziale <- Loans_table
colnames(bcc_Raw) <- tolower(colnames(bcc_Raw)) %>% gsub(" |_", ".",.)
colnames(giudiziale) <- tolower(colnames(giudiziale)) %>% gsub(" |_", ".",.)

bcc_Raw <- bcc_Raw %>% 
  mutate_all(tolower)
giudiziale <- giudiziale %>% 
  mutate_all(tolower)

giudiziale <- giudiziale %>% filter(`mortgage.yes/no` == "yes" | `consortium.guarantee.(yes/no)` == "yes")

#select what you need
guarantees <- bcc_Raw %>% select(ndg, id.loans, mortgage.date, type.of.mortgage, mortgage.amount, previous.mortgage.amount, lien)

guarantees <- distinct(guarantees)

#create type for liens only
guarantees <- guarantees %>% mutate(type= "lien")

#rename columns
guarantees <- guarantees %>% rename(id.bor = ndg, id.loan= id.loans, date.registration = mortgage.date, 
                                    origin.lien= type.of.mortgage, amount.guarantee= mortgage.amount,
                                    rank.lien= lien, amount.prev.lien= previous.mortgage.amount)

guarantees$id.bor <- as.character(guarantees$id.bor)

guarantees <- guarantees %>%
  mutate_all(~str_replace_all(., "\\([^)]+\\)", ""))

guarantees <- guarantees %>%
  mutate(amount.guarantee = gsub(",", "", amount.guarantee))

guarantees$amount.guarantee <- guarantees$amount.guarantee %>% str_trim(.,'both')



#### Obtain Missing Information From Loans_table ####

giudiziale <- giudiziale %>% select(ndg, id.loans, type.of.guarantee, type.of.mortgage, 
                                    guarantee.amount, 
                                    `consort..guarantee.amount.(importo.garantito)`, consortium.name) %>%
  rename(id.loan= id.loans)

#create a table for consortiums guarantees only
giudiziale_consortium <- giudiziale %>% filter(!is.na(`consort..guarantee.amount.(importo.garantito)`))
giudiziale_consortium$guarantee.amount <- giudiziale_consortium$`consort..guarantee.amount.(importo.garantito)`

#clean and merge the guarantees 
giudiziale <- giudiziale %>% select(-c(`consort..guarantee.amount.(importo.garantito)`, consortium.name))
giudiziale_consortium <- giudiziale_consortium %>% 
  mutate(type.of.guarantee = consortium.name) %>% 
  select(-c(`consort..guarantee.amount.(importo.garantito)`, consortium.name))

giudiziale <- giudiziale %>% rbind(giudiziale_consortium) %>% 
  rename(id.bor = ndg, amount.guarantee = guarantee.amount, origin.lien= type.of.mortgage,
         type= type.of.guarantee)

giudiziale <- giudiziale %>% mutate(origin.lien = strsplit(as.character(origin.lien), "\\+")) %>% 
  unnest(origin.lien)

giudiziale$origin.lien <- giudiziale$origin.lien %>% str_trim(.,'both')

giudiziale <- distinct(giudiziale)



guarantees <- bind_rows(guarantees, giudiziale)





giudiziale_only <- giudiziale %>% filter(origin.lien == "giudiziale") %>%
  select(-c(type, origin.lien, amount.guarantee))


guarantees <- guarantees %>% left_join(giudiziale_only, by= "id.bor")

guarantees$id.loan.x <- ifelse(guarantees$id.loan.x == "giudiziale", guarantees$id.loan.y, guarantees$id.loan.x)
  
guarantees <- guarantees %>% rename(id.loan= id.loan.x) %>% select(-id.loan.y)

guarantees <- distinct(guarantees)

guarantees <- guarantees %>% group_by(id.bor, date.registration, origin.lien,
                                      amount.guarantee, rank.lien) %>% 
  mutate(id.guarantee = paste0("g_", id.bor, "_", date.registration, "_", origin.lien,
                               "_", amount.guarantee, "_", rank.lien)) %>%
  relocate(id.guarantee, .before = id.bor) %>% ungroup()



#### Link Guarantees-Loans ####

link_guarantees_loans <- guarantees %>% select(id.guarantee, id.loan) %>% distinct()


#### Last Cleaning and Define origin.lien, type, status ####

guarantees <- guarantees %>% select(-id.loan) %>% distinct()

guarantees <- add_origin.lien_guarantees(guarantees)

guarantees <- add_type_guarantees(guarantees)

guarantees <- add_status_guarantees(guarantees, "date.registration", 20)

#### Create NULL Columns and set column classes ####

guarantees$id.group <- NA
guarantees$registry <- NA
guarantees$nrg <- NA
guarantees$nrp <- NA
guarantees$flag.imputed <- NA

guarantees$date.registration <- as.Date(guarantees$date.registration)

#character columns
columns_to_convert <- c("id.guarantee", "id.bor", "id.group", "registry", "nrg", "nrp")

guarantees <- guarantees %>%
  mutate(across(all_of(columns_to_convert), as.character))

#factor columns
columns_to_convert <- c("type", "status","origin.lien")

guarantees <- guarantees %>%
  mutate(across(all_of(columns_to_convert), as.factor))


#integer columns
columns_to_convert <- c("rank.lien", "flag.imputed")

guarantees <- guarantees %>%
  mutate(across(all_of(columns_to_convert), as.integer))


#integer columns
columns_to_convert <- c("amount.guarantee", "amount.prev.lien")

guarantees <- guarantees %>%
  mutate(across(all_of(columns_to_convert), as.numeric))




#### Reorder Columns ####

column_order <- c("id.guarantee", "id.bor", "id.group", "type", "amount.guarantee", "status", 
"date.registration", "origin.lien", "rank.lien", "amount.prev.lien", "registry", "nrg", "nrp", 
"flag.imputed")

guarantees <- guarantees %>%
  select(column_order)


#### Link guarantees-entities ####

link_guar_count <- link_guarantees_loans %>% left_join(Link_loans_counterparties, by="id.loan")
link_guar_ent <- link_guar_count %>% left_join(Link_counterparties_entities, by="id.counterparty")
link_guarantees_entities <- link_guar_ent %>% select(-c(id.loan, id.counterparty)) %>% distinct()

###------------------------------------------###
#---                 sources       -----
###------------------------------------------###
source("empty_column&tolower_functions.R")
source('Functions/Entities_functions.R')
source("Functions/Area.R")


###------------------------------------------###
#---               counterparties        -----
###------------------------------------------###

# da Loans table prendiamo BorrowerName e gli assegniamo il ruolo borrower
#                          Guarantors Name                        guarantors
# poi creiamo il counter_id 
#poi dopo aver creato entities, aggiungiamo la colonna n.entities


Borr <- Loans_table %>% select("ID Loans" , BorrowerName) %>% distinct() %>% na.omit(.)
Borr <- Borr %>% mutate(role = 'Borrower') 
Borr <- Borr %>% rename(name = BorrowerName)

Guar <- Loans_table %>% select("ID Loans",`Guarantors Name`) %>% distinct() %>% na.omit(.)
Guar <- Guar %>% mutate(role = 'Guarantor')
Guar <- Guar %>% rename(name = `Guarantors Name`)

Counterparties <- rbind(Borr,Guar)
Counterparties <- Counterparties %>% rename(id.loan = "ID Loans")

###------------------------------------------###
#---    link loans counterparties       -----
###------------------------------------------###

Link_loans_counterparties <- Counterparties %>% select(id.loan, name)

Counterparties <- Counterparties %>% select(-id.loan) %>% distinct()
Counterparties <- Counterparties %>% mutate(id.counterparty = paste0("c_", 400 + row_number())) %>%
  relocate(id.counterparty, .before = name)
column_to_create <- c("id.bor","id.group","flag.imputed")
Counterparties <- create_empty_columns(Counterparties,column_to_create )
Counterparties <- Counterparties[ ,c(1,4,5,3,2,6)]    

Link_loans_counterparties <- Link_loans_counterparties %>%
  left_join(Counterparties, by = "name") %>% select(c(1,3))

Link_loans_counterparties <- Link_loans_counterparties %>% mutate_if(is.character, tolower)

###------------------------------------------###
#---               entities           -----
###------------------------------------------###
Borrowers_table <- Borrowers_table %>% rename(name = Name, cf.piva = Tax_ID,
                                              city = Town, province = City, region = Region)


Guarantors_groups <- Guarantors_groups %>% rename(name = guarantors.name, cf.piva = tax.code.for.guarantors)
# create the entities table from the NDG and Loans table:
Entities <-  merge(Borrowers_table, Guarantors_groups, by = c("name", "cf.piva"), all = TRUE)
Entities$cf.piva <-  str_trim(Entities$cf.piva)

# colonne da aggiungere nulle: dummy.info, solvency.pf, income.pf, status.pg, date.cessation, flag.imputed

column_to_create_empty <- c("dummy.info","solvency.pf", "income.pf", "status.pg", "date.cessation", "flag.imputed")
Entities <- create_empty_columns(Entities,column_to_create_empty )

#Entities <- create_type_subject(Entities, Entities$cf.piva)
#determine if the entity is corporate or individual according to the cf.piva
Entities <- Entities %>%
  mutate(type.subject = ifelse(grepl("^[0-9]+$", cf.piva), 'corporate', NA))
Entities$type.subject <- ifelse(is.na(Entities$type.subject), 'individual', Entities$type.subject)

#Entities <- create_sex(Entities, Entities$cf.piva)
Entities <- add_sex_column(Entities)
Entities <- create_date_of_birth(Entities, Entities$cf.piva)
Entities$date.of.birth <- as.Date(Entities$date.of.birth, format = '%Y-%m-%d')
Entities <- create_age(Entities, Entities$date.of.birth)
#Entities <- create_range_age(Entities, Entities$age)
Entities <- add_age_range_column(Entities)

Entities$name <- Entities$name %>% gsub("[.]","",.)
Counterparties$name <- Counterparties$name %>% gsub("[.]","",.)

#Entities <- create_type_pg(Entities, Entities$type.subject,Entities$name)
Entities <- add_type.pg_column(Entities)
Entities <- create_area_city_prov(Entities, Entities$city, Entities$province)

#create the Id for the entities:
Entities <- Entities %>% mutate(id.entity = paste0("e_", 100 + row_number()))
Entities <- Entities[,c(19,1,2,12,6,13,15,16,7,8,17,9,10,3,4,5,18,11)]

Entities <- Entities %>% mutate_if(is.character, tolower)
Counterparties <- Counterparties %>% mutate_if(is.character, tolower)
###------------------------------------------###
#---     link counterparties entities  -----
###------------------------------------------###
Link_counterparties_entities <- c()
for(i in 1:nrow(Counterparties)){
  elements <- unlist(strsplit(Counterparties$name[i], '[,,-]'))
  for(j in 1:nrow(Entities)){
    if(str_trim(Entities$name[j]) %in% str_trim(elements)){
      Link_counterparties_entities <- rbind(Link_counterparties_entities,c(Entities$id.entity[j], Counterparties$id.counterparty[i])) 
    }
  }
}
Link_counterparties_entities <- Link_counterparties_entities %>% as.data.frame() %>%
  rename(id.counterparty = V2, id.entity = V1)
Link_counterparties_entities <- Link_counterparties_entities[,c(2,1)]

Link_counterparties_entities <- Link_counterparties_entities %>% mutate_if(is.character, tolower)

# POTREBBE SERVIRE: 
# unisce TUZZATO FRANCESCO a "IMMOBILIARE SPES DI TUZZATO FRANCESCO SOCIETA' SEMPLICE"
# grepl(paste0("\\b", Entities$name[28], "\\b"),Counterparties$name[11] )

###------------------------------------------###
#---  adding n_entities a Counterparties  -----
###------------------------------------------###


Counterparties$n.entities <- NA 
Counterparties <- create_n_entities(Counterparties)
Counterparties <- Counterparties[,c(1:5,7,6)]

Counterparties$role <- as.factor(Counterparties$role)
Entities$type.subject <- as.factor(Entities$type.subject)
Entities$sex <- as.factor(Entities$sex)
Entities$range.age <- as.factor(Entities$range.age)



#write.xlsx(new_data, existing_file, sheetName = "NewSheet", append = TRUE, col.names = TRUE)
#write.xlsx(new_data, existing_file, sheetName = "NewSheet", append = TRUE, col.names = TRUE)


#########################################
##---      charts and tables    ---##
#########################################

####  Guarantees by type ####


guarantees_no_na <- guarantees %>% filter(!is.na(amount.guarantee))

total.guarantees <- guarantees_no_na %>% 
  summarise(type = "total", "N Cases" = n(), 
            "% Cases" = paste0(round(n()/n()*100,1), "%"),
            "Sum Guarantee (k)" = round(sum(amount.guarantee)/1e3,1),
            "Mean Guarantee (k)" = round(sum(amount.guarantee)/n()/1e3,1),
            "% Guarantee" = paste0(round(sum(amount.guarantee)/sum(amount.guarantee)*100,1), "%"))


guar_total <- sum(guarantees_no_na$amount.guarantee)
guar_n_total <- 25
guar.type.table <- guarantees_no_na %>%
  group_by(type) %>% summarise("N Cases" = n(), 
                                    "% Cases" = paste0(round(n()/25*100,1), "%"),
                               "Sum Guarantee (k)" = round(sum(amount.guarantee)/1e3,1),
                               "Mean Guarantee (k)" = round(sum(amount.guarantee)/n()/1e3,1),
                                    "% Guarantee" = paste0(round(sum(amount.guarantee)/guar_total*100,1), "%"))

guar.type.table <- rbind(total.guarantees, guar.type.table)
guar.type.table <- guar.type.table %>% rename(Type = type)


####  Guarantees by origin.lien ####
guarantees_no_na <- guarantees %>% filter(!is.na(amount.guarantee))

total.guarantees <- guarantees_no_na %>% 
  summarise(origin.lien = "total", "N Cases" = n(), 
            "% Cases" = paste0(round(n()/n()*100,1), "%"),
            "Sum Guarantee (k)" = round(sum(amount.guarantee)/1e3,1),
            "Mean Guarantee (k)" = round(sum(amount.guarantee)/n()/1e3,1),
            "% Guarantee" = paste0(round(sum(amount.guarantee)/sum(amount.guarantee)*100,1), "%"))


guar_total <- sum(guarantees_no_na$amount.guarantee)
guar_n_total <- 25
guar.origin.lien.table <- guarantees_no_na %>%
  group_by(origin.lien) %>% summarise("N Cases" = n(), 
                               "% Cases" = paste0(round(n()/25*100,1), "%"),
                               "Sum Guarantee (k)" = round(sum(amount.guarantee)/1e3,1),
                               "Mean Guarantee (k)" = round(sum(amount.guarantee)/n()/1e3,1),
                               "% Guarantee" = paste0(round(sum(amount.guarantee)/guar_total*100,1), "%"))

guar.origin.lien.table <- rbind(total.guarantees, guar.origin.lien.table)
guar.origin.lien.table <- guar.origin.lien.table %>% rename("Origin Lien" = origin.lien)

#### Guarantees by status ####

guarantees_no_na <- guarantees %>% filter(!is.na(amount.guarantee))

total.guarantees <- guarantees_no_na %>% 
  summarise(status = "total", "N Cases" = n(), 
            "% Cases" = paste0(round(n()/n()*100,1), "%"),
            "Sum Guarantee (k)" = round(sum(amount.guarantee)/1e3,1),
            "Mean Guarantee (k)" = round(sum(amount.guarantee)/n()/1e3,1),
            "% Guarantee" = paste0(round(sum(amount.guarantee)/sum(amount.guarantee)*100,1), "%"))


guar_total <- sum(guarantees_no_na$amount.guarantee)
guar_n_total <- 25
guar.status.table <- guarantees_no_na %>%
  group_by(status) %>% summarise("N Cases" = n(), 
                                      "% Cases" = paste0(round(n()/25*100,1), "%"),
                                 "Sum Guarantee (k)" = round(sum(amount.guarantee)/1e3,1),
                                 "Mean Guarantee (k)" = round(sum(amount.guarantee)/n()/1e3,1),
                                      "% Guarantee" = paste0(round(sum(amount.guarantee)/guar_total*100,1), "%"))




#in no UTP also use this other half
df <- data.frame('status' = "expired",
                 'StatusCases' = 0,
                 '%Cases' = paste0(0, "%"),
                 'MeanGBVResidual(k)' = 0,
                 'SumGBVResidual(k)' = 0,
                 '%GBVResidual' = paste0(0, "%")
)
df <- df %>% rename('status' = "status",
                    'N Cases' = 'StatusCases',
                    '% Cases' = 'X.Cases',
                    'Mean Guarantee (k)' = 'MeanGBVResidual.k.',
                    'Sum Guarantee (k)' = 'SumGBVResidual.k.',
                    '% Guarantee' = 'X.GBVResidual'
)
status.guar <- rbind(total.guarantees, guar.status.table, df)
status.guar <- status.guar %>% rename(Status = status)





###-----------------------------------------------------------------------###
#-----             Loans                     -----         
###-----------------------------------------------------------------------###
#summary table for totals
r.introductionP6 <- Loans %>% 
  summarise(
    'N Borrowers' = n_distinct(id.bor),
    'N LOANS' = n_distinct(id.loan),
    'GBV (M)' = round(sum(gbv.residual) / 1e6, 1),  # Round GBV to 2 decimal places and convert to millions
    'Average Borrower size (k)' = round(sum(gbv.residual) / n_distinct(id.bor) / 1e3, 1),  # Round average borrower size to 2 decimal places and convert to thousands
    'Average loan size (k)' = round(sum(gbv.residual) / n_distinct(id.loan) / 1e3, 1)  # Round average loan size to 2 decimal places and convert to thousands
  )

#counts and % of type loans - min mean, max gbv for each
total.n <- nrow(Loans)
total.gbv <- sum(Loans$gbv.residual)

r.type.gbv.totals <- Loans %>% summarise("type" = "total", 'Type Cases' = n_distinct(id.loan),
                                         '% Cases' = paste0(round(n_distinct(id.loan)/total.n*100, 1), "%"),
                                         'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                         'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.loan)/ 1e3, 1),
                                         '% GBV' =  paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))

r.type.gbv <- Loans %>% group_by(type) %>% summarise('Type Cases' = n_distinct(id.loan),
                                                     '% Cases' = paste0(round(n_distinct(id.loan)/total.n*100, 1), "%"),
                                                     'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                     'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.loan)/ 1e3, 1) ,
                                                     '% GBV' =  paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))

r.type.gbv <- rbind(r.type.gbv.totals, r.type.gbv)
r.type.gbv <- r.type.gbv %>% rename(Type = type)

#counts and % of status loans - min mean, max gbv for each
total.n <- nrow(Loans)
total.gbv <- sum(Loans$gbv.residual)

r.status.gbv.totals <- Loans %>% summarise(status = "total", 'Status Cases' = n_distinct(id.loan),
                                           '% Cases' = paste0(round(n_distinct(id.loan)/total.n*100, 1), "%"),
                                           'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                           'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.loan)/ 1e3, 1) ,
                                           '% GBV' =  paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))


r.status.gbv <- Loans %>% group_by(status) %>% summarise('Status Cases' = n_distinct(id.loan),
                                                         '% Cases' = paste0(round(n_distinct(id.loan)/total.n*100, 1), "%"),
                                                         'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                         'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.loan)/ 1e3, 1) ,
                                                         '% GBV' =  paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))

#in no UTP also use this other half
df <- data.frame('status' = "utp",
                 'StatusCases' = 0,
                 '%Cases' = paste0(0, "%"),
                 'MeanGBVResidual(k)' = 0,
                 'SumGBVResidual(k)' = 0,
                 '%GBVResidual' = paste0(0, "%")
)
df <- df %>% rename('status' = "status",
                    'Status Cases' = 'StatusCases',
                    '% Cases' = 'X.Cases',
                    'Mean GBV Residual (k)' = 'MeanGBVResidual.k.',
                    'Sum GBV Residual (k)' = 'SumGBVResidual.k.',
                    '% GBV' = 'X.GBVResidual'
)
r.status.gbv <- rbind(r.status.gbv.totals,r.status.gbv, df)
r.status.gbv <- r.status.gbv %>% rename(Status = status)


# % gbv in each type class
total.n <- nrow(Loans)
total.gbv <- sum(Loans$gbv.residual)
r.gbv.perc <- Loans %>% group_by(type) %>% summarise('GBV %' = round(sum(gbv.residual)/total.gbv*100, 1))
chart.gbv.perc <- ggplot(r.gbv.perc, aes(x = type, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Type", y = "GBV %", title= "GBV Residual % by Loan Type") + 
  geom_text(aes(label = `GBV %`), vjust = 2.5, size = 5)
chart.gbv.perc



#cases for date.status
Loans$year <- format(Loans$date.status, "%Y")
r.date.status.cases <- Loans %>% group_by(year) %>% summarise("N Cases" = n_distinct(id.loan)) %>% ungroup()
chart.date.status.cases <- ggplot(r.date.status.cases, aes(x = year, y = `N Cases`, group= 1)) +
  geom_bar(stat = 'identity', fill = "slategray", color= "blue") + 
  labs(x= "Year of State Passage", y= "N Loans",title= "Last Status Date") + 
  geom_text(aes(label = paste0( "[",`N Cases`, "]"), y=47), size = 4) + theme_white_but_border() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
chart.date.status.cases

###-----------------------------------------------------------------------###
#-----                     loans ranges                              -----         
###-----------------------------------------------------------------------###

quartiles <- quantile(Loans$gbv.residual, probs = c(0.25, 0.5, 0.75))
plot(Loans$gbv.residual)
hist(Loans$gbv.residual)

#gbv ranges chart
gbv_ranges <- c(0, 150000, 300000, 450000, Inf)  # Define the age ranges
gbv_labels <- c("0-100k", "150k-300k", "300k-450k", "+450k")  # Define labels for the ranges

#create range.gbv column, calculate the total gbv, create table with range.gbv and % of gbv per range
df <- Loans
df$range.gbv <- cut(df$gbv.residual, breaks = gbv_ranges, labels = gbv_labels, include.lowest = TRUE)
total.gbv <- sum(df$gbv.residual)
r.p28.gbvByLoanSize <- df %>% select(gbv.residual, range.gbv) %>% group_by(range.gbv) %>% 
  summarise("GBV %" = round(sum(gbv.residual) / total.gbv *100, 1))

r.p28.g.gbvByLoanSize <- ggplot(r.p28.gbvByLoanSize, aes(x = range.gbv, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "slategray", color = "blue") +    #71EAF7
  labs(x = "GBV Ranges", y = "GBV %", title= "GBV Residual % by Loan Size") + 
  geom_text(aes(label = paste0("[",`GBV %`, "%", "]"), y=52), size = 4, color= "black") + geom_rangeframe() +
  theme_white_but_border()#+ bbc_style()
r.p28.g.gbvByLoanSize


gg <- ggplot(Loans, aes(x = gbv.residual)) +
  geom_histogram(binwidth = 150000, fill = "slategray", color = "blue") +
  xlab("GBV Amount") +
  ylab("N Loans") +
  ggtitle("Number of Loans by Amount") + 
  theme_white_but_border()
gg


###-----------------------------------------------------------------------###
#-----                     borrowers                               -----         
###-----------------------------------------------------------------------###
Borrowers <- counterparty_finished %>% filter(role=='borrower') %>% distinct()
Borrowers <- left_join(Borrowers,Loans, by = "id.bor",relationship = "many-to-many")
Borrowers <- left_join(Borrowers,link_entity_counterparty,by = "id.counterparty",relationship = "many-to-many")
Borrowers <- Borrowers %>% distinct(id.bor, id.loan, .keep_all = TRUE)
Borrowers <- left_join(Borrowers,entities_complete, by = "id.entity")

#% Borrowers by areas 
total.gbv <- sum(Loans$gbv.residual)
total.borrowers <- n_distinct(Borrowers$id.entity)
r.p27.geographicalDistribution <- Borrowers %>%
  mutate(
    area = case_when(
      area == "center"  ~ "center",
      area == "islands" ~ "south and islands",
      area == "south" ~ "south and islands",
      area == "north-east" ~ "north-east",
      area == "north-west" ~ "north-west"
    )) %>%
  select(id.entity, area, gbv.residual) %>%
  group_by(area) %>%
  summarise("Borrower %" = round(n_distinct(id.entity) / total.borrowers * 100, 1),
            "GBV %" = round(sum(gbv.residual) / total.gbv * 100, 1)
  )
r.p27.borrowerByArea <- ggplot(r.p27.geographicalDistribution, aes(x = area, y = `Borrower %`)) +
  geom_bar(stat = 'identity', fill = "#79CDCD") +
  labs(x = "Area", y = "Borrowers %", title= "Borrowers % per Area") + 
  geom_text(aes(label = paste0("[",`Borrower %`, "%", "]"), y = 95), vjust = 0.5, size = 4) + theme_white_but_border()
r.p27.borrowerByArea

r.p27.gbvByArea <- ggplot(r.p27.geographicalDistribution, aes(x = area, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#66CDAA") +
  labs(x = "Area", y = "GBV %", title= "GBV % per Area") + 
  geom_text(aes(label = paste0("[",`GBV %`, "%","]"), y= 74), vjust = 0.5, size = 4) + theme_white_but_border()
r.p27.gbvByArea


#table n.entities per borrower
total.n <- n_distinct(Borrowers$id.counterparty)
total.gbv <- sum(Borrowers$gbv.residual)
r.borrower.n.entities.totals <- Borrowers %>% summarise(n.entities = "total",'Borrowers' = n_distinct(id.counterparty),
                                                        '% Cases' = paste0(round(n_distinct(id.counterparty)/total.n*100, 1), "%"),
                                                        'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                        'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.counterparty)/ 1e3, 1),
                                                        '% GBV' = paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))


r.borrower.n.entities <- Borrowers %>% group_by(n.entities) %>% summarise('Borrowers' = n_distinct(id.counterparty),
                                                                          '% Cases' = paste0(round(n_distinct(id.counterparty)/total.n*100, 1), "%"),
                                                                          'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                                          'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.counterparty)/ 1e3, 1) ,
                                                                          '% GBV' = paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))

r.borrower.n.entities <- rbind(r.borrower.n.entities.totals, r.borrower.n.entities)
r.borrower.n.entities <- r.borrower.n.entities %>% rename("N Entities" = n.entities)


#table N borrowers with N loans

df <- Borrowers %>% group_by(id.counterparty) %>% summarise("N Loans per Borrower"= n_distinct(id.loan),
                                                            "GBV Sum" = sum(gbv.residual))
total.gbv <- sum(df$`GBV Sum`)
r.n.borrowers.n.loans.totals <- df %>% summarise("N Loans per Borrower" = "total", "Borrowers"= n_distinct(id.counterparty),
                                                 "GBV Residual Sum (k)" = round(sum(`GBV Sum`)/1e3, 1),
                                                 '% GBV' = paste0(round(sum(`GBV Sum`)/total.gbv*100, 1), "%"))


r.n.borrowers.n.loans <- df %>% group_by(`N Loans per Borrower`) %>% summarise("Borrowers"= n_distinct(id.counterparty),
                                                                               "GBV Residual Sum (k)" = round(sum(`GBV Sum`)/1e3, 1),
                                                                               '% GBV' = paste0(round(sum(`GBV Sum`)/total.gbv*100, 1), "%"))


r.n.borrowers.n.loans <- rbind(r.n.borrowers.n.loans.totals, r.n.borrowers.n.loans)

entities_merged <- entities_complete %>% merge(link_entity_counterparty, by= "id.entity")
entities_merged2 <- entities_merged %>% merge(counterparty_finished, by="id.counterparty")
entities_merged2 <- entities_merged2 %>% filter(role== "borrower")

# n. borrowers by type
total.borrowers <- n_distinct(entities_merged2$id.counterparty)
r.type.subject.n.cases.totals <- entities_merged2 %>% summarise(type.subject = "total","N Borrowers" = n_distinct(id.counterparty),
                                                                "Borrower %" = paste0(round(n_distinct(id.counterparty) / total.borrowers * 100, 1), "%"))


r.type.subject.n.cases <- entities_merged2 %>% group_by(type.subject) %>% summarise("N Borrowers" = n_distinct(id.counterparty),
                                                                                    "Borrower %" = paste0(round(n_distinct(id.counterparty) / total.borrowers * 100, 1), "%"))

r.type.subject.n.cases <- rbind(r.type.subject.n.cases.totals, r.type.subject.n.cases)
r.type.subject.n.cases <- r.type.subject.n.cases %>% rename("Borrower Type" = type.subject)


#age range borrowers
individuals <- entities_merged2 %>% filter(type.subject == "individual")
individuals_distinct <- individuals[!duplicated(individuals[c("id.counterparty")]), ]
total.individuals <- nrow(individuals_distinct)

r.individuals.range.age.totals <- individuals_distinct %>% 
  summarise(range.age = "total","N Borrowers"= n_distinct(id.counterparty),
            "Borrower %" = paste0(round(n_distinct(id.counterparty) / total.individuals * 100, 1), "%"))


r.individuals.range.age <- individuals_distinct %>% group_by(range.age) %>% 
  summarise("N Borrowers"= n_distinct(id.counterparty),
            "Borrower %" = paste0(round(n_distinct(id.counterparty) / total.individuals * 100, 1), "%"))

r.individuals.range.age <- rbind(r.individuals.range.age.totals, r.individuals.range.age)
r.individuals.range.age <- r.individuals.range.age %>% rename("Age Range" = range.age)







#GBV by borrower province
r.p27.borrowersByProvince <- Borrowers %>% select(id.counterparty,gbv.residual,province) %>% distinct() %>% 
  group_by(province) %>% 
  summarise(sum_gbv = round(sum(gbv.residual)/ 1e6, 1), 
            N_borr = n_distinct(id.counterparty), 
            avg_size = round( sum(gbv.residual)/N_borr / 1e3, 1)) %>% 
  arrange(desc(sum_gbv))
# the top 5 are Roma (rm), Teramo(te), Pescara(pe) ,Milano (mi), Genova (ge)
r.p27.borrowersByProvince.head <- head(r.p27.borrowersByProvince, 5)
r.p27.borrowersByProvince.head <- r.p27.borrowersByProvince.head %>% as.data.frame()

r.p27.borrowersByProvince.head <- r.p27.borrowersByProvince.head %>% mutate(
  province = case_when(
    province == "fi"  ~ "firenze",
    province == "bo" ~ "bologna",
    province == "ra" ~ "ravenna",
    province == "pi" ~ "pisa",
    is.na(province) ~ "NA"
  ))

r.p27.borrowersByProvince.head <- r.p27.borrowersByProvince.head %>%
  arrange(desc(sum_gbv))

r.p27.borrowersByProvince.head.support.table <- r.p27.borrowersByProvince.head %>% select(-sum_gbv)
r.p27.borrowersByProvince.head.support.table <- r.p27.borrowersByProvince.head.support.table %>% 
  rename("N Borrowers" = "N_borr", "Mean Size Borrowers (k)" = "avg_size")

r.p27.borrowersByProvince.top.5 <- ggplot(r.p27.borrowersByProvince.head, aes(x = sum_gbv, y = reorder(province, sum_gbv))) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "GBV Sum (M)", y = "Province", title= "GBV Sum per Province") + 
  geom_text(aes(label = sum_gbv), vjust = 1, size = 5)
r.p27.borrowersByProvince.top.5



###-----------------------------------------------------------------------###
#-----                profiling  info providing                        -----         
###-----------------------------------------------------------------------###


#infoprov_cf_final  n.cases per solvency type
chart.info.pf.solvency.region <- infoprov_pf_final %>% 
  group_by(solvency.adj) %>% 
  summarise(cases = n())

my.palette <- c("#97FFFF", "#79CDCD", "#528B8B")

chart.solvency.class.cases <- ggplot(chart.info.pf.solvency.region, aes(x = solvency.adj, y= cases)) +  
  geom_col(aes(fill = solvency.adj)) +  
  labs(title = "Solvency Adjusted N Cases", x = "Solvency Type", y= "N Cases", color= "brrr") + 
  guides(fill=guide_legend(title="Solvency Type")) +
  scale_fill_manual(values = my.palette) + geom_text(aes(label = paste0( "[",cases, "]"), y=122), size = 4) +
  theme_white_but_border() 
chart.solvency.class.cases

#mean income.net per solvency type
n_cases_total <- infoprov_pf_final %>% filter(!is.na(income.net)) %>% nrow()
table.info.income.solvency.totals <- infoprov_pf_final %>% filter(!is.na(income.net)) %>%
  summarise(solvency.adj = "total", "N Cases" = n(), "% Cases" = paste0(round(n()/n_cases_total*100,1), "%"), mean_income = round(mean(income.net), 1))
table.info.income.solvency <- infoprov_pf_final %>% filter(!is.na(income.net)) %>%
  group_by(solvency.adj) %>% 
  summarise("N Cases" = n(), "% Cases" = paste0(round(n()/n_cases_total*100,1), "%"), mean_income = round(mean(income.net), 1))
table.info.income.solvency <- rbind(table.info.income.solvency.totals, table.info.income.solvency)
table.info.income.solvency <- table.info.income.solvency %>% rename("Solvency Type" = solvency.adj,
                                                                    "Mean Net Income" = mean_income)

n_cases_total <- infoprov_pf_final %>% filter(!is.na(income.net)) %>% nrow()
table.info.income.region.totals <- infoprov_pf_final %>% filter(!is.na(income.net)) %>% 
  summarise(region = "total", "N Cases" = n(), "% Cases" = paste0(round(n()/n_cases_total*100,1), "%"), mean_income = round(mean(income.net), 1))

table.info.income.region <- infoprov_pf_final %>% filter(!is.na(income.net)) %>%
  group_by(region) %>% summarise("N Cases" = n(), "% Cases" = paste0(round(n()/n_cases_total*100,1), "%"), mean_income = round(mean(income.net), 1)) %>% arrange(desc(`% Cases`))
table.info.income.region <- rbind(table.info.income.region.totals, table.info.income.region)
table.info.income.region <- table.info.income.region %>% rename("Region" = region,
                                                                "Mean Net Income" = mean_income)

#infoprov_piva_final 
#% of status per type of corporate

table.info.piva.type <- infoprov_piva_final %>% filter(!is.na(type)) %>% group_by(type, status) %>%
  summarise(cases= n())

table.info.piva.type <- table.info.piva.type %>% pivot_wider(names_from= status, values_from = cases,
                                                             id_expand = FALSE)
table.info.piva.type[is.na(table.info.piva.type)] <- 0



rowtotals <- rowSums(table.info.piva.type[,-1])
table.info.piva.type2 <- round(table.info.piva.type[,-1]/rowtotals*100,1)
table.info.piva.type2 <- table.info.piva.type2 %>% mutate(type = table.info.piva.type$type)
table.info.piva.type2 <- table.info.piva.type2[, c(7,1:6)]
table.info.piva.type <- table.info.piva.type2

#n.cases status per region


table.info.piva.status.region <- infoprov_piva_final %>% filter(!is.na(status)) %>%
  group_by(region, status) %>% summarise(cases = n()) %>% arrange(desc(cases))
table.info.piva.status.region <- table.info.piva.status.region %>% pivot_wider(names_from= status, values_from = cases,
                                                                               id_expand = FALSE)

table.info.piva.status.region[is.na(table.info.piva.status.region)] <- 0



###-----------------------------------------------------------------------###
#-----               Agreement Summary                     -----         
###-----------------------------------------------------------------------###

agreement.summary.status <- Agreement_Summary 

agreement.summary.status.totals <- agreement.summary.status %>% 
  summarise(status = "total", 
            "GBV Sum" = round(sum(gbv.agreement),1),
            "GBV Mean" = round(mean(gbv.agreement),1),
            "Amount Sum" = sum(amount.agreement),
            "Amount Mean" = mean(amount.agreement),
            "% of Discount" = paste0(round(100-(sum(amount.agreement)/sum(gbv.agreement)*100),1), "%"),
            Months = mean(length), "Sum Amount Paid" = sum(paid), 
            "% of Payback" = paste0(round(sum(paid)/sum(amount.agreement)*100,1), "%"))

agreement.summary.status <- agreement.summary.status %>% group_by(status) %>% 
  summarise("GBV Sum" = round(sum(gbv.agreement),1),
            "GBV Mean" = round(sum(gbv.agreement)/n(),1),
            "Amount Sum" = sum(amount.agreement),
            "Amount Mean" = sum(amount.agreement)/n(),
            "% of Discount" = paste0(round(100-(sum(amount.agreement)/sum(gbv.agreement)*100),1), "%"),
            Months = mean(length), "Sum Amount Paid" = sum(paid), 
            "% of Payback" = paste0(round(sum(paid)/sum(amount.agreement)*100,1), "%"))

agreement.summary.status <- rbind(agreement.summary.status.totals, agreement.summary.status)
agreement.summary.status <- agreement.summary.status %>% rename("Status" = status)

agreement.proj.period <- Agreement_Proj
agreement.proj.period$year_month <- format(as.Date(agreement.proj.period$date.paid), "%Y-%m")
agreement.proj.period<- agreement.proj.period[complete.cases(agreement.proj.period$year_month), ]
agreement.proj.period <- agreement.proj.period %>% group_by(year_month) %>% 
  summarise("Amount Paid" = sum(amount.paid))

chart.date.amount.paid <- ggplot(agreement.proj.period, aes(x = year_month, y = `Amount Paid`, group= 1)) +
  geom_line(stat = 'identity', color= "blue") + labs(x= "Year and Month", y="Amount", title= "Payback") +
  theme_white_but_border() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+ bbc_style()
chart.date.amount.paid



agreement.summary.n.cases.status <- Agreement_Summary 

agreement.summary.n.cases.status.totals <- agreement.summary.n.cases.status %>% 
  summarise(status = "total", "N Agreements" = n(), "Mean Instalments" = mean(n.instalment))

agreement.summary.n.cases.status <- agreement.summary.n.cases.status %>% group_by(status) %>% summarise(
  "N Agreements" = n(), "Mean Instalments" = mean(n.instalment)
)

agreement.summary.n.cases.status <- rbind(agreement.summary.n.cases.status.totals, agreement.summary.n.cases.status)
agreement.summary.n.cases.status <- agreement.summary.n.cases.status %>% rename(Status = status)




p <- Agreement_Summary %>% filter(n.instalment != 39)

p <- ggplot(p, aes(x = n.instalment , y = paid)) +
  geom_smooth(data = p, method = "lm", 
              se = FALSE, col = "#ee1515",outliers.shape = NA) +
  geom_point() +
  labs(title = "Amount Paid - N Instalments") +
  scale_x_continuous(labels = function(x) paste0(x, " N")) +
  scale_y_continuous(labels = function(y) paste0(y, " paid")) +
  bbc_style() +
  theme(plot.title = element_text(size= 15, color = "#063376"))
p




#########################
### CORP & IND ###
#########################

counter <- counterparty_finished %>% select(id.counterparty, role, n.entities)
agree_count <- Agreement_Summary %>% left_join(counter, by= "id.counterparty")
agree_count <- agree_count %>% left_join(link_entity_counterparty, by= "id.counterparty", relationship = "many-to-many")
ent <- entities_complete %>% select(id.entity, cf.piva, range.age, sex, region, type.subject, type.pg)
agree_count <- agree_count %>% left_join(ent, by="id.entity")
info <- infoprov_pf_final %>% select(cf.piva, solvency.adj, income.net)
agree_count <- agree_count %>% left_join(info, by="cf.piva")


agree.count.ind.corp.age <- agree_count[!duplicated(agree_count$id.agreement), ]

agree.count.ind.corp.age.total <- agree.count.ind.corp.age %>% 
  summarise(Type = "total", range.age= NA, "N Cases" = n(), 
            "% Cases" = paste0(round(n()/n()*100,1), "%"),
            "Sum GBV" = sum(gbv.agreement),
            "Mean GBV" = sum(gbv.agreement)/n(),
            "% GBV" = paste0(round(sum(gbv.agreement)/sum(gbv.agreement)*100,1), "%"))


n_ind_total <- agree.count.ind.corp.age %>% filter(type.subject == "individual") %>% nrow()
gbv_ind_total <-agree.count.ind.corp.age %>% filter(type.subject == "individual") 
gbv_ind_total <- sum(gbv_ind_total$gbv.agreement)
agree.count.ind.age <- agree.count.ind.corp.age %>% filter(type.subject == "individual") %>%
  group_by(range.age) %>% summarise(Type = "individual", "N Cases" = n(), 
                                    "% Cases" = paste0(round(n()/n_ind_total*100,1), "%"),
                                    "Sum GBV" = sum(gbv.agreement),
                                    "Mean GBV" = sum(gbv.agreement)/n(),
                                    "% GBV" = paste0(round(sum(gbv.agreement)/gbv_ind_total*100,1), "%"))

n_total <- agree.count.ind.corp.age %>% nrow()
gbv_total <-agree.count.ind.corp.age 
gbv_total <- sum(gbv_total$gbv.agreement)
agree.count.ind.age.total <-agree.count.ind.corp.age %>% filter(type.subject == "individual") %>%
  summarise(Type = "individual tot", range.age= NA, "N Cases" = n(), 
            "% Cases" = paste0(round(n()/n_total*100,1), "%"),
            "Sum GBV" = sum(gbv.agreement),
            "Mean GBV" = sum(gbv.agreement)/n(),
            "% GBV" = paste0(round(sum(gbv.agreement)/gbv_total*100,1), "%"))



n_corp_total <- agree.count.ind.corp.age %>% filter(type.subject == "corporate") %>% nrow()
gbv_corp_total <-agree.count.ind.corp.age %>% filter(type.subject == "corporate") 
gbv_corp_total <- sum(gbv_corp_total$gbv.agreement)
agree.count.corp.age <- agree.count.ind.corp.age %>% filter(type.subject == "corporate") %>%
  group_by(range.age) %>% summarise(Type = "corporate","N Cases" = n(), 
                                    "% Cases" = paste0(round(n()/n_corp_total*100,1), "%"),
                                    "Sum GBV" = sum(gbv.agreement),
                                    "Mean GBV" = sum(gbv.agreement)/n(),
                                    "% GBV" = paste0(round(sum(gbv.agreement)/gbv_corp_total*100,1), "%"))



agree.count.corp.age.total <-agree.count.ind.corp.age %>% filter(type.subject == "corporate") %>%
  summarise(Type = "corporate tot", range.age= NA, "N Cases" = n(), 
            "% Cases" = paste0(round(n()/n_total*100,1), "%"),
            "Sum GBV" = sum(gbv.agreement),
            "Mean GBV" = sum(gbv.agreement)/n(),
            "% GBV" = paste0(round(sum(gbv.agreement)/gbv_total*100,1), "%"))

agree.count.ind.corp.age <- rbind(agree.count.ind.corp.age.total, agree.count.ind.age.total, 
                                  agree.count.ind.age, agree.count.corp.age.total, agree.count.corp.age)







###-----------------------------------------------------------------------###
#-----                     Page 29 report                                -----         
###-----------------------------------------------------------------------###
#gbv by default vintage
bv_ranges <- c(0, 8, 11, 14, 17, 20, Inf)  # Define the age ranges
gbv_labels <- c("0-8 years", "9-11 years", "12-14 years", "15-17 years", "18-20 years", "+20 years")  # Define labels for the ranges

df <- LOANS
df <- df %>%
  mutate(vintage = floor(as.numeric(interval(date.status, as.Date("2021-11-30")) / dyears(1))))
df$range.date <- cut(df$vintage, breaks = gbv_ranges, labels = gbv_labels, include.lowest = TRUE)
r.p29.gbvByDefaultVintage <- df %>% select(gbv.residual, range.date) %>% group_by(range.date) %>% 
  summarise("GBV Total (m)" = round(sum(gbv.residual)/1e6,1),
            "GBV %" = round(sum(gbv.residual) / total.gbv *100, 1),
            "Loans %" = round(n() / nrow(df) * 100, 1),
            "Avg. Loan size (€k)" = round(sum(gbv.residual)/n()/1e3,1))

r.p29.g.gbvByDefaultVintage <- ggplot(r.p29.gbvByDefaultVintage, aes(x = range.date, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  geom_text(aes(label = paste0(`GBV %`, "%"), vjust = -0.1), size = 5) +
  labs(x = "Year Ranges", y = "GBV %", title = "GBV By Default Vintage") +
  ylim(0, 32) +
  annotate("text", x = 1:6, y = rep(0, 6), label = paste0(r.p29.gbvByDefaultVintage$`GBV Total (m)`, "M"), size = 5, hjust = 0.5, vjust = -25, color = "#054A53")
r.p29.g.gbvByDefaultVintage

r.p29.g.loansByDefaultVintage <- ggplot(r.p29.gbvByDefaultVintage, aes(x = range.date, y = `Loans %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  geom_text(aes(label = paste0(`GBV %`, "%"), vjust = -0.1), size = 5) +
  labs(x = "Year Ranges", y = "Loans %", title = "Loans By Default Vintage", subtitle = "and avg. loan size") +
  ylim(0, 32) +
  annotate("text", x = 1:6, y = rep(0, 6), label = paste0(r.p29.gbvByDefaultVintage$`Avg. Loan size (€k)`, "€k"), size = 5, hjust = 0.5, vjust = -24.5, color = "#054A53")
r.p29.g.loansByDefaultVintage


###-----------------------------------------------------------------------###
#-----                     Page 31 report                                -----         
###-----------------------------------------------------------------------###
garanzia <- garanzie_cleaned %>% rename(id.loan = numero.rapporto.garantito)
df <- merge(Loans, garanzia[,c("id.loan", "descrizione.garanzia")], by = "id.loan", all.x= TRUE)
df <- df %>%
  distinct(id.loan, .keep_all = TRUE)
df$guarantee.presence <- ifelse(!is.na(df$descrizione.garanzia), "guaranteed", "not guaranteed")
total.gbv <- sum(df$gbv.residual)

r.p31.gbvByGuaranteePresence.totals <- df %>% summarise(guarantee.presence = "total", "N Loans" = n(), "GBV %" = paste0(round(sum(gbv.residual) / total.gbv * 100, 1), "%"))

r.p31.gbvByGuaranteePresence <- df %>%
  #select(gbv.residual, guarantee.presence) %>%
  group_by(guarantee.presence) %>%
  summarise("N Loans" = n(), "GBV %" = paste0(round(sum(gbv.residual) / total.gbv * 100, 1), "%"))

r.p31.gbvByGuaranteePresence <- rbind(r.p31.gbvByGuaranteePresence.totals, r.p31.gbvByGuaranteePresence)
r.p31.gbvByGuaranteePresence <- r.p31.gbvByGuaranteePresence %>% rename("Guarantee Status"=guarantee.presence)



#gbv % per guaranteed and no guaranteed
custom_colors <- c("#054A53", "#ACDBE1")
r.p31.g.gbvByGuaranteePresence <- ggplot(r.p31.gbvByGuaranteePresence, aes(x = "", y = `GBV %`, fill = guarantee.presence)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "Guarantee Presence") +
  theme_void() +
  geom_text(aes(label = paste0(`GBV %`, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = custom_colors)
r.p31.g.gbvByGuaranteePresence


#gbv % per guarantee type chart
df1 <- df %>%
  filter(guarantee.presence == "guaranteed")
total.guaranteed.gbv <- sum(df1$gbv.residual)

df1 <- df1 %>%
  mutate(descrizione.garanzia = case_when(
    str_detect(descrizione.garanzia, "fideiussione")  ~ "surety",
    descrizione.garanzia == "effetto in bianco"  ~ "surety",
    TRUE ~ "pledge"  # Catch-all condition for "corporate"
  ))
r.p31.gbvByGuaranteedType.totals <- df1 %>% 
  summarise(descrizione.garanzia = "total", "N Loans" = n(), "GBV %" = paste0(round(sum(gbv.residual) / total.guaranteed.gbv *100, 1), "%"))

r.p31.gbvByGuaranteedType <- df1 %>% group_by(descrizione.garanzia) %>% 
  summarise("N Loans" = n(), "GBV %" = paste0(round(sum(gbv.residual) / total.guaranteed.gbv *100, 1), "%"))

r.p31.gbvByGuaranteedType <- rbind(r.p31.gbvByGuaranteedType.totals, r.p31.gbvByGuaranteedType)
r.p31.gbvByGuaranteedType <- r.p31.gbvByGuaranteedType %>% rename("Guarantee Type" = descrizione.garanzia)




r.p31.g.gbvByGuaranteedType <- ggplot(r.p31.gbvByGuaranteedType, aes(x = guarantee.type, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Guarantee Type", y = "GBV %", title= "GBV Residual % per Guarantee Type") + 
  geom_text(aes(label = `GBV %`), vjust = 1, size = 5)
r.p31.g.gbvByGuaranteedType
#custom_colors <- c("#054A53", "#ACDBE1", "#008197", "#28464B", "#003A44", "#595F60")
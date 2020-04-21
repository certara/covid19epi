library(socialmixr)
default_cm <- contact_matrix(polymod, age.limits = c(0, 10, 20, 30, 40, 50, 60, 70, 80))$matrix


library(wpp2019)
data(pop)

age_group_names <- rownames(default_cm)

pop_by_country <- rbind(data.frame(popF[c("country_code", "name", "age", "2020")], sex = "F"),
      data.frame(popM[c("country_code", "name", "age", "2020")], sex = "M")) %>%
  setNames(c("code", "country", "age", "pop", "sex")) %>%
  mutate(age = forcats::fct_collapse(age, "[0,10)" = c("0-4", "5-9"),
                                     "[10,20)" = c("10-14", "15-19"), 
                                     "[20,30)" = c("20-24", "25-29"),
                                     "[30,40)" = c("30-34", "35-39"),
                                     "[40,50)" = c("40-44", "45-49"),
                                     "[50,60)" = c("50-54", "55-59"),
                                     "[60,70)" = c("60-64", "65-69"),
                                     "[70,80)" = c("70-74", "75-79"),
                                     "80+" =   c("80-84", "85-89", "90-94", "95-99", "100+"))) %>%
  mutate(age = factor(age, levels = age_group_names)) %>%
  
  group_by(code, country, age) %>% 
  summarise(total = sum(pop*1000))

pbc_spread <- pop_by_country %>% spread(age, total) %>% ungroup()
countries <- as.character(pbc_spread$code)
names(countries) <- pbc_spread$country
pbc_spread <- pbc_spread %>% select(-country) %>% column_to_rownames("code")

save(countries, pbc_spread, default_cm, file = "transformed_data/demo_inputs.Rdata")

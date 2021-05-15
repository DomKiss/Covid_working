library(dplyr)
library(stringr)
library(ggplot2)


q1 <-
  "C:/Egyetem/HR/Data/1. found it difficult to concentrate_merge.xlsx"
q2 <-
  "C:/Egyetem/HR/Data/2. found that your family responsibilities_female.xlsx"
q3 <- "C:/Egyetem/HR/Data/3. work in freetime_merge.xlsx"
q4 <- "C:/Egyetem/HR/Data/4. prevented giving time_merge.xlsx"

import_and_clean <- function(file) {
  found_it_difficult_df <- read_excel(file)
  found_it_difficult_df$Country <-
    stringr::str_replace_all(found_it_difficult_df$Country, "\\*", "")
  colnames(found_it_difficult_df)[3] <- "Gender"
  found_it_difficult_df <-
    found_it_difficult_df %>% mutate(Gender_name = ifelse(Gender == 0, "Male", "Female"))
  return(found_it_difficult_df)
}
found_it_difficult_df <- import_and_clean(q1)
family_responsiilities <- import_and_clean(q2)
family_responsiilities <-
  merge(
    family_responsiilities,
    found_it_difficult_df %>% select(Country, GenderGap_index),
    by = "Country",
    all.x = TRUE,
    all.y = FALSE
  ) %>% unique
family_responsiilities <-
  family_responsiilities %>% filter(Country != "Total (EU27)")
family_responsiilities <- unique(family_responsiilities)
work_in_freetime <- import_and_clean(q3)
work_in_freetime <-
  merge(
    work_in_freetime,
    found_it_difficult_df %>% select(Country, GenderGap_index),
    by = "Country",
    all.x = TRUE,
    all.y = FALSE
  ) %>% unique
work_in_freetime <-
  work_in_freetime %>% filter(Country != "Total (EU27)")

prevented_family <- import_and_clean(q4)
prevented_family <-
  merge(
    prevented_family,
    found_it_difficult_df %>% select(Country, GenderGap_index),
    by = "Country",
    all.x = TRUE,
    all.y = FALSE
  ) %>% unique


#linear regression model average score vs gender, ggap index
lm_difficult_to <-
  lm(Average_score ~ Gender + GenderGap_index, data = found_it_difficult_df)
lm_family_resp <-
  lm(Average_score ~ Gender + GenderGap_index, data = family_responsiilities)
lm_work_freetime <-
  lm(Average_score ~ Gender + GenderGap_index, data = work_in_freetime)
lm_prevented_family <-
  lm(Average_score ~ Gender + GenderGap_index, data = prevented_family)
#only female
only_female <-
  found_it_difficult_df %>% filter(Gender_name == "Female")
lm_difficult_to_only_f <-
  lm(Average_score ~ GenderGap_index, data = only_female)
lm_resp_only_f <-
  lm(Average_score ~ GenderGap_index, data = family_responsiilities)

#plot difficult to
ggplot(found_it_difficult_df,
       aes(x = GenderGap_index, y = Average_score)) + geom_point() + facet_grid(cols =
                                                                                  vars(Gender_name)) +
  geom_smooth(method = lm, color = "black") #+ ggtitle("Difficult to concentrate because of family responsibilities")


#plot family responsibility
ggplot(family_responsiilities,
       aes(x = GenderGap_index, y = Average_score)) + geom_point() + facet_grid(cols =
                                                                                  vars(Gender_name)) +
  geom_smooth(method = lm, color = "black") #+ ggtitle("Family responsibilities prevented you from giving the time you should to your job")

#plot work_in_freetime
ggplot(work_in_freetime, aes(x = GenderGap_index, y = Average_score)) + geom_point() +
  facet_grid(cols = vars(Gender_name)) +
  geom_smooth(method = lm, color = "black") #+ ggtitle("How often have you worked in your free time to meet work demands?")

#prevented family
ggplot(prevented_family, aes(x = GenderGap_index, y = Average_score)) + geom_point() +
  facet_grid(cols = vars(Gender_name)) +
  geom_smooth(method = lm, color = "black") #+ ggtitle("Your job prevented you from giving the time you wanted to your family")
devtools::install_github("strengejacke/strengejacke")
library(sjPlot)
tab_model(lm_difficult_to)
tab_model(lm_family_resp)
tab_model(lm_work_freetime)
tab_model(lm_prevented_family)

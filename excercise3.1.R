clinic_raw = read_csv("clinic_raw.csv", skip = 2)
head(clinic_raw)
str(clinic_raw) # lists each column's name and type in one compact view
summary(clinic_raw)


ggplot(clinic_raw, aes(x = age)) +
  geom_histogram(binwidth = 5, na.rm = TRUE)

clinic_clean =
  clinic_raw %>%
  rename(id = patient_id) # patient_id -> id, values untouched
clinic_clean =
  clinic_clean %>%
  select(id, site, sex, age, visit_date, blood_pressure, weight_kg, notes)
clinic_clean =
  clinic_clean %>%
  filter(age >= 18 | is.na(age)) # keep adults, but don't drop missing ages

clinic_clean %>%
  group_by(site) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE)
    , n = n() # count number of rows in each group
  )

clinic_clean =
  clinic_clean %>%
  mutate(
    site = tolower(trimws(site)) # lowercase + strip whitespace
    , site = gsub("_", " ", site)  # replace underscore with space
  )

clinic_clean =
  clinic_clean %>%
  mutate(bp_upper = as.numeric(str_extract(blood_pressure, "^[0-9]+")) # digits before the "/"
    , bp_lower = as.numeric(str_extract(blood_pressure, "(?<=/)[0-9]+")) # digits after the "/"
  )
head(clinic_clean[, c("blood_pressure", "bp_upper", "bp_lower")])

clinic_clean %>% filter(grepl("follow", notes, ignore.case = TRUE))

clinic_clean =
  clinic_clean %>%
  mutate(
    weight_kg = na_if(weight_kg, "-") # turn placeholder "-" into real NA
    , weight_kg = as.numeric(weight_kg) # now safe to convert to numeric
    , visit_date = as.Date(visit_date, format = "%d/%m/%Y") # dd/mm/yyyy text -> Date
  )
head(clinic_clean)

clinic_clean =
  clinic_clean %>%
  mutate(
    sex = ifelse(
      toupper(sex) == "M", "Male"
      , ifelse(toupper(sex) == "F", "Female", NA) # nested ifelse for the third outcome
    )
  )

clinic_clean =
  clinic_clean %>%
  mutate(
    bp_category = case_when(
      is.na(bp_upper)   ~ NA_character_ # keep missing as missing
      , bp_upper < 120    ~ "Normal"
      , bp_upper < 140    ~ "Elevated"
      , TRUE              ~ "High" # catch-all for anything left
    )
  )
table(clinic_clean$bp_category, useNA = "always")

clinic_clean =
  clinic_clean %>%
  mutate(notes = clean_text(notes)) # reuse the function on the notes column
table(clinic_clean$notes, useNA = "always")

clinic_clean %>%
  select(id, bp_upper, bp_lower) %>%
  head(4) %>%
  tidyr::pivot_longer(
    cols = c(bp_upper, bp_lower) # the two columns to stack
    , names_to = "reading"         # new column naming which reading it was
    , values_to = "value"          # new column holding the actual number
  )

clinic_clean %>%
  group_by(site, sex) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sex, values_from = mean_age)

site_lookup = data.frame(
  site = c("site a", "site b", "site c", "site d")
  , region = c("North", "North", "South", "South")
)

clinic_clean =
  clinic_clean %>%
  left_join(site_lookup, by = "site") # attach region, keep every clinic_clean row
head(clinic_clean)

write_csv(clinic_clean, "clinic_clean.csv")


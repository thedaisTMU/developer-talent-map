#### Setup ####
setwd("report")
library(tidyverse)
library(readxl)
library(stringi)
library(stringr)
library(tools)
library(scales)
library(wordcloud)

data_dir <- "data"
mappings_dir <- "mappings"
if (!dir.exists("figures"))  dir.create("figures")
theme_set(theme_bw() +
              theme(text = element_text(color = "#0b2b48"),
                    axis.text = element_text(color = "#0b2b48"),
                    strip.text = element_text(color = "#0b2b48"),
                    plot.subtitle = element_text(face = "italic"),
                    panel.grid.major = element_line(color = "gray90"),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    axis.ticks = element_blank(),
                    legend.position = "none"))
update_geom_defaults("text", list(colour = "#0b2b48"))
plot_bar_width <- 0.75
plot_axis_padding = c(0.01, 0)
plot_grid_color <- "grey90"
plot_color <- "#e24585"
plot_colors_2_emphasis <- c("#e24585", "#99daea")
plot_colors_4_emphasis <- c("#e24585", "#4d8e9e", "#99daea", "#b3f4ff")
plot_colors_6 <- c("#960039", "#e24585", "#ffabeb", "#4d8e9e", "#99daea", "#b3f4ff")
plot_sizes_4_emphasis <- c(3, 1, 1, 1)
plot_subtitle_respondents_share <- "% of Survey Respondents"
plot_subtitle_visitors_share <- "% of Web Traffic to Stack Overflow"
plot_ylab_respondents_share <- "% of Respondents"
plot_ylab_visitors_share <- "% of Traffic"
save_plot <- function (file_name, file_path = "figures/",
                       file_formats = c("png", "svg"), plot_width = 8,
                       plot_height = 4, plot_scale = 1.2) {
    for (i in 1:length(file_formats)) {
        ggsave(paste0(file_path, file_name, ".", file_formats[i]),
               width = plot_width, height = plot_height, scale = plot_scale)
    }
}


#### Load and Tidy Data ####
# Unzip data
zip_files <- grep("\\.zip", list.files(data_dir), value = TRUE)
for (i in 1:length(zip_files)) {
    unzip_files <- unzip(paste(data_dir, zip_files[i], sep = "/"), list = TRUE) %>%
        filter(!grepl("MACOSX", Name)) %>%
        pull(Name)
    if (!all(unzip_files %in% list.files(data_dir))) {
        unzip(paste(data_dir, zip_files[i], sep = "/"), files = unzip_files,
              exdir = data_dir)
    }
}

# Load data
respondents_2014_raw <- read_csv(
    paste(data_dir, "2014 Stack Overflow Survey Responses.csv", sep = "/")) %>%
    slice(-1) %>%
    mutate(respondent_id = row_number())
respondent_languages_2014 <- respondents_2014_raw %>%
    select(respondent_id, 43:54) %>%
    gather(question, languages, -respondent_id) %>%
    filter(!is.na(languages)) %>%
    group_by(respondent_id) %>%
    summarise(languages = paste(languages, collapse = ";"))
respondents_2014 <- respondents_2014_raw %>%
    left_join(respondent_languages_2014, by = "respondent_id") %>%
    select(country = `What Country do you live in?`,
           gender = `What is your gender?`,
           industry = `How would you best describe the industry you currently work in?`,
           languages) %>%
    mutate(year = 2014)
respondents_2015_raw <- read_csv(
    paste(data_dir, "2015 Stack Overflow Developer Survey Responses.csv", sep = "/"),
    skip = 1) %>%
    mutate(respondent_id = row_number())
respondent_languages_2015 <- respondents_2015_raw %>%
    select(respondent_id, contains("Current Lang")) %>%
    gather(question, languages, -respondent_id) %>%
    filter(!is.na(languages)) %>%
    group_by(respondent_id) %>%
    summarise(languages = paste(languages, collapse = ";"))
respondents_2015 <- respondents_2015_raw %>%
    left_join(respondent_languages_2015, by = "respondent_id") %>%
    select(country = Country, gender = Gender, industry = Industry, languages,
           employment_status = `Employment Status`) %>%
    mutate(year = 2015)
respondents_2016 <- read_csv(
    paste(data_dir, "2016 Stack Overflow Survey Results", "2016 Stack Overflow Survey Responses.csv", sep = "/")) %>%
    select(country, gender, industry, languages = tech_do, employment_status) %>%
    mutate(year = 2016)
respondents_2017_private_raw <- read_csv(
    paste(data_dir, "dev_survey_2017_usable_private_label_encoding.csv", sep = "/"),
    col_types = cols(q0040_prompt = col_character(),
                     q0074_0014_0001 = col_character(),
                     q0074_0018_0001 = col_character()))
response_mappings_2017 <- read_excel(
    paste(data_dir, "layout_usable_private_file.xlsx", sep = "/"), skip = 2)
respondent_devroles_2017 <- respondents_2017_private_raw %>%
    setNames(gsub("^.*RespondentID.*$", "respondent_id", names(.))) %>%
    select(respondent_id, contains("q0029")) %>%
    gather(dev_role_code, selection, -respondent_id) %>%
    filter(selection == "Selected") %>%
    inner_join(response_mappings_2017 %>%
                   filter(grepl("q0029", Variable) & !is.na(Position)),
               by = c("dev_role_code" = "Variable")) %>%
    group_by(respondent_id) %>%
    summarise(dev_roles = paste(Label, collapse = ";"))
respondent_languages_2017 <- respondents_2017_private_raw %>%
    setNames(gsub("^.*RespondentID.*$", "respondent_id", names(.))) %>%
    select(respondent_id, contains("q0074")) %>%
    gather(language_code, selection, -respondent_id) %>%
    inner_join(response_mappings_2017 %>%
                   filter(grepl("q0074", Variable) & !is.na(Position)),
               by = c("language_code" = "Variable")) %>%
    mutate(language = ifelse(selection == "Worked with last year", Label,
                             ifelse(selection == 0, "", NA))) %>%
    filter(language != "") %>%
    group_by(respondent_id) %>%
    summarise(languages = paste(language, collapse = ";"))
respondent_informaleduc_2017 <- respondents_2017_private_raw %>%
    setNames(gsub("^.*RespondentID.*$", "respondent_id", names(.))) %>%
    select(respondent_id, contains("q0069")) %>%
    gather(informal_educ_code, selection, -respondent_id) %>%
    filter(selection == "Selected") %>%
    inner_join(response_mappings_2017 %>%
                   filter(grepl("q0069", Variable) & !is.na(Position)),
               by = c("informal_educ_code" = "Variable")) %>%
    group_by(respondent_id) %>%
    summarise(informal_educ = paste(Label, collapse = ";"))
respondent_ethnicities_2017 <- respondents_2017_private_raw %>%
    setNames(gsub("^.*RespondentID.*$", "respondent_id", names(.))) %>%
    select(respondent_id, contains("q0103")) %>%
    gather(ethnicity_code, selection, -respondent_id) %>%
    filter(selection == "Selected") %>%
    inner_join(response_mappings_2017 %>%
                   filter(grepl("q0103", Variable) & !is.na(Position)),
               by = c("ethnicity_code" = "Variable")) %>%
    group_by(respondent_id) %>%
    summarise(ethnicities = paste(Label, collapse = ";"))
respondents_2017_private <- respondents_2017_private_raw %>%
    setNames(gsub("^.*RespondentID.*$", "respondent_id", names(.))) %>%
    select(respondent_id, country = q0003, gender, industry = q0022,
           pro_status = q0001, web_dev_role = q0030, company_size = q0023,
           company_type = q0024, employment_status = q0017,
           salary_usd = current_salary_USD, local_currency_per_usd = salary_conversion,
           local_currency = q0063, local_currency_other = q0063_other,
           contains("q0029"), job_seeking_status = q0041, job_discovery_channel = q0061,
           educ_level = q0018,
           educ_level_group = ed_level, major = ed_major,
           unemployed_time_post_bootcamp = q0071, pro_experience = q0027) %>%
    mutate_all(funs(stri_trans_general(., "latin-ascii"))) %>%
    mutate_at(vars(respondent_id, salary_usd, local_currency_per_usd), funs(as.numeric)) %>%
    left_join(respondent_devroles_2017, by = "respondent_id") %>%
    left_join(respondent_languages_2017, by = "respondent_id") %>%
    left_join(respondent_informaleduc_2017, by = "respondent_id") %>%
    left_join(respondent_ethnicities_2017, by = "respondent_id") %>%
    mutate(year = 2017,
           company_size = gsub(" employees", "", company_size))

country_label_corrections <- read_csv(
    paste(mappings_dir, "country-label-corrections.csv", sep = "/"),
    col_types = cols(.default = col_character()))
country_metadata <- read_csv(
    paste(mappings_dir, "country-metadata.csv", sep = "/"),
    col_types = cols(.default = col_character()))
industry_consolidations <- read_csv(
    paste(mappings_dir, "industry-consolidations.csv", sep = "/"),
    col_types = cols(.default = col_character()))
employment_status_consolidations <- read_csv(
    paste(mappings_dir, "employment-status-consolidations.csv", sep = "/"),
    col_types = cols(.default = col_character()))
currency_mappings <- read_csv("mappings/currency-mappings.csv") %>%
    mutate(currency_name = tolower(trimws(gsub("\\(.*\\)", "", currency_name))))
country_ppps <- read_csv(paste(data_dir, "world-bank-ppp-rates.csv", sep = "/")) %>%
    setNames(c("original_country_label", "local_currency_per_intl_dollar")) %>%
    mutate(original_country_label = tolower(
        trimws(stri_trans_general(original_country_label, "latin-ascii")))) %>%
    left_join(country_label_corrections, by = "original_country_label") %>%
    mutate(country = ifelse(!is.na(corrected_country_label),
                            corrected_country_label, original_country_label)) %>%
    select(country, local_currency_per_intl_dollar)

respondents <- bind_rows(
    respondents_2014, respondents_2015, respondents_2016, respondents_2017_private) %>%
    mutate_at(vars(country, pro_status, dev_roles, web_dev_role,
                   employment_status, local_currency, local_currency_other,
                   industry, company_size, company_type, languages, 
                   educ_level, job_seeking_status, job_discovery_channel,
                   educ_level_group, unemployed_time_post_bootcamp,
                   pro_experience, major, gender),
              funs(tolower)) %>%
    mutate(local_currency = trimws(gsub("\\(.*\\)", "", local_currency))) %>%
    left_join(country_label_corrections,
              by = c("country" = "original_country_label")) %>%
    left_join(industry_consolidations,
              by = c("industry" = "industry_original")) %>%
    left_join(employment_status_consolidations,
              by = c("employment_status" = "employment_status_original")) %>%
    left_join(currency_mappings %>%
                  select(currency_name, local_currency_code_name = currency_code),
              by = c("local_currency" = "currency_name")) %>%
    left_join(currency_mappings %>%
                  left_join(country_metadata, by = "currency_code") %>%
                  select(country, local_currency_code_rate = currency_code, currency_per_usd),
              by = c("country", "local_currency_per_usd" = "currency_per_usd")) %>%
    rename(industry_original = industry,
           employment_status_original = employment_status) %>%
    mutate(country = ifelse(
        country %in% c("i prefer not to say", "n/a"), NA, ifelse(
            !is.na(corrected_country_label), corrected_country_label, country)),
        dev_role_desktop_mobile = grepl("desktop|mobile", dev_roles),
        dev_role_dbs_sysadmin = grepl("dba|sys.*admin", dev_roles),
        pro_experience_low_year = ifelse(
            pro_experience == "less than a year", 0,
            as.numeric(str_extract(pro_experience, "^[[:digit:]]+"))),
        pro_experience_high_year = ifelse(
            pro_experience == "less than a year", 1, ifelse(
                pro_experience == "20 or more years", 20,
                as.numeric(trimws(str_extract(pro_experience, " [[:digit:]]+"))))),
        pro_experience_mid_year = (pro_experience_low_year +
                                       pro_experience_high_year) / 2,
        pro_experience_bin = cut(
            pro_experience_low_year, breaks = seq(0, 25, by = 5), right = FALSE,
            labels = c("Under 5", "5 to 10", "10 to 15", "15 to 20", "Over 20")),
        pro_experience_bin_above_15 =  pro_experience_bin %in% c("15 to 20", "Over 20"),
        industry_consolidated = ifelse(
            grepl("(i prefer not to)|(not currently employed)|student",
                  industry_original), NA, ifelse(
                      is.na(industry_consolidated), industry_original, ifelse(
                          industry_consolidated == "n/a", NA,
                          industry_consolidated))),
        local_currency_code = ifelse(!is.na(local_currency_code_rate),
                                     local_currency_code_rate,
                                     ifelse(!is.na(local_currency_code_name),
                                            local_currency_code_name,
                                            ifelse(is.na(local_currency) &
                                                       nchar(local_currency_other) == 3,
                                                   local_currency_other, NA)))) %>%
    left_join(country_metadata, by = "country") %>%
    left_join(country_ppps, by = "country") %>%
    rename(country_currency_code = currency_code) %>%
    replace_na(list(is_eu = FALSE)) %>%
    mutate(region = factor(ifelse(country == "canada", "Canada",
                                  ifelse(country == "united states", "US",
                                         ifelse(is_eu, "EU", "ROW"))),
                           levels = c("Canada", "US", "EU", "ROW")),
           region_carow_label = factor(
               ifelse(country == "canada", "Canada", "ROW"),
               levels = c("Canada", "ROW")),
           salary_cad = salary_usd * 1.32,
           salary_local_currency = salary_usd * local_currency_per_usd,
           salary_intl_dollar = ifelse(
               country_currency_code == local_currency_code,
               salary_local_currency / local_currency_per_intl_dollar, NA)) %>%
    select(-contains("q0029"))

# Web traffic by developer role in Canada
devroles_visitors_2017_ca <- full_join(
    read_excel(paste(data_dir, "Brookfield_report_data_with_additions_4-19-2017.xlsx",
                     sep = "/"), sheet = "Canada Data", skip = 1) %>%
        setNames(c("dev_role", names(.)[-1])),
    read_csv(paste(data_dir, "flipped_Victoria_BC_city_report_08102017.csv",
                   sep = "/")) %>%
        setNames(c("dev_role", names(.)[-1])),
    by = "dev_role") %>%
    setNames(gsub("_+", "_", gsub("\\.", "", make.names(
        stri_trans_general(
            tolower(gsub(" ", "_", gsub(" *$", "", names(.)))), "latin-ascii"),
        unique = TRUE)))) %>%
    slice(1:20) %>%
    mutate(dev_role = tolower(dev_role))

# Web traffic by developer role internationally
devroles_visitors_2017_intl <- left_join(
    read_excel(paste(data_dir, "Brookfield_report_data_with_additions_4-19-2017.xlsx",
                     sep = "/"), sheet = "Rest of World Data", skip = 3),
    read_excel(paste(data_dir, "Brookfield_report_data_with_additions_4-19-2017.xlsx",
                     sep = "/"), sheet = "USA Data", skip = 1), by = "") %>%
    setNames(gsub(" ", "_", gsub("\\+ ", "", tolower(c("dev_role", names(.)[-1]))))) %>%
    select(-matches("%|\\.")) %>%
    slice(1:20) %>%
    mutate(dev_role = tolower(dev_role))

#### Country ####
country_years <- respondents %>%
    filter(!is.na(country)) %>%
    group_by(year, country, region, region_carow_label) %>%
    summarise(respondents = n()) %>%
    group_by(year) %>%
    mutate(respondents_share = respondents / sum(respondents)) %>%
    ungroup()
country_years_prodevs <- respondents %>%
    filter(!is.na(country) & pro_status == "pro dev") %>%
    group_by(year, country, region, region_carow_label) %>%
    summarise(respondents = n(),
              mean_salary_cad = mean(salary_cad, na.rm = TRUE),
              median_salary_cad = median(salary_cad, na.rm = TRUE),
              mean_salary_usd = mean(salary_usd, na.rm = TRUE),
              median_salary_usd = median(salary_usd, na.rm = TRUE),
              n_salary_responses = sum(ifelse(!is.na(salary_cad), TRUE, FALSE)),
              median_salary_intl_dollar = median(salary_intl_dollar, na.rm = TRUE),
              n_salary_intl_dollar_responses = sum(ifelse(!is.na(salary_intl_dollar), TRUE, FALSE)),
              mean_pro_experience_years = mean(pro_experience_mid_year, na.rm = TRUE),
              median_pro_experience_years = median(pro_experience_mid_year, na.rm = TRUE),
              respondents_share_pro_experience_bin_above_15 = sum(pro_experience_bin_above_15) /
                  sum(!is.na(pro_experience_bin))) %>%
    left_join(respondents %>%
                  filter(!is.na(country) & pro_status == "pro dev" &
                             gender %in% c("male", "female")) %>%
                  group_by(year, country, gender) %>%
                  summarise(median_salary_cad = median(salary_cad, na.rm = TRUE)) %>%
                  spread(gender, median_salary_cad) %>%
                  rename(median_salary_cad_males = male,
                         median_salary_cad_females = female),
              by = c("year", "country")) %>%
    left_join(respondents %>%
                  filter(!is.na(country) & pro_status == "pro dev" &
                             gender %in% c("male", "female")) %>%
                  group_by(year, country, gender) %>%
                  summarise(n_salary_responses = sum(
                      ifelse(!is.na(salary_cad), TRUE, FALSE))) %>%
                  spread(gender, n_salary_responses) %>%
                  rename(n_salary_responses_males = male,
                         n_salary_responses_females = female),
              by = c("year", "country")) %>%
    group_by(year) %>%
    mutate(respondents_share = respondents / sum(respondents),
           salary_pc_diff_females_males = median_salary_cad_females /
               median_salary_cad_males - 1) %>%
    ungroup()
region_years <- country_years %>%
    group_by(year, region) %>%
    summarise_at(vars(respondents, respondents_share), funs(sum))

# Plot of top country survey respondent shares in 2017
n_top_countries <- 10
plot_title <- paste("Share of Developers Among Top", n_top_countries,
                    "Countries, 2017")
country_years %>%
    filter(year == 2017) %>%
    top_n(n_top_countries, respondents_share) %>%
    arrange(respondents_share) %>%
    mutate(country = factor(
        country, levels = country, labels = toTitleCase(country))) %>%
    ggplot(aes(country, respondents_share, fill = region_carow_label,
               label = percent(respondents_share))) +
    geom_col(width = plot_bar_width) +
    geom_text(hjust = 0, nudge_y = 0.001) +
    coord_flip() +
    scale_fill_manual(values = plot_colors_2_emphasis) +
    scale_y_continuous(limits = c(0, 0.24), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = plot_ylab_respondents_share, title = plot_title,
         subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.y = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Plot of respondent share indices by country/region, 2014-2017
plot_title <- "Share of Developers Among Countries/Regions Over Time, 2014-2017"
region_years %>%
    group_by(region) %>% 
    mutate(respondents_share_ind = respondents_share /
               respondents_share[year == min(year)] * 100) %>%
    ungroup() %>% 
    mutate(region = factor(region, levels = c("ROW", "EU", "US", "Canada"))) %>%
    ggplot(aes(year, respondents_share_ind, color = region, size = region)) +
    geom_line() +
    scale_color_manual(values = rev(plot_colors_4_emphasis)) +
    scale_size_manual(values = rev(plot_sizes_4_emphasis)) +
    scale_y_continuous(limits = c(60, 140), expand = plot_axis_padding) +
    labs(x = "", y = "Index, 2014 = 100", color = "", size = "",
         title = plot_title, subtitle = "Index of % of Survey Responents, 2014 = 100") +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "right") +
    guides(color = guide_legend(reverse = TRUE), size = guide_legend(reverse = TRUE))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Growth from 2015 to 2017 among top countries
country_years %>%
    inner_join(country_years %>%
                   filter(year >= 2015) %>%
                   group_by(country) %>%
                   summarise(mean_respondents_share = mean(respondents_share)) %>%
                   top_n(15, mean_respondents_share),
               by = "country") %>%
    filter(year %in% c(2015, 2017)) %>%
    group_by(country) %>%
    summarise(respondents_share_pc_change = respondents_share[year == max(year)] /
                  respondents_share[year == min(year)] - 1) %>%
    arrange(desc(respondents_share_pc_change))

#### Province ####
province_consolidations <- read_csv(
    paste(mappings_dir, "province-consolidations.csv", sep = "/"),
    col_types = cols(.default = col_character()))
provinces_employment_2017 <- bind_rows(
    read_csv(paste(data_dir, "cansim-2820088-employment-province.csv", sep = "/")),
    read_csv(paste(data_dir, "cansim-2820100-employment-territory.csv", sep = "/")) %>%
        rename(GEOGRAPHY = GEO)) %>%
    mutate(province = tolower(gsub(" ", "_", GEOGRAPHY))) %>%
    group_by(province) %>%
    summarise(employees = mean(Value) * 100)
provincesconsolidated_2017 <- devroles_visitors_2017_ca %>%
    filter(dev_role == "all developers") %>%
    select(alberta:yukon) %>%
    gather(province, visitors) %>%
    left_join(provinces_employment_2017, by = "province") %>%
    left_join(province_consolidations, by = c("province" = "province_original")) %>%
    group_by(province_consolidated) %>%
    summarise(visitors = sum(visitors),
              employees = sum(employees)) %>%
    mutate(visitors_share = visitors / sum(visitors),
           employees_share = employees / sum(employees, na.rm = TRUE),
           visitor_employee_ratio = visitors_share / employees_share)

# Plot of province visitor shares
provinces_consolidated_plot_order <- c(
    "territories", "british columbia", "alberta", "saskatchewan", "manitoba",
    "ontario", "quebec", "atlantic canada")
plot_title <- "Share of Canadian Developers by Province, 2017"
provincesconsolidated_2017 %>%
    mutate(province_consolidated = factor(
        province_consolidated,
        levels = provinces_consolidated_plot_order,
        labels = toTitleCase(provinces_consolidated_plot_order))) %>%
    ggplot(aes(province_consolidated, visitors_share,
               label = percent(visitors_share))) +
    geom_col(fill = plot_color, width = plot_bar_width) +
    geom_text(vjust = 0, nudge_y = 0.005) +
    scale_y_continuous(limits = c(0, 0.5), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = plot_ylab_visitors_share, title = plot_title,
         subtitle = plot_subtitle_visitors_share) +
    theme(panel.grid.major.x = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Plot of province visitor share-employee share ratios
plot_title <- "Concentration of Canadian Developers by Province, 2017"
provincesconsolidated_2017 %>%
    mutate(province_consolidated = factor(
        province_consolidated,
        levels = provinces_consolidated_plot_order,
        labels = toTitleCase(provinces_consolidated_plot_order))) %>%
    ggplot(aes(province_consolidated, visitor_employee_ratio,
               label = format(round(visitor_employee_ratio, 2), n.small = 2))) +
    geom_col(fill = plot_color, width = plot_bar_width) +
    geom_text(vjust = 0, nudge_y = 0.01) +
    scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 2, by = 0.25),
                       expand = plot_axis_padding) +
    labs(x = "", y = "Ratio of % of Traffic to % of Employment", title = plot_title,
         subtitle = "Ratio of % of Web Traffic to Stack Overflow to % of National Employment") +
    theme(panel.grid.major.x = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

#### City ####
# Within Canada
city_so_mappings <- read_csv(paste(mappings_dir, "city-so-mappings.csv", sep = "/"))
city_statcan_mappings <- read_csv(paste(mappings_dir, "city-statcan-mappings.csv", sep = "/"))
visitors_2017_ca <- devroles_visitors_2017_ca %>%
    filter(dev_role == "all developers") %>%
    pull(canada)
# Scale factor used to help make city shares add to 100% theoretically
# Unlike for provinces, data we have doesn't collectively cover entire dev population
city_scale_factor <- visitors_2017_ca /
    provincesconsolidated_2017 %>%
    summarise(visitors = sum(visitors)) %>%
    pull(visitors)
cities_statcan_2017_ca <- bind_rows(
    read_csv(paste(data_dir, "cansim-2820135-employment-city.csv", sep = "/")) %>%
        select(city = GEO, value = Value) %>%
        mutate(variable = "employees"),
    read_csv(paste(data_dir, "cansim-0510056-population-city.csv", sep = "/")) %>%
        select(city = GEO, value = Value) %>%
        mutate(variable = "population")) %>%
    spread(variable, value) %>%
    filter(!grepl("(ontario|quebec) part", city, ignore.case = TRUE)) %>%
    mutate(city = gsub("Montr.*al", "montreal",
                       gsub("Qu.*bec", "quebec city",
                            gsub("-", " - ",
                                 gsub(",.*", "",
                                      gsub("Ottawa.*Gatineau.*", "ottawa - gatineau",
                                           gsub("Kitchener.*", "Kitchener - Cambridge - Waterloo",
                                                stri_trans_general(city, "latin-ascii"))))))),
           employees = employees * 1000) %>%
    group_by(city) %>%
    summarise(employees = sum(employees, na.rm = TRUE),
              population = sum(population, na.rm = TRUE))
cities_2017_ca <- inner_join(
    devroles_visitors_2017_ca %>%
        filter(dev_role == "all developers") %>%
        select(calgary:waterloo_kitchener_cambridge, victoria_bc) %>%
        gather(city, visitors) %>%
        inner_join(city_so_mappings, by = c("city" = "city_so")) %>%
        group_by(city_consolidated) %>%
        summarise(visitors = sum(visitors)),
    cities_statcan_2017_ca %>%
        full_join(city_statcan_mappings %>%
                       mutate(city_statcan = gsub("Montr.*al", "montreal",
                                                  gsub("Qu.*bec", "quebec city",
                                                       gsub("-", " - ",
                                                            gsub(",.*", "",
                                                                 gsub("Ottawa.*Gatineau.*", "ottawa - gatineau",
                                                                      gsub("Kitchener.*", "Kitchener - Cambridge - Waterloo",
                                                                           stri_trans_general(city_statcan, "latin-ascii")))))))),
                   by = c("city" = "city_statcan")) %>%
        group_by(city_consolidated) %>%
        summarise(employees = sum(employees),
                  population = sum(population)),
    by = "city_consolidated") %>%
    filter(!is.na(city_consolidated)) %>%
    mutate(visitors_scaled = visitors * city_scale_factor,
           visitors_share_scaled = visitors_scaled / visitors_2017_ca,
           employees_share = employees /
               (cities_statcan_2017_ca %>%
                    filter(city == "Canada") %>%
                    pull(employees)),
           visitor_employee_ratio = visitors_share_scaled / employees_share)

# Plot of Canadian cities share of web traffic
plot_title <- "Share of Canadian Developers by City, 2017"
cities_2017_ca %>%
    arrange(visitors_share_scaled) %>%
    mutate(city_consolidated_label = factor(
        city_consolidated,
        levels = city_consolidated,
        labels = toTitleCase(gsub("_", "-", city_consolidated)))) %>%
    ggplot(aes(city_consolidated_label, visitors_share_scaled,
               label = percent(visitors_share_scaled))) +
    geom_col(fill = plot_color, width = plot_bar_width) +
    geom_text(hjust = 0, nudge_y = 0.001) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 0.35), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = plot_ylab_visitors_share, title = plot_title,
         subtitle = plot_subtitle_visitors_share) +
    theme(panel.grid.major.y = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Table of Canadian cities in 2017 -- for interactive web viz
cities_2017_ca %>%
    select(city = city_consolidated, visitors, visitors_scaled,
           visitors_share_scaled, employees, employees_share,
           visitor_employee_ratio) %>%
    write_csv(paste(data_dir, "cities-ca.csv", sep = "/"))

# Plot Canadian city visitor share-employee share ratios
plot_title <- "Concentration of Canadian Developers by City, 2017"
cities_2017_ca %>%
    arrange(visitor_employee_ratio) %>%
    mutate(city_consolidated_label = factor(
        city_consolidated,
        levels = city_consolidated,
        labels = toTitleCase(gsub("_", "-", city_consolidated)))) %>%
    ggplot(aes(city_consolidated_label, visitor_employee_ratio,
               label = format(round(visitor_employee_ratio, 2), n.small = 2))) +
    geom_col(fill = plot_color, width = plot_bar_width) +
    geom_text(hjust = 0, nudge_y = 0.005) +
    geom_hline(yintercept = 1, color = "gray50", linetype = "dashed") +
    scale_y_continuous(limits = c(0, 2), expand = plot_axis_padding) +
    coord_flip() +
    labs(x = "", y = "Ratio of % of Traffic to % of Employment",
         title = plot_title,
         subtitle = "Ratio of % of Web Traffic to Stack Overflow to % of National Employment") +
    theme(panel.grid.major.y = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Visitors share for 3 most populous Canadian cities
sum(cities_2017_ca %>% top_n(3, population) %>% pull(visitors_share_scaled))
sum(cities_2017_ca %>% top_n(3, population) %>% pull(employees_share))

# International cities
city_years <- read_excel(
    paste(data_dir, "global_developer_counts_3-year_trend_with_countries.xlsx",
          sep = "/"), sheet = "city_data") %>%
    select(city = CityName, country = CountryName, `2015` = Developers2015,
           `2016` = Developers2016, `2017` = Developers2017) %>%
    mutate_at(vars(city:country), funs(tolower)) %>%
    gather(year, visitors, -city, -country)

# Plot of international city comparison
visitors_2017_to <- city_years %>%
    filter(year == 2017 & city == "toronto") %>%
    pull(visitors)
n_top_cities <- 20
plot_title <- "Developers Among Top 20 and Other Canadian Cities, 2017"
bind_rows(city_years %>%
              filter(year == 2017) %>%
              top_n(n_top_cities, visitors) %>%
              mutate(city_group = paste("Top", n_top_cities, "Cities")),
          city_years %>%
              filter(year == 2017 & country == "canada") %>%
              mutate(city_group = paste0("Other Canadian Cities"))) %>%
    distinct(city, country, .keep_all = TRUE) %>%
    arrange(visitors) %>%
    mutate(city_label = ifelse(
        city == "london" & country == "united kingdom", "London, UK", ifelse(
            city == "london" & country == "canada", "London, ON", ifelse(
                city == "central district", "Hong Kong", ifelse(
                    city == "waterloo", "Kitchener-Waterloo", ifelse(
                        grepl("qu.*bec", city), "Quebec City", toTitleCase(city)))))),
        n_char_city = nchar(city),
        city_label = ifelse(max(row_number()) - row_number() + 1 <= n_top_cities,
                            paste0(max(row_number()) - row_number() + 1, ". ", city_label, "         "),
                            city_label),
        city_label = factor(city_label, levels = city_label),
        visitors_ind = visitors / visitors_2017_to,
        city_group = factor(city_group, levels = rev(unique(city_group))))  %>%
    ggplot(aes(city_label, visitors_ind, fill = country == "canada",
               label = paste0(format(round(visitors_ind, 2), n.small = 2), "x"))) +
    facet_grid(city_group ~ ., scales = "free_y", space = "free") +
    geom_col(width = plot_bar_width) +
    geom_text(hjust = 0, nudge_y = 0.005) +
    geom_hline(yintercept = 1, col = "gray50", linetype = "dashed") +
    coord_flip() +
    scale_fill_manual(values = rev(plot_colors_2_emphasis)) +
    scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 4),
                       labels = paste0(seq(0, 4), "x")) +
    labs(x = "", y = "Traffic Relative to Toronto", title = plot_title,
         subtitle = "Web Traffic to Stack Overflow Relative to Toronto Traffic") +
    guides(fill = "none") +
    theme(panel.grid.major.y = element_line(NA),
          panel.spacing.y = unit(1, "lines"),
          axis.text.y = element_text(hjust = 0),
          strip.background = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title), plot_height = 5.5)

# Canadian city ranks in 2017 -- not valid for lowest ranking cities because incomplete data set
city_years %>%
    filter(year == 2017) %>%
    mutate(rank = rank(desc(visitors), ties.method = "min")) %>%
    filter(country == "canada")

# International city growth from 2016 to 2017
min_visitors_city_2016 <- 1e5
plot_title <- "Growth in Developers Among Large Developer Centres, 2017"
city_years %>%
    spread(year, visitors) %>%
    mutate(pc_growth_2017 = `2017` / `2016` - 1) %>%
    filter(`2016` >= min_visitors_city_2016) %>%
    arrange(pc_growth_2017) %>%
    mutate(city_label = factor(
        city, levels = city,
        labels = toTitleCase(
            ifelse(city == "central district", "hong kong",
                   ifelse(city == "london" & country == "united kingdom",
                          "London, UK", city))))) %>%
    ggplot(aes(city_label, pc_growth_2017, fill = city == "toronto",
               label = percent(pc_growth_2017))) +
    geom_col(width = plot_bar_width) +
    geom_text(hjust = 0, nudge_y = 0.001) +
    coord_flip() +
    scale_fill_manual(values = rev(plot_colors_2_emphasis)) +
    scale_y_continuous(limits = c(-0.06, 0.25), breaks = seq(-0.05, 0.3, by = 0.05),
                       labels = percent_format(), expand = plot_axis_padding) +
    labs(x = "", y = "% Growth", title = plot_title,
         subtitle = "% Growth Year-Over-Year in Web Traffic to Stack Overflow") +
    guides(fill = "none") +
    theme(panel.grid.major.x = element_line(plot_grid_color),
          panel.grid.major.y = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title), plot_height = 5.5)

# Table of international cities over 2015-2017 data -- for interactive web viz
city_years %>% write_csv(paste(data_dir, "city-years-intl.csv", sep = "/"))

#### Professional Status ####
# Professional statuses in Canada, 2017
prostatuses_2017_ca <- respondents %>%
    filter(year == 2017 & country == "canada") %>%
    group_by(pro_status) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / sum(respondents)) %>%
    inner_join(data_frame(
        pro_status = c(
            "ex-pro dev", "pro dev", "pro non-dev", "pure hobbyist", "student"),
        pro_status_label = c(
            "Ex-Professional Developer", "Professional Developer",
            "Professional Non-Developer", "Other", "Student")),
        by = "pro_status")

prostatuses_2017_ca

#### Developer Roles ####
dev_role_survey_mappings <- read_csv(paste(mappings_dir, "dev-role-survey-mappings.csv", sep = "/"))
respondent_devroles_2017_ca_prodevs <- respondents %>%
    filter(year == 2017 & country == "canada" & !is.na(dev_roles) &
               pro_status == "pro dev") %>%
    separate_rows(dev_roles, sep = ";") %>%
    select(respondent_id, dev_role = dev_roles)
n_respondents_devrole_2017_ca_prodevs <- length(unique(
    respondent_devroles_2017_ca_prodevs$respondent_id))
devroles_2017_ca_prodevs <- respondent_devroles_2017_ca_prodevs %>%
    group_by(dev_role) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / n_respondents_devrole_2017_ca_prodevs)

# Plot of dev role shares
plot_title <- "Share of Canadian Professional Developers by Role, 2017"
devroles_2017_ca_prodevs %>%
    left_join(dev_role_survey_mappings, by = "dev_role") %>%
    arrange(respondents_share) %>%
    mutate(dev_role_label = ifelse(
        grepl("statistics", dev_role_label, ignore.case = TRUE),
        "Developer with a Statistics\nor Mathematics Background",
        ifelse(grepl("embedded", dev_role_label, ignore.case = TRUE),
               "Embedded Applications/\nDevices Developer",
               ifelse(grepl("web developer", dev_role_label, ignore.case = TRUE),
                      "Web Developer (Front-End,\nBack-End, or Full-Stack)",
                      dev_role_label))),
        dev_role_label = factor(dev_role_label, levels = dev_role_label)) %>% 
    ggplot(aes(dev_role_label, respondents_share,
               label = percent(respondents_share))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(hjust = 0, nudge_y = 0.005) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 0.81), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "", title = plot_title,
         subtitle = plot_subtitle_respondents_share) +
    guides(fill = "none") +
    theme(panel.grid.major.y = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Plot of number of developer roles
plot_title <- "Share of Canadian Professional Developers by Number of Roles, 2017"
respondent_devroles_2017_ca_prodevs %>%
    group_by(respondent_id) %>%
    summarise(n_dev_roles = n()) %>%
    group_by(n_dev_roles) %>% 
    summarise(n_dev_roles_share = n() / nrow(.)) %>% 
    ggplot(aes(n_dev_roles, n_dev_roles_share, label = percent(n_dev_roles_share))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(vjust = 0, nudge_y = 0.005) +
    scale_x_continuous(limits = c(0.5, 8.5), breaks = seq(1, 8)) +
    scale_y_continuous(limits = c(0, 0.5), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "Number of Roles", y = plot_ylab_respondents_share, 
         title = plot_title, subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.x = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Desktop or mobile developer
respondent_devroles_2017_ca_prodevs %>%
    group_by(respondent_id) %>%
    summarise(desktop_or_mobile_dev_role = ifelse(
        sum(grepl("desktop|mobile", dev_role)) > 0, TRUE, FALSE)) %>%
    summarise(sum(desktop_or_mobile_dev_role) / n()) 

# Share of Canadian professional developers with more than one developer role
respondent_devroles_2017_ca_prodevs %>%
    group_by(respondent_id) %>%
    summarise(n_dev_roles = n()) %>%
    summarise(sum(n_dev_roles > 1) / nrow(.))
 
# Web developer types
respondent_webdevroles_2017_ca_prodevs <- respondents %>%
    filter(year == 2017 & country == "canada" & !is.na(web_dev_role) &
               pro_status == "pro dev") %>%
    separate_rows(dev_roles, sep = ";") %>%
    select(respondent_id, web_dev_role)
n_respondents_webdevrole_2017_ca_prodevs <- length(unique(
    respondent_webdevroles_2017_ca_prodevs$respondent_id))
webdevroles_2017_ca_prodevs <- respondent_webdevroles_2017_ca_prodevs %>%
    group_by(web_dev_role) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / n_respondents_webdevrole_2017_ca_prodevs)

# Plot of web developer roles
plot_title <- "Share of Canadian Professional Web Developers by Type, 2017"
webdevroles_2017_ca_prodevs %>%
    arrange(respondents_share) %>%
    mutate(web_dev_role_label = factor(
        web_dev_role, levels = web_dev_role,
        labels = toTitleCase(gsub("full stack", "full-stack", web_dev_role)))) %>%
    ggplot(aes(web_dev_role_label, respondents_share,
               label = percent(respondents_share))) +
    geom_col(fill = plot_color, width = plot_bar_width) +
    geom_text(hjust = 0, nudge_y = 0.005) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 0.75), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "", title = plot_title,
         subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.y = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Developers and cities
dev_role_groups <- c("all developers", "mobile developers", "web developers",
                     "other kinds of developers")
unusable_dev_roles <- c("biz intel developers", "highly technical designers",
                        "highly technical product managers", "qa engineers")
devrole_cities_2017_ca <- devroles_visitors_2017_ca %>%
    filter(!(dev_role %in% unusable_dev_roles)) %>%
    select(dev_role, calgary:waterloo_kitchener_cambridge, victoria_bc) %>%
    gather(city, visitors, -dev_role) %>%
    inner_join(city_so_mappings, by = c("city" = "city_so")) %>%
    group_by(dev_role, city_consolidated) %>%
    summarise(visitors = sum(visitors)) %>%
    inner_join(cities_2017_ca %>%
                   select(city_consolidated, total_visitors_city = visitors),
               by = "city_consolidated") %>%
    inner_join(devroles_visitors_2017_ca %>%
                   select(dev_role, visitors_ca = canada) %>%
                   mutate(visitors_share_ca = visitors_ca /
                              visitors_ca[dev_role == "all developers"]),
               by = "dev_role") %>%
    left_join(read_csv(paste(mappings_dir, "dev-role-traffic-mappings.csv", sep = "/")),
              by = "dev_role") %>% 
    mutate(visitors_share = visitors / total_visitors_city,
           location_quotient = visitors_share / visitors_share_ca) %>%
    ungroup()
n_top_cities_dev_role <- 5
dev_role_label_plot_order <- c(
    "Front-End Web Developer", "Back-End Web Developer", "Full-Stack Web Developer",
    "Android Developer", "iOS Developer", "Desktop Developer",
    "Database Administrator","Systems Administrator", "Embedded Developer",
    "Graphics Programmer", "Data Scientist", "Machine Learning Specialist")

# Plot of top developer roles by Canadian city
plot_title <- "Concentration of Canadian Developer Roles by City, 2017"
devrole_cities_2017_ca %>%
    filter(dev_role_label %in% dev_role_label_plot_order) %>%
    arrange(location_quotient) %>%
    mutate(dev_role_city = factor(
        paste(dev_role, city_consolidated, sep = "_"),
        levels = paste(dev_role, city_consolidated, sep = "_"),
        labels = toTitleCase(gsub("_", "-", city_consolidated))),
        dev_role_label = factor(dev_role_label, levels = dev_role_label_plot_order)) %>%
    group_by(dev_role) %>%
    top_n(n_top_cities_dev_role, location_quotient) %>%
    ggplot(aes(dev_role_city, location_quotient,
               label = format(round(location_quotient, 2), n.small = 2))) +
    facet_wrap(~ dev_role_label, scales = "free_y", nrow = 4) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(hjust = 0, nudge_y = 0.005) +
    geom_hline(yintercept = 1, color = "gray50", linetype = "dashed") +
    coord_flip() +
    scale_y_continuous(limits = c(0, 4), expand = plot_axis_padding) +
    labs(x = "", y = "Location Quotient", title = plot_title,
         subtitle = "Location Quotients Based on % of Web Traffic to Stack Overflow") +
    theme(panel.grid.major.y = element_line(NA),
          strip.background = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title), plot_height = 6)

# Developers roles by international city
devrole_cities_2017_intl <- devroles_visitors_2017_intl %>%
    gather(city, visitors, -dev_role) %>%
    inner_join(read_csv("mappings/intl-city-country-mappings.csv"), by = "city") %>%
    group_by(city) %>%
    mutate(visitors_share = visitors / visitors[dev_role == "all developers"]) %>%
    inner_join(devroles_visitors_2017_intl %>%
                   gather(country, visitors, -dev_role) %>%
                   inner_join(read_csv("mappings/intl-city-country-mappings.csv"),
                              by = "country") %>%
                   group_by(country) %>%
                   mutate(visitors_share = visitors / visitors[dev_role == "all developers"]),
               by = c("dev_role", "city", "country"),
               suffix = c("_city", "_country")) %>%
    mutate(location_quotient = visitors_share_city / visitors_share_country)

# Developer roles by city -- for interactive web viz
bind_rows(devrole_cities_2017_ca %>%
              mutate(region = "canada") %>%
              rename(city = city_consolidated),
          devrole_cities_2017_intl %>%
              mutate(region = "international") %>%
              rename(visitors_share = visitors_share_city)) %>%
    mutate(dev_role_parent_group = ifelse(
        dev_role %in% c("all developers"),
        "-", ifelse(
            dev_role %in% c("mobile developers", "web developers", "other kinds of developers"),
            "all developers", ifelse(
                dev_role %in% c("android developers", "ios developers"),
                "mobile developers", ifelse(
                    dev_role %in% c("back-end developers", "front-end developers", "full-stack developers"),
                    "web developers", "other kinds of developers"))))) %>%
    select(city, region, dev_role, dev_role_parent_group, visitors,
           city_visitors_share = visitors_share, location_quotient) %>%
    arrange(region, city, dev_role_parent_group, dev_role) %>%
    write_csv(paste(data_dir, "devroles-city.csv", sep = "/"))

# Developer roles by province -- for interactive web viz
devroles_visitors_2017_ca %>%
    filter(!(dev_role %in% unusable_dev_roles)) %>%
    select(dev_role, alberta:yukon) %>%
    gather(province, visitors, -dev_role) %>%
    inner_join(devroles_visitors_2017_ca %>%
                   filter(dev_role == "all developers") %>%
                   select(alberta:yukon) %>%
                   gather(province, visitors) %>%
                   rename(visitors_province = visitors),
               by = "province") %>%
    inner_join(devroles_visitors_2017_ca %>%
                   select(dev_role, visitors_ca = canada) %>%
                   mutate(visitors_share_ca = visitors_ca /
                              visitors_ca[dev_role == "all developers"]),
               by = "dev_role") %>%
    mutate(visitors_share = visitors / visitors_province,
           location_quotient = visitors_share / visitors_share_ca,
           dev_role_parent_group = ifelse(
               dev_role %in% c("all developers"),
               "-", ifelse(
                   dev_role %in% c("mobile developers", "web developers", "other kinds of developers"),
                   "all developers", ifelse(
                       dev_role %in% c("android developers", "ios developers"),
                       "mobile developers", ifelse(
                           dev_role %in% c("back-end developers", "front-end developers", "full-stack developers"),
                           "web developers", "other kinds of developers"))))) %>%
    select(province, dev_role, dev_role_parent_group, visitors,
           province_visitors_share = visitors_share, location_quotient) %>%
    arrange(province, dev_role_parent_group, dev_role) %>%
    write_csv(paste(data_dir, "devroles-province.csv", sep = "/"))

#### Languages ####
language_metadata <- read_csv(paste(mappings_dir, "language-metadata.csv", sep = "/"))

# Languages by Canada/ROW
language_regionscarow_2017_prodevs <- respondents %>%
    filter(year == 2017 & !is.na(region_carow_label) & !is.na(languages) &
               pro_status == "pro dev") %>%
    separate_rows(languages, sep = ";") %>%
    group_by(language = languages, region_carow_label) %>%
    summarise(respondents = n()) %>%
    left_join(language_metadata, by = "language") %>%
    left_join(respondents %>%
                  filter(year == 2017 & !is.na(region_carow_label) & !is.na(languages) &
                             pro_status == "pro dev") %>%
                  group_by(region_carow_label) %>%
                  summarise(total_respondents = n()),
              by = "region_carow_label") %>%
    mutate(respondents_share = respondents / total_respondents) %>% 
    ungroup()

# Plot of languages wordcloud
n_top_languages <- 10
set.seed(17)
png(filename = paste("figures/Share of Canadian Professional Developers Using Top",
                     n_top_languages, "Languages  2017 Wordcloud.png"),
    width = 1000, height = 1000, res = 300)
language_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada") %>%
    top_n(n_top_languages, respondents_share) %>% 
    with(wordcloud(
        language_label, respondents_share, colors = "#0b2b48", rot.per = 0.25,
        scale = c(max(respondents_share) / min(respondents_share) / 2, 1 / 2)))
dev.off()

set.seed(17)
svg(filename = paste("figures/Share of Canadian Professional Developers Using Top",
                     n_top_languages, "Languages  2017 Wordcloud.svg"))
language_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada") %>%
    top_n(n_top_languages, respondents_share) %>% 
    with(wordcloud(
        language_label, respondents_share, colors = "#0b2b48", rot.per = 0.25,
        scale = c(max(respondents_share) / min(respondents_share) / 2, 1 / 2)))
dev.off()

# Plot of top languages in Canada, 2017
language_label_plot_order <- language_regionscarow_2017_prodevs %>%
    select(language_label, respondents_share, region_carow_label) %>%
    spread(region_carow_label, respondents_share) %>%
    ungroup() %>%
    mutate(canada_rank = rank(desc(Canada)),
           row_rank = rank(desc(ROW))) %>%
    arrange(desc(canada_rank)) %>%
    top_n(-n_top_languages, canada_rank) %>%
    pull(language_label)
plot_title <- paste("Share of Canadian Professional Developers Using Top",
                    n_top_languages, "Languages, 2017")
language_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada" & language_label %in% language_label_plot_order) %>%
    arrange(respondents_share) %>%
    mutate(language_label = factor(language_label, levels = language_label_plot_order)) %>%
    ggplot(aes(language_label, respondents_share, label = percent(respondents_share))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(hjust = 0, nudge_y = 0.005) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 0.73), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = plot_ylab_respondents_share, title = plot_title,
         subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.y = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Share of Canadian professional developers using JavaScript in 2017
language_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada" & language == "javascript") %>%
    select(respondents_share)

# Plot of number of languages used, 2017
y_lim <- 10.5
plot_title <- paste("Share of Canadian Professional Developers by Number of",
                    "Languages Used, 2017")
respondents %>%
    filter(year == 2017 & country == "canada" & !is.na(languages) &
               pro_status == "pro dev") %>%
    separate_rows(languages, sep = ";") %>%
    group_by(respondent_id) %>%
    summarise(n_languages = n()) %>%
    group_by(n_languages) %>% 
    summarise(n_languages_share = n() / nrow(.)) %>% 
    ggplot(aes(n_languages, n_languages_share, label = percent(n_languages_share))) +
    geom_col(width = plot_bar_width, fill = plot_color) + 
    geom_text(vjust = 0, nudge_y = 0.001) +
    scale_x_continuous(limits = c(0.5, y_lim), breaks = seq(1, 10)) +
    scale_y_continuous(limits = c(0, 0.25), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "Number of Languages", y = plot_ylab_respondents_share,
         title = plot_title, subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.x = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Number of observations removed from previous plot
respondents %>%
    filter(year == 2017 & country == "canada" & !is.na(languages) &
               pro_status == "pro dev") %>%
    separate_rows(languages, sep = ";") %>%
    group_by(respondent_id) %>%
    summarise(n_languages = n()) %>%
    filter(n_languages > y_lim) %>%
    nrow()

# Share of Canadians using at least certain amount of languages
n_langs <- 2
respondents %>%
    filter(year == 2017 & country == "canada" & !is.na(languages) &
               pro_status == "pro dev") %>%
    separate_rows(languages, sep = ";") %>%
    group_by(respondent_id) %>%
    summarise(n_languages = n()) %>%
    group_by(n_languages) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / sum(respondents)) %>%
    filter(n_languages >= n_langs) %>%
    summarise(sum(respondents_share))

# Language-country-years
language_years_ca <- respondents %>%
    filter(country == "canada" & !is.na(languages)) %>%
    separate_rows(languages, sep = ";") %>%
    mutate(language = trimws(languages)) %>%
    group_by(year, language) %>%
    summarise(respondents = n()) %>%
    inner_join(respondents %>%
                   filter(country == "canada" & !is.na(languages)) %>%
                   group_by(year) %>%
                   summarise(total_respondents = n()),
               by = c("year")) %>%
    mutate(respondents_share = respondents / total_respondents) %>%
    group_by(language) %>%
    mutate(respondents_share_ind = respondents_share /
               respondents_share[year == min(year)] * 100,
           respondents_share_pc_growth_yoy = respondents_share /
               lag(respondents_share) - 1) %>%
    left_join(language_metadata, by = "language")

languages_ca <- language_years_ca %>%
    group_by(language, language_label) %>%
    summarise(avg_respondents_share = mean(respondents_share),
              survey_years = n()) %>%
    ungroup()

# Plot of share of developers working with widely used languages over time
min_avg_respondents_share_language <- 0.1
top_languages_ca <- languages_ca %>%
    filter(survey_years == max(survey_years) &
               avg_respondents_share >= min_avg_respondents_share_language) %>%
    arrange(desc(avg_respondents_share))
plot_title <- "Share of Canadian Developers Using Widely Used Languages Over Time, 2014-2017"
language_years_ca %>%
    filter(language %in% top_languages_ca$language) %>%
    mutate(language_label = factor(
        language_label,
        levels = top_languages_ca$language_label)) %>%
    ggplot(aes(year, respondents_share_ind)) +
    geom_line(color = plot_color, size = plot_sizes_4_emphasis[1]) +
    facet_wrap(~ language_label, nrow = 2) +
    scale_y_continuous(limits = c(80, 125), expand = plot_axis_padding) +
    labs(x = "", y = "Index, 2014 = 100", title = plot_title,
         subtitle = "Index of % of Survey Responents, 2014 = 100") +
    theme(panel.grid.major.x = element_blank(),
          strip.background = element_blank(),
          panel.spacing.x = unit(1.5, "lines"))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Highest language usage growth over past year
language_years_ca %>%
    filter(language %in% top_languages_ca$language & year == 2017) %>%
    arrange(desc(respondents_share_pc_growth_yoy)) %>%
    select(language, respondents_share, respondents_share_pc_growth_yoy)

# Difference between Canada and ROW in language usage plot
min_respondents_language <- 50
plot_title <- paste("Difference in Share of Professional Developers Using",
                    "Lanuage Between Canada and Rest of World")
language_regionscarow_2017_prodevs %>%
    select(language, region_carow_label, respondents_share) %>%
    spread(region_carow_label, respondents_share) %>%
    mutate(respondents_share_pc_diff = Canada / ROW - 1) %>%
    inner_join(language_regionscarow_2017_prodevs %>%
                   filter(region_carow_label == "Canada" &
                              respondents >= min_respondents_language) %>%
                   select(language),
               by = "language") %>%
    left_join(language_metadata, by = "language") %>%
    arrange(respondents_share_pc_diff) %>%
    ungroup() %>%
    mutate(language_label = factor(language_label, levels = language_label)) %>%
    ggplot(aes(language_label, respondents_share_pc_diff,
               label = percent(respondents_share_pc_diff))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(hjust = 0, nudge_y = 0.001) +
    coord_flip() +
    scale_fill_manual(values = plot_colors_2_emphasis) +
    scale_y_continuous(limits = c(-0.15, 0.35), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = "% Difference", title = plot_title,
         subtitle = "% Difference Between Canada and Rest of World in Share of Survey Respondents") +
    guides(fill = "none") +
    theme(panel.grid.major.x = element_line(plot_grid_color),
          panel.grid.major.y = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

#### Company Type and Size ####
omit_company_types_sizes <- c("i don't know", "i prefer not to answer")

# Company type
companytype_regionscarow_2017_prodevs <- respondents %>%
    mutate(company_type = ifelse(company_type %in% omit_company_types_sizes, NA,
                                 ifelse(company_type == "something else: ",
                                        "other", company_type))) %>%
    filter(year == 2017 & !is.na(region_carow_label) & !is.na(company_type) &
               pro_status == "pro dev") %>%
    group_by(region_carow_label, company_type) %>%
    summarise(respondents = n(),
              median_salary_cad = median(salary_cad, na.rm = TRUE),
              n_salary_responses = sum(ifelse(!is.na(salary_cad), TRUE, FALSE))) %>%
    left_join(read_csv(paste(mappings_dir, "company-type-mappings.csv", sep = "/")),
              by = "company_type") %>% 
    group_by(region_carow_label) %>%
    mutate(respondents_share = respondents / sum(respondents),
           company_type_label = ifelse(
               grepl("non-profit", company_type),
               "Non-profit/non-governmental organization\nor private school/university",
               ifelse(grepl("government agency", company_type),
                      "Government agency\nor public school/university",
                      ifelse(grepl("proprietorship", company_type),
                             "Sole proprietorship or partnership,\nnot in startup mode",
                             ifelse(grepl("privately-held", company_type),
                                    "Privately-held limited company,\nnot in startup mode",
                                    company_type_label))))) %>% 
    ungroup()

# Plot of company type and Canada v. ROW, 2017
plot_title <- "Share of Professional Developers by Company Type, 2017"
companytype_regionscarow_2017_prodevs %>%
    filter(company_type != "other") %>%
    arrange(respondents_share) %>%
    mutate(company_type_label = factor(company_type_label, levels = company_type_label),
        region_carow_label = factor(region_carow_label, levels = c("ROW", "Canada"))) %>%
    ggplot(aes(company_type_label, respondents_share, fill = region_carow_label,
               label = percent(respondents_share))) +
    geom_col(width = plot_bar_width, position = position_dodge()) +
    geom_text(hjust = 0, position = position_dodge(width = plot_bar_width)) +
    coord_flip() +
    scale_fill_manual(values = rev(plot_colors_2_emphasis)) +
    scale_y_continuous(limits = c(0, 0.6), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "", title = plot_title,
         subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "right") +
    guides(fill = guide_legend(reverse = TRUE))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Comparison
companytype_regionscarow_2017_prodevs %>%
    select(region_carow_label, company_type, respondents_share) %>%
    spread(region_carow_label, respondents_share)

# Company sizes by region
companysize_regions_2017_prodevs <- respondents %>%
    mutate(company_size = ifelse(company_size %in% omit_company_types_sizes,
                                 NA, company_size)) %>%
    filter(year == 2017 & !is.na(region) & !is.na(company_size) &
               pro_status == "pro dev") %>%
    group_by(region, company_size) %>%
    summarise(respondents = n()) %>%
    group_by(region) %>%
    mutate(respondents_share = respondents / sum(respondents))
company_sizes <- c("fewer than 10", "10 to 19", "20 to 99", "100 to 499",
                   "500 to 999", "1,000 to 4,999", "5,000 to 9,999",
                   "10,000 or more")

# Plot of company size by region
plot_title <- "Share of Professional Developers by Company Size, 2017"
companysize_regions_2017_prodevs %>%
    mutate(company_size = factor(
        company_size, levels = company_sizes,
        labels = toTitleCase(company_sizes))) %>%
    ggplot(aes(company_size, respondents_share, fill = region)) +
    geom_col(width = plot_bar_width, position = position_dodge()) +
    scale_fill_manual(values = plot_colors_4_emphasis) +
    scale_y_continuous(limits = c(0, 0.26), breaks = seq(0, 0.3, by = 0.05),
                       labels = percent_format(), expand = plot_axis_padding) +
    labs(x = "Number of Employees", y = plot_ylab_respondents_share, fill = "",
         title = plot_title, subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "right")
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Cumulative shares work in companies of difference sizes
companysize_regions_2017_prodevs %>%
    mutate(company_size = factor(company_size, levels = company_sizes)) %>%
    group_by(region) %>%
    arrange(company_size) %>%
    mutate(respondents_share_cumsum = cumsum(respondents_share)) %>%
    select(region, company_size, respondents_share_cumsum) %>%
    spread(region, respondents_share_cumsum)

# Shares of developers working in companies of different sizes
companysize_regions_2017_prodevs %>%
    mutate(company_size = factor(company_size, levels = company_sizes)) %>%
    select(region, company_size, respondents_share) %>%
    spread(region, respondents_share)

#### Industry ####
# Industries by respondent share
industry_regionscarow_2017_prodevs <- respondents %>%
    filter(year == 2017 & !is.na(region_carow_label) &
               !is.na(industry_original) & pro_status == "pro dev") %>%
    group_by(region_carow_label, industry_original) %>%
    summarise(respondents = n(),
              median_salary_cad = median(salary_cad, na.rm = TRUE),
              n_salary_responses = sum(ifelse(!is.na(salary_cad), TRUE, FALSE))) %>%
    group_by(region_carow_label) %>%
    mutate(respondents_share = respondents / sum(respondents),
           industry_original_label = ifelse(
               grepl("advertising", industry_original),
               "Media, Advertising,\nPublishing, or Entertainment",
               ifelse(grepl("finance", industry_original),
                      "Finance, Banking,\nor Insurance",
                      ifelse(grepl("military", industry_original),
                             "Government\n(Including Military)",
                             toTitleCase(industry_original))))) %>%
    ungroup()

# Plot of industry respondent shares
n_top_industries_ca <- 10
top_industry_labels_2017_ca_prodevs <- industry_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada" & !grepl("other", industry_original)) %>%
    top_n(n_top_industries_ca, respondents_share) %>%
    arrange(respondents_share) %>%
    pull(industry_original_label)
plot_title <- paste("Share of Canadian Professional Developers Among Top",
                    n_top_industries_ca, "Industries, 2017")
industry_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada" &
               industry_original_label %in% top_industry_labels_2017_ca_prodevs) %>%
    mutate(industry_original_label = factor(
        industry_original_label, levels = top_industry_labels_2017_ca_prodevs)) %>%
    ggplot(aes(industry_original_label, respondents_share,
               label = percent(respondents_share))) +
    geom_col(fill = plot_color, width = plot_bar_width) +
    geom_text(hjust = 0, nudge_y = 0.002) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 0.3), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "",
         title = plot_title, subtitle = plot_subtitle_respondents_share) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(panel.grid.major.y = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Share working in software or internet/web services
industry_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada" & industry_original %in% c(
        "software", "internet or web services")) %>%
    summarise(sum(respondents_share))

# Shares working in other top industries
industry_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada" & industry_original %in% c(
        "finance, banking, or insurance", "health care services", "consulting",
        "media, advertising, publishing, or entertainment")) %>%
    select(industry_original, respondents_share)

# Industries, 2017 (professional developers)
industry_2017_prodevs <- industry_regionscarow_2017_prodevs %>%
    mutate(region_carow_label = ifelse(region_carow_label == "Canada",
                                       "respondents_share_ca", "respondents_share_row")) %>%
    select(region_carow_label, industry_original_label, respondents_share) %>%
    spread(region_carow_label, respondents_share) %>%
    left_join(industry_regionscarow_2017_prodevs %>%
                  mutate(region_carow_label = ifelse(
                      region_carow_label == "Canada", "respondents_ca",
                      "respondents_row")) %>%
                  select(region_carow_label, industry_original_label, respondents) %>%
                  spread(region_carow_label, respondents),
              by = "industry_original_label") %>%
    left_join(industry_regionscarow_2017_prodevs %>%
                  mutate(region_carow_label = ifelse(
                      region_carow_label == "Canada", "median_salary_cad_ca",
                      "median_salary_cad_row")) %>%
                  select(region_carow_label, industry_original_label, median_salary_cad) %>%
                  spread(region_carow_label, median_salary_cad),
              by = "industry_original_label") %>%
    mutate(respondents_total = respondents_ca + respondents_row,
           pc_diff_carow = respondents_share_ca / respondents_share_row - 1,
           pp_diff_carow = respondents_share_ca - respondents_share_row,
           pc_diff_median_salary_carow = median_salary_cad_ca /
               median_salary_cad_row - 1,
           diff_median_salary_carow = median_salary_cad_ca - median_salary_cad_row) 

# % diff between CA and ROW
min_respondents_industry <- 50
industry_2017_prodevs %>%
    filter(respondents_ca >= min_respondents_industry &
               !grepl("Other", industry_original_label)) %>%
    arrange(pc_diff_carow) %>%
    mutate(industry_original_label = factor(
        industry_original_label, levels = industry_original_label)) %>%
    ggplot(aes(industry_original_label, pc_diff_carow, label = percent(pc_diff_carow))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(hjust = 0, nudge_y = 0.005) +
    coord_flip() +
    scale_y_continuous(limits = c(-0.25, 1.05), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = "% Difference",
         title = paste("Difference in Share of Professional Developers by",
                       "Industry\nBetween Canada and Rest of the World, 2017"),
         subtitle = paste("% Difference Between Canada and Rest of World in",
                          "Share of Survey Respondents")) +
    theme(panel.grid.major.y = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Industry growth over time in Canada
industry_years_20152017_ca <- respondents %>%
    filter(year >= 2015 & country == "canada" & !is.na(industry_consolidated)) %>%
    group_by(year, industry_consolidated) %>%
    summarise(respondents = n()) %>%
    group_by(year) %>%
    mutate(respondents_share = respondents / sum(respondents)) %>%
    group_by(industry_consolidated) %>%
    mutate(respondents_share_ind = respondents_share /
               respondents_share[year == min(year)] * 100)

# Consulting, government, manufacturing industries omitted
# because their related survey options have been less consistent over time
min_respondents_industry_year <- 20
plot_industries <- industry_years_20152017_ca %>%
    select(industry_consolidated, year, respondents) %>%
    spread(year, respondents) %>%
    filter(`2015` >= min_respondents_industry_year &
               `2016` >= min_respondents_industry_year &
               `2017` >= min_respondents_industry_year &
               !(industry_consolidated %in% c(
                   "consulting", "manufacturing", "government", "other"))) %>%
    pull(industry_consolidated)

# Plot of industry growth over time, 2015-2017
plot_title <- "Share of Canadian Developers Among Select Industries Over Time, 2015-2017"
industry_years_20152017_ca %>%
    filter(industry_consolidated %in% plot_industries) %>%
    mutate(industry_consolidated_label = factor(
        industry_consolidated,
        levels = c("software", "internet or web services", "finance",
                   "media and advertising", "health care", "telecommunications"),
        labels = toTitleCase(
            c("software", "internet or web services", "finance",
              "media and advertising", "health care", "telecommunications")))) %>%
    ggplot(aes(year, respondents_share_ind, color = industry_consolidated_label)) +
    geom_line(size = plot_sizes_4_emphasis[1]) +
    scale_color_manual(values = plot_colors_6) +
    scale_x_continuous(breaks = seq(2015, 2017, by = 1)) +
    scale_y_continuous(breaks = seq(40, 160, by = 20), limits = c(40, 160),
                       expand = plot_axis_padding) +
    labs(x = "", y = "Index, 2015 = 100", color = "", title = plot_title,
         subtitle = "Index of % of Survey Responents, 2015 = 100") +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "right")
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Industry growth numbers
industry_years_20152017_ca %>%
    select(year, industry_consolidated, respondents_share) %>%
    spread(year, respondents_share) %>%
    mutate(growth_2017 = `2017` / `2016` - 1,
           growth_20152017 = `2017` / `2015` - 1) %>%
    arrange(desc(growth_2017)) %>%
    select(growth_2017, growth_20152017)

# Growth in health care without combining pharma in 2017
respondents %>%
    filter(year >= 2015 & country == "canada" & !is.na(industry_original)) %>%
    select(-industry_consolidated) %>%
    left_join(industry_consolidations %>%
                  filter(!grepl("pharma", industry_original)),
              by = "industry_original") %>%
    group_by(year, industry_consolidated) %>%
    summarise(respondents = n()) %>%
    group_by(year) %>%
    mutate(respondents_share = respondents / sum(respondents)) %>%
    group_by(industry_consolidated) %>%
    mutate(respondents_share_pc_growth = respondents_share /
               lag(respondents_share) - 1) %>%
    filter(industry_consolidated == "health care" & year == 2017)

#### Employment Status ####
# Employment status data
employmentstatusoriginal_2017_ca_prodevs <- respondents %>%
    filter(year == 2017 & country == "canada" &
               !is.na(employment_status_original) &
               !(employment_status_original %in% c("retired", "i prefer not to say")) &
               pro_status == "pro dev") %>%
    group_by(employment_status_original) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / sum(respondents))
employment_statuses_original_order <- c(
    "employed full-time", "independent contractor, freelancer, or self-employed",
    "employed part-time", "not employed, but looking for work",
    "not employed, and not looking for work")

# Plot of employment statuses among Canadian professional developers in 2017
plot_title <- "Share of Canadian Professional Developers by Employment Status, 2017"
employmentstatusoriginal_2017_ca_prodevs %>%
    mutate(employment_status = factor(
               employment_status_original,
               levels = rev(employment_statuses_original_order),
               labels = rev(toTitleCase(ifelse(
                   grepl("freelancer", employment_statuses_original_order),
                   "independent contractor, freelancer,\nor self-employed",
                   employment_statuses_original_order))))) %>%
    ggplot(aes(employment_status, respondents_share, label = percent(respondents_share))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(hjust = 0, nudge_y = 0.005) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1), labels = percent_format(),
                       expand = plot_axis_padding) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "", title = plot_title,
         subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.y = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

#### Salary ####
min_n_salary_responses_country <- 20
n_top_countries_salary <- 20

# Plot of median salaries by country
top_countries_salary <- country_years_prodevs %>%
    filter(year == 2017) %>%
    arrange(median_salary_cad) %>%
    pull(country)
plot_title <- paste("Average Salary of Professional Developers Among Top",
                    n_top_countries_salary, "Countries, 2017")
country_years_prodevs %>%
    mutate(country = factor(
        country, levels = top_countries_salary,
        labels = toTitleCase(top_countries_salary))) %>%
    filter(year == 2017 &
               n_salary_responses >= min_n_salary_responses_country) %>%
    top_n(n_top_countries_salary, median_salary_cad) %>%
    ggplot(aes(country, median_salary_cad, fill = region_carow_label,
               label = paste0("C$", comma(round(median_salary_cad))))) +
    geom_col(width = plot_bar_width) +
    geom_text(hjust = 0, nudge_y = 200) +
    coord_flip() +
    scale_fill_manual(values = plot_colors_2_emphasis) +
    scale_y_continuous(limits = c(0, 140000), breaks = seq(0, 150000, by = 25000),
                       labels = paste0("C$", comma(seq(0, 150000, by = 25000))),
                       expand = plot_axis_padding) +
    labs(x = "", y = "C$", title = plot_title,
         subtitle = "Median Annual Salary in C$ of Survey Respondents") +
    theme(panel.grid.major.y = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Sample size of median salary plot
country_years_prodevs %>%
    filter(year == 2017 & n_salary_responses >= min_n_salary_responses_country) %>%
    nrow()

# Canada's salary rank
country_years_prodevs %>%
    filter(year == 2017 &
               n_salary_responses >= min_n_salary_responses_country) %>%
    mutate(rank = min_rank(desc(median_salary_cad))) %>%
    filter(country == "canada") %>%
    select(country, rank, median_salary_cad, n_salary_responses)

# US' salary rank
country_years_prodevs %>%
    filter(year == 2017 & country == "united states") %>%
    select(country, mean_salary_cad, median_salary_cad, n_salary_responses)

# Comparing country salary ranks between market and PPP valuations
# Plot of country salary rankings measured in market and PPP rates, 2017
median_salary_cad_ca <- country_years_prodevs %>%
    filter(year == 2017 & country == "canada") %>%
    pull(median_salary_cad)
median_salary_intl_dollar_ca <- country_years_prodevs %>%
    filter(year == 2017 & country == "canada") %>%
    pull(median_salary_intl_dollar)
plot_title <- paste("Average Salary of Professional Developers Among Top",
                    n_top_countries_salary, "Countries, 2017")
country_years_prodevs %>%
    filter(year == 2017 & n_salary_responses >= min_n_salary_responses_country &
               n_salary_intl_dollar_responses >= min_n_salary_responses_country) %>%
    mutate(median_salary_cad_ind = median_salary_cad / median_salary_cad_ca,
           median_salary_intl_dollar_ind = median_salary_intl_dollar /
               median_salary_intl_dollar_ca) %>%
    select(country, region_carow_label, `Market Exchange Rates` = median_salary_cad_ind,
           `PPP Exchange Rates` = median_salary_intl_dollar_ind) %>%
    gather(currency, median_salary_ind, -country, -region_carow_label) %>%
    group_by(currency) %>%
    mutate(median_salary_rank = rank(-median_salary_ind)) %>%
    top_n(n_top_countries_salary, median_salary_ind) %>%
    ungroup() %>%
    arrange(median_salary_ind) %>%
    mutate(country_currency = factor(
        paste(country, currency, sep = "_"),
        levels = paste(country, currency, sep = "_"),
        labels = paste0(median_salary_rank, ". ", toTitleCase(country)))) %>%
    ggplot(aes(country_currency, median_salary_ind, fill = region_carow_label,
               label = paste0(format(round(median_salary_ind, 2), n.small = 2), "x"))) +
    facet_wrap(~ currency, scales = "free_y") +
    geom_col(width = plot_bar_width) +
    geom_text(hjust = 0, nudge_y = 0.005) +
    coord_flip() +
    scale_fill_manual(values = plot_colors_2_emphasis) +
    scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 2.5, by = 0.5),
                       labels = paste0(comma(seq(0, 2.5, by = 0.5)), "x"),
                       expand = plot_axis_padding) +
    labs(x = "", y = "Salary Relative to Canada", title = plot_title,
         subtitle = paste("Median Annual Salary of Survey Respondents by Country",
                          "Relative to Canadian Median Salary\nMeasured at Market",
                          "and Purchasing Power Parity (PPP) Exchange Rates")) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(hjust = 0),
          strip.background = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Sample size of PPP plot
country_years_prodevs %>%
    filter(year == 2017 & n_salary_responses >= min_n_salary_responses_country &
               n_salary_intl_dollar_responses >= min_n_salary_responses_country) %>%
    nrow()

# Canada PPP rank
country_years_prodevs %>%
    filter(year == 2017 & n_salary_responses >= min_n_salary_responses_country &
               n_salary_intl_dollar_responses >= min_n_salary_responses_country) %>%
    mutate(median_salary_intl_dollar_rank = rank(-median_salary_intl_dollar)) %>%
    filter(country == "canada") %>%
    select(median_salary_intl_dollar_rank)

# Proportion of respondents omitted because of home currency not matching salary currency
(respondents %>%
        filter(year == 2017 & !is.na(salary_usd) & !is.na(country) & (
            is.na(country_currency_code) | is.na(local_currency_code) |
                country_currency_code != local_currency_code)) %>%
        nrow()) /
    (respondents %>%
         filter(year == 2017 & !is.na(salary_usd) & !is.na(country)) %>%
         nrow())

# Plot of salaries by company type in Canada, 2017
min_n_salaries_company_type <- 20
plot_title <- "Average Salary of Canadian Professional Developers by Company Type, 2017"
companytype_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada" &
               n_salary_responses >= min_n_salaries_company_type) %>%
    arrange(median_salary_cad) %>%
    mutate(company_type_label = factor(
        company_type_label, levels = company_type_label)) %>%
    ggplot(aes(company_type_label, median_salary_cad,
               label = paste0("C$", comma(median_salary_cad)))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(hjust = 0, nudge_y = 200) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 105000), breaks = seq(0, 120000, by = 20000),
                       labels = paste0("C$", comma(seq(0, 120000, by = 20000))),
                       expand = plot_axis_padding) +
    labs(x = "", y = "C$", title = plot_title,
         subtitle = "Median Annual Salary in C$ of Survey Respondents") +
    theme(panel.grid.major.y = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Median salary for pro devs in industries in Canada, 2017
min_n_salaries_industry <- 20
omit_industries <- c("other (please specify)")
plot_title <- "Average Salary of Canadian Professional Developers by Industry, 2017"
industry_regionscarow_2017_prodevs %>%
    filter(region_carow_label == "Canada" &
               n_salary_responses >= min_n_salaries_industry &
               !(industry_original %in% omit_industries)) %>%
    arrange(median_salary_cad) %>%
    mutate(industry_original_label = factor(
        industry_original_label, levels = industry_original_label)) %>%
    ggplot(aes(industry_original_label, median_salary_cad,
               label = paste0("C$", comma(round(median_salary_cad))))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(hjust = 0, nudge_y = 200) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 105000), breaks = seq(0, 120000, by = 20000),
                       labels = paste0("C$", comma(seq(0, 120000, by = 20000))),
                       expand = plot_axis_padding) +
    labs(x = "", y = "C$", fill = "", title = plot_title,
         subtitle = "Median Annual Salary in C$ of Survey Respondents") +
    theme(panel.grid.major.y = element_blank())
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

#### Job Seeking and Discovery ####
# Plot of employment and job-seeking status among Canadian professional developers, 2017
job_seeking_status_order <- data_frame(
    status = c("i am actively looking for a job",
               "i'm not actively looking, but i am open to new opportunities",
               "i am not interested in new job opportunities"),
    label = c(
        "Actively looking for a job",
        "Not actively looking for a job,\nbut open to new opportunities",
        "Not interested in new job opportunities"))
employment_status_original_order <- data_frame(
    status = c("employed full-time",
               "independent contractor, freelancer, or self-employed",
               "employed part-time", "unemployed"),
    label = c("Employed full-time",
              "Independent contractor, freelancer,\nor self-employed",
              "Employed part-time", "Unemployed"))
plot_title <- paste("Share of Canadian Professional Developers by",
                    "Employment and Job-Seeking Status, 2017")
respondents %>%
    mutate(employment_status_original = ifelse(
        grepl("not employed", employment_status_original), "unemployed",
        employment_status_original)) %>%
    filter(year == 2017 & country == "canada" &
               !is.na(job_seeking_status) &
               employment_status_original %in% c(
                   "employed full-time", "employed part-time", "unemployed",
                   "independent contractor, freelancer, or self-employed") &
               pro_status == "pro dev") %>%
    group_by(employment_status_original, job_seeking_status) %>%
    summarise(respondents = n()) %>%
    group_by(employment_status_original) %>%
    mutate(respondents_share = respondents / sum(respondents),
           job_seeking_status_label = factor(
               job_seeking_status,
               levels = job_seeking_status_order$status,
               labels = job_seeking_status_order$label),
           employment_status_original_label = factor(
               employment_status_original,
               levels = employment_status_original_order$status,
               labels = employment_status_original_order$label)) %>%
    ggplot(aes(employment_status_original_label, respondents_share,
               fill = job_seeking_status_label, label = percent(respondents_share))) +
    geom_col(width = plot_bar_width) +
    geom_text(vjust = "inward", position = position_fill()) +
    scale_fill_manual(values = c("red1", "red3", "red4")) +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "Employment Status", y = plot_ylab_respondents_share, fill = "",
         title = plot_title, subtitle = "% of Survey Respondents per Employment Status") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Sample size per employment status
respondents %>%
    mutate(employment_status_original = ifelse(
        grepl("not employed", employment_status_original), "unemployed",
        employment_status_original)) %>%
    filter(year == 2017 & country == "canada" &
               !is.na(job_seeking_status) &
               employment_status_original %in% c(
                   "employed full-time", "employed part-time", "unemployed",
                   "independent contractor, freelancer, or self-employed") &
               pro_status == "pro dev") %>%
    group_by(employment_status_original) %>%
    summarise(respondents = n())

# Job discovery channels
jobdiscoverychannels_2017_ca <- respondents %>%
    filter(year == 2017 & country == "canada" & !is.na(job_discovery_channel) &
               pro_status == "pro dev") %>%
    group_by(job_discovery_channel) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / sum(respondents))

# Plot of job discovery channels among Canadian professional developers, 2017
plot_title <- "Share of Canadian Professional Developers by Job Discovery Channel, 2017"
jobdiscoverychannels_2017_ca %>%
    mutate(job_discovery_channel_consolidated = ifelse(
        grepl("job board", job_discovery_channel), "job board", job_discovery_channel)) %>%
    group_by(job_discovery_channel_consolidated) %>%
    summarise(respondents_share = sum(respondents_share)) %>%
    arrange(respondents_share) %>%
    mutate(job_discovery_channel_consolidated_label = factor(
        job_discovery_channel_consolidated,
        levels = job_discovery_channel_consolidated,
        labels = toTitleCase(job_discovery_channel_consolidated))) %>%
    ggplot(aes(job_discovery_channel_consolidated_label, respondents_share,
               label = percent(respondents_share))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(hjust = "inward") +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "", title = plot_title,
         subtitle = "% of Survey Respondents") +
    theme(panel.grid.major.x = element_line(plot_grid_color),
          panel.grid.major.y = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Channels included under "job board"
jobdiscoverychannels_2017_ca %>%
    filter(grepl("job board", job_discovery_channel)) %>%
    select(job_discovery_channel)

# Contacted by someone from company or external recruiter
jobdiscoverychannels_2017_ca %>%
    filter(grepl("recruiter|contacted directly", job_discovery_channel)) %>%
    summarise(sum(respondents_share))

#### Experience ####
# Years of experience by region
proexperiencebin_regions_2017_prodevs <- respondents %>%
    filter(year == 2017 & !is.na(region) & !is.na(pro_experience) &
               pro_status == "pro dev") %>%
    group_by(pro_experience_bin, pro_experience_bin_above_15, region) %>%
    summarise(respondents = n()) %>%
    group_by(region) %>%
    mutate(respondents_share = respondents / sum(respondents))

# Plot of years experience by region, 2017
plot_title <- "Share of Professional Developers by Years of Experience, 2017"
proexperiencebin_regions_2017_prodevs %>%
    ggplot(aes(pro_experience_bin, respondents_share, fill = region)) +
    geom_col(position = position_dodge(), width = plot_bar_width) +
    scale_fill_manual(values = plot_colors_4_emphasis) +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "Years of Experience", y = plot_ylab_respondents_share, fill = "",
         title = plot_title, subtitle = plot_subtitle_respondents_share)
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Share of developers with over 15 years experience by region, 2017
proexperiencebin_regions_2017_prodevs %>%
    group_by(region, pro_experience_bin_above_15) %>%
    summarise(respondents_share = sum(respondents_share))

# Plot of top countries with share of devs over 15 years of experience, 2017
min_respondents_country <- 50
n_top_countries_proexperience <- 20
plot_title <- paste("Share of Professional Developers With Over 15 Years",
                    "of Programming Experience\nAmong Top",
                    n_top_countries_proexperience, "Countries, 2017")
respondents %>%
    filter(year == 2017 & !is.na(country) & !is.na(pro_experience_bin) &
               pro_status == "pro dev") %>%
    group_by(country) %>%
    summarise(pro_experience_bin_above_15_share = sum(pro_experience_bin_above_15) / n(),
              respondents = n()) %>%
    filter(respondents >= min_respondents_country) %>%
    top_n(n_top_countries_proexperience, pro_experience_bin_above_15_share) %>%
    arrange(pro_experience_bin_above_15_share) %>%
    mutate(country_label = factor(country, levels = country,
                                  labels = toTitleCase(country))) %>%
    ggplot(aes(country_label, pro_experience_bin_above_15_share,
               fill = country == "canada")) +
    geom_col(width = plot_bar_width) +
    coord_flip() +
    scale_fill_manual(values = plot_colors_2_emphasis) +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "", title = plot_title,
         subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.x = element_line(plot_grid_color),
          panel.grid.major.y = element_line(NA)) +
    guides(fill = "none")
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Canada's rank
respondents %>%
    filter(year == 2017 & !is.na(country) & !is.na(pro_experience_bin) &
               pro_status == "pro dev") %>%
    group_by(country) %>%
    summarise(pro_experience_bin_above_15_share = sum(pro_experience_bin_above_15) / n(),
              respondents = n()) %>%
    filter(respondents >= min_respondents_country) %>%
    mutate(rank = rank(-pro_experience_bin_above_15_share)) %>%
    filter(country == "canada")

#### Formal Education ####
# Education level by region, 2017
educ_level_groups <- c("hs or lower", "some college", "bachelor's", "grad degree")
educlevel_regions_2017_prodevs <- respondents %>%
    filter(year == 2017 & !is.na(region) & !is.na(educ_level_group) &
               educ_level_group != "prefer not say" & pro_status == "pro dev") %>%
    group_by(educ_level_group, region) %>%
    summarise(respondents = n(),
              median_salary_cad = median(salary_cad, na.rm = TRUE),
              mean_salary_cad = mean(salary_cad, na.rm = TRUE),
              n_salary_responses = sum(ifelse(!is.na(salary_cad), TRUE, FALSE))) %>%
    group_by(region) %>%
    mutate(respondents_share = respondents / sum(respondents),
           educ_level_group = factor(
               educ_level_group, levels = educ_level_groups,
               labels = ifelse(educ_level_groups == "hs or lower",
                               "High School or Lower",
                               ifelse(educ_level_groups == "some college",
                                      "Some College", toTitleCase(educ_level_groups)))))

# Plot of educational attainment by region, 2017
educlevel_regions_2017_prodevs %>%
    ggplot(aes(educ_level_group, respondents_share, fill = region)) +
    geom_col(position = position_dodge(), width = plot_bar_width) +
    scale_fill_manual(values = plot_colors_4_emphasis) +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "",
         title = paste0("Share of Professional Developers by Highest Level of ",
                        "Formal Education and Country/Region, 2017"),
         subtitle = plot_subtitle_respondents_share)
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Regional shares attaining bachelor's degree or above
educlevel_regions_2017_prodevs %>%
    filter(educ_level_group %in% c("Bachelor's", "Grad Degree")) %>%
    summarise(respondents_share = sum(respondents_share))

# Plot of salary by educational attainment in Canada, 2017
plot_title <- paste("Average Salary of Canadian Professional Developers by",
                    "Formal Education Level, 2017")
educlevel_regions_2017_prodevs %>%
    filter(region == "Canada") %>%
    ggplot(aes(educ_level_group, median_salary_cad,
               label = paste0("C$", comma(median_salary_cad)))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(nudge_y = 3000) +
    scale_y_continuous(labels = comma_format()) +
    labs(x = "", y = "C$", fill = "", title = plot_title,
         subtitle = "Median Annual Salary in C$ of Survey Respondents")
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

#### Informal Education ####
n_pro_dev_respondents_2017_ca <- respondents %>%
    filter(year == 2017 & country == "canada" & pro_status == "pro dev") %>%
    nrow()
informaleduc_2017_ca_prodevs <- respondents %>%
    filter(year == 2017 & country == "canada" & pro_status == "pro dev") %>%
    separate_rows(informal_educ, sep = ";") %>%
    group_by(informal_educ) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / n_pro_dev_respondents_2017_ca)

# Plot of share of developers taking informal education in Canada in 2017
plot_title <- "Share of Canadian Professional Developers by Informal Education, 2017"
informaleduc_2017_ca_prodevs %>%
    filter(!is.na(informal_educ)) %>%
    arrange(desc(respondents_share)) %>%
    mutate(informal_educ_label = factor(
        informal_educ,
        levels = informal_educ,
        labels = ifelse(informal_educ == "Part-time/evening course", "Part-time/\nevening course",
                        ifelse(informal_educ == "Open source contrib", "Open source\ncontribution",
                               ifelse(informal_educ == "Industry certification", "Industry\ncertification",
                               informal_educ))))) %>%
    ggplot(aes(informal_educ_label, respondents_share, label = percent(respondents_share))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(vjust = "inward") +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "", title = plot_title,
         subtitle = plot_subtitle_respondents_share)
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Formal education and self-taught
plot_title <- paste("Share of Canadian Professional Developers Partially",
                    "Self-Taught\nAt Each Level of Formal Education, 2017")
respondents %>%
    filter(year == 2017 & country == "canada" & !is.na(educ_level_group) &
               !grepl("prefer not", educ_level_group) & pro_status == "pro dev") %>%
    mutate(is_self_taught = ifelse(
        grepl("self-taught", informal_educ, ignore.case = TRUE), "Partially Self-Taught",
        "Not Partially Self-Taught"),
        educ_level_group = factor(educ_level_group, levels = educ_level_groups,
                                  labels = toTitleCase(educ_level_groups))) %>%
    group_by(educ_level_group, is_self_taught) %>%
    summarise(respondents = n()) %>%
    group_by(educ_level_group) %>%
    mutate(respondents_share = respondents / sum(respondents)) %>%
    ggplot(aes(educ_level_group, respondents_share, fill = is_self_taught,
               label = percent(respondents_share))) +
    geom_col(width = plot_bar_width) +
    geom_text(vjust = "inward", position = position_fill()) +
    scale_fill_manual(values = c("red3", "red4")) +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "Highest Level of Education", y = plot_ylab_respondents_share,
         fill = "", title = plot_title,
         subtitle = "% of Survey Respondents per Formal Education Level")
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Unemployed time since bootcamp
plot_title <- paste("Share of Canadian Developers by Employment Status",
                    "Relative to Taking Bootcamp, 2017")
respondents %>%
    filter(year == 2017 & country == "canada" &
               !is.na(unemployed_time_post_bootcamp)) %>%
    mutate(unemployed_time_post_bootcamp_group = factor(
        ifelse(grepl("already had a job", unemployed_time_post_bootcamp),
               "Had a job prior to bootcamp",
               ifelse(grepl("haven't gotten a job", unemployed_time_post_bootcamp),
                      "Haven't gotten a job since bootcamp",
                      "Got a job since starting bootcamp")),
        levels = c("Had a job prior to bootcamp",
                   "Got a job since starting bootcamp",
                   "Haven't gotten a job since bootcamp"))) %>%
    group_by(unemployed_time_post_bootcamp_group) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / sum(respondents)) %>%
    ggplot(aes(unemployed_time_post_bootcamp_group, respondents_share,
               label = percent(respondents_share))) +
    geom_col(width = plot_bar_width, fill = plot_color) +
    geom_text(vjust = "inward") +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "",
         title = plot_title, subtitle = plot_subtitle_respondents_share)
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

#### Gender ####
gender_country_years <- respondents %>%
    filter(!is.na(country) & !is.na(gender) & gender != "prefer not to disclose") %>%
    group_by(year, country, region_carow_label, gender) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / sum(respondents)) %>%
    ungroup()

# Genders breakdown of Canadians in 2017
gender_country_years %>%
    filter(country == "canada" & year == 2017) %>%
    select(gender, respondents, respondents_share)

# Plot of most female-represented countries, 2017
min_respondents_country_gender <- 50
n_top_female_countries <- 20
top_female_countries <- gender_country_years %>%
    inner_join(gender_country_years %>%
                   filter(year == 2017) %>%
                   group_by(country) %>%
                   summarise(respondents = sum(respondents)) %>%
                   filter(respondents >= min_respondents_country_gender) %>%
                   select(country),
               by = "country") %>%
    filter(year == 2017 & gender == "female") %>%
    top_n(n_top_female_countries, respondents_share) %>%
    arrange(respondents_share) %>%
    pull(country)
plot_title <- paste("Share of Developers by Gender Among Top",
                    n_top_female_countries, "Most Female-Represented Countries, 2017")
gender_country_years %>%
    filter(year == 2017 & country %in% top_female_countries) %>%
    mutate(gender = factor(
        gender,
        levels = c("male", "transgendered or non-conforming", "female"),
        labels = c("Male", "Transgendered\nor non-conforming", "Female")),
           country = factor(country, levels = top_female_countries,
                            labels = toTitleCase(top_female_countries))) %>%
    ggplot(aes(country, respondents_share, fill = gender)) +
    geom_col(width = plot_bar_width) +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = c("darkseagreen", "dodgerblue4", "red4")) +
    labs(x = "", y = plot_ylab_respondents_share, fill = "",
         title = plot_title, subtitle = plot_subtitle_respondents_share) +
    guides(fill = guide_legend(reverse = TRUE))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Canada's rank in female representation
gender_country_years %>%
    inner_join(gender_country_years %>%
                   filter(year == 2017) %>%
                   group_by(year, country) %>%
                   summarise(respondents = sum(respondents)) %>%
                   filter(respondents >= min_respondents_country_gender) %>%
                   select(year, country),
               by = c("year", "country")) %>%
    filter(gender == "female") %>%
    mutate(rank = rank(-respondents_share)) %>%
    filter(country == "canada")

# Plot of male:female ratio over time in Canada and ROW, 2014-2017
plot_title <- "Ratio of Male to Female Canadian Developers Over Time, 2014-2017"
gender_country_years %>%
    filter(gender %in% c("male", "female")) %>%
    group_by(year, region_carow_label, gender) %>%
    summarise(respondents = sum(respondents)) %>%
    spread(gender, respondents) %>%
    mutate(male_female_ratio = male / female) %>%
    ggplot(aes(year, male_female_ratio)) +
    geom_line(aes(color = region_carow_label, size = region_carow_label)) +
    geom_text(aes(label = paste0(round(male_female_ratio), ":1")), nudge_y = 1) +
    scale_color_manual(values = plot_colors_2_emphasis) +
    scale_size_manual(values = c(1, 3)) +
    lims(y = c(5, 25)) +
    labs(x = "", y = "Males:Females", color = "", size = "", title = plot_title,
         subtitle = "Number of Male Survey Respondents to Female Respondents") +
    guides(color = guide_legend(reverse = TRUE), size = guide_legend(reverse = TRUE))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Plot of gender pay gap among Canada and other large countries, 2017
min_n_salary_responses_gender_country <- 20
plot_title <- paste("Salary Gap Between Male and Female Professional Developers",
                    "Among Most-Represented Countries, 2017")
country_years_prodevs %>%
    filter(year == 2017 &
               n_salary_responses_males >= min_n_salary_responses_gender_country &
               n_salary_responses_females >= min_n_salary_responses_gender_country) %>%
    arrange(desc(salary_pc_diff_females_males)) %>%
    mutate(country_label = factor(country, levels = country, labels = toTitleCase(country))) %>%
    ggplot(aes(country_label, salary_pc_diff_females_males,
               fill = country_label == "Canada",
               label = percent(round(salary_pc_diff_females_males, 3)))) +
    geom_col(width = plot_bar_width) +
    geom_text(nudge_y = -0.01) +
    scale_fill_manual(values = plot_colors_2_emphasis) +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "", y = "% Difference", title = plot_title,
         subtitle = "% Difference Between Median Female and Male Respondents' Salary") +
    guides(fill = "none")
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Salary comparison in Canada, 2017
country_years_prodevs %>%
    filter(year == 2017 & country == "canada") %>%
    select(year, country, median_salary_cad_males, median_salary_cad_females)

# Plot of gender and experience in Canada, 2017
gender_proexperience_2017_ca_prodevs <- respondents %>%
    filter(year == 2017 & country == "canada" & !is.na(gender) &
               gender != "prefer not to disclose" & !is.na(pro_experience_bin) &
               pro_status == "pro dev") %>%
    group_by(gender, pro_experience_bin, pro_experience_bin_above_15) %>%
    summarise(respondents = n()) %>%
    group_by(gender) %>%
    mutate(respondents_share = respondents / sum(respondents))
plot_title <- paste("Share of Canadian Professional Developers by Years of",
                    "Experience and Gender, 2017")
gender_proexperience_2017_ca_prodevs %>%
    filter(gender %in% c("male", "female")) %>%
    mutate(gender_label = factor(gender, levels = c("male", "female"),
                                 labels = c("Male", "Female"))) %>%
    ggplot(aes(pro_experience_bin, respondents_share, fill = gender_label)) +
    geom_col(position = position_dodge(), width = plot_bar_width) +
    scale_fill_manual(values = c("red3", "red4")) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 0.6)) +
    labs(x = "Years of Experience", y = plot_ylab_respondents_share, fill = "",
         title = plot_title, subtitle = plot_subtitle_respondents_share)
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Gender breakdown under different experience bins
gender_proexperience_2017_ca_prodevs %>% filter(pro_experience_bin == "Under 5")
gender_proexperience_2017_ca_prodevs %>%
    filter(pro_experience_bin_above_15) %>%
    group_by(gender) %>%
    summarise(respondents_share = sum(respondents_share))

#### Ethnicity ####
n_ethnicity_respondents_2017_ca <- respondents %>%
    mutate(ethnicities = gsub(
        ";+", ";",
        gsub("^;|;$", "",
             gsub("(I don.t know|I prefer not to say)", "", ethnicities)))) %>%
    filter(year == 2017 & country == "canada" &
               !is.na(ethnicities) & grepl("[[:alpha:]]", ethnicities)) %>%
    nrow()
ethnicities_2017_ca <- respondents %>%
    mutate(ethnicities = gsub(
        ";+", ";",
        gsub("^;|;$", "",
             gsub("(I don.t know|I prefer not to say)", "", ethnicities)))) %>%
    filter(year == 2017 & country == "canada" &
               !is.na(ethnicities) & grepl("[[:alpha:]]", ethnicities)) %>%
    separate_rows(ethnicities, sep = ";") %>%
    group_by(ethnicity = ethnicities) %>%
    summarise(respondents = n()) %>%
    mutate(respondents_share = respondents / n_ethnicity_respondents_2017_ca)

# Plot of ethnicity shares in Canada, 2017
plot_title <- "Share of Canadian Developers by Ethnicity, 2017"
ethnicities_2017_ca %>%
    filter(ethnicity != "Other") %>%
    arrange(respondents_share) %>%
    mutate(ethnicity_label = factor(
        ethnicity,
        levels = ethnicity,
        labels = ifelse(grepl("Native", ethnicity), "Native American, Pacific Islander\nor Indigenous Australian", ethnicity))) %>%
    ggplot(aes(ethnicity_label, respondents_share, label = percent(respondents_share))) +
    geom_col(fill = plot_color, width = plot_bar_width) +
    geom_text(hjust = "inward") +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "", y = plot_ylab_respondents_share, title = plot_title,
         subtitle = plot_subtitle_respondents_share) +
    theme(panel.grid.major.x = element_line(plot_grid_color),
          panel.grid.major.y = element_line(NA))
save_plot(file_name = gsub("[[:punct:]]", " ", plot_title))

# Share of East and South Asians
ethnicities_2017_ca %>%
    filter(grepl("asian", ethnicity, ignore.case = TRUE)) %>%
    summarise(sum(respondents_share))

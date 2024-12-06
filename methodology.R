
# Table S1. Country selection ------------------------------------------------------------------------------

map_info <- import(here("data", "S1 Table.xlsx")) %>% 
  clean_names() %>% 
  
  # Create columns: LIC, MIC, HIC
  mutate(
    country = countrycode::countrycode(
      iso3, "iso3c", "country.name")) %>% 
  # select only columns need for the analysis
  # rename abbreviation income group
  mutate(income_group = case_when(
    income == "LMIC" ~ "Low- and Middle-income Country",
    income == "HIC" ~ "High Income Country"))



# Table S2. Ecology variables ------------------------------------------------------------------------------
driver_indicators <- data.frame(
  Tier1 = rep("Drivers (Total)", 19),
  Tier2 = c("Sanitation", "Sanitation", "Sanitation", "Sanitation", "Infection", "Infection", "Infection", "Infection", "Vaccination", "Vaccination", "Vaccination", "Vaccination", "Vaccination", "Vaccination", "Vaccination", "Workforce", "Workforce", "Workforce", "Workforce"),
  Description = c("Hygiene and Sanitation Standards (+)", "Hygiene and Sanitation Standards (+)", "Hygiene and Sanitation Standards (+)", "Hygiene and Sanitation Standards (+)", "Infection Prevalence (-)", "Infection Prevalence (-)", "Infection Prevalence (-)", "Infection Prevalence (-)", "Vaccination Coverage (+)", "Vaccination Coverage (+)", "Vaccination Coverage (+)", "Vaccination Coverage (+)", "Vaccination Coverage (+)", "Vaccination Coverage (+)", "Vaccination Coverage (+)", "Health Care Workforce (+)", "Health Care Workforce (+)", "Health Care Workforce (+)", "Health Care Workforce (+)"),
  Tier3 = c("Drinking Water Source", "Drinking Water Source", "Overall Sanitation", "Water Source Access", "HIV Prevalence", "HIV Prevalence", "Tuberculosis Prevalence (Tb)", "Tuberculosis Prevalence (Tb)", "DTP3", "HepB3", "Hib3", "PCV3", "Pol3", "Measles", "RCV1", "Physicians per capita", "Physicians per capita", "Nursing and midwifery", "Nursing and midwifery"),
  DescriptionTier3 = c("Proportion of population using improved drinking water sources", "Population using improved drinking-water sources (%)", "Population using improved sanitation facilities (%)", "Improved water source (% of population with access)", "UN Prevalence of HIV Total (% of population ages 15-49)", "WB Prevalence of HIV Total (% of population ages 15-49)", "UN Prevalence of tuberculosis (per 100 000 population)", "WB Incidence of tuberculosis (per 100 000 population)", "Diphtheria, Tetanus, and Pertussis immunization coverage among 1-year-olds (%)", "Hepatitis B immunization coverage among 1-year-olds (%)", "Haemophilus influenzae type b immunization coverage among 1-year-olds (%)", "Pneumococcal conjugate vaccines immunization coverage among 1-year-olds (%)", "Polio immunization coverage among 1-year-olds (%)", "Immunization, measles (% of children ages 12-23 months)", "Rubella vaccination coverage (%)", "Physicians density (per 1000 population)", "Physicians density (per 1000 population)", "Nursing and midwifery personnel density per 1000 population", "Nursing and midwifery personnel density per 1000 population"),
  `Data Source` = c("UN", "UN", "UN", "UN", "UN", "WB", "UN", "WB", "WHO", "WHO", "WHO", "WHO", "WHO", "UN and WB", "WHO", "UN", "WHO", "UN", "WHO"),
  Unit = c("pct (%)", "pct (%)", "pct (%)", "pct (%)", "pct (%)", "pct (%)", "Per 100 000", "Per 100 000", "pct (%)", "pct (%)", "pct (%)", "pct (%)", "pct (%)", "pct (%)", "pct (%)", "per 1000", "per 1000", "per 1000", "per 1000")
)




use_indicators <- data.frame(
  Tier1 = c("Use (Total)", "Use (Total)", "Use (Total)", "Use (Total)", "Use (Total)"),
  Tier2 = c("BroadPerTotalABXUse", "BroadPerTotalABXUse", "BroadPerTotalABXUse", "NewABXUse", "TotalDDDPer1000Persons"),
  IndicatorComponent = c("Fluoroquinolones, Macrolides",
                         "third-generation cephalosporins",
                         "co-amoxiclav, clindamycin, oral vancomycin",
                         "dalfopristin/quinupristin (2005), gatifloxacin (1999), moxifloxacin (1999), linezolid (2000), telithromycin (2001), balofloxacin (2006), Biapenem (2002), ertapenem (2002), pazufloxacin (2002), prulifloxacin (2002), daptomycin (2003), gemifloxacin (2003), doripenem (2005), tigecycline (2005), garenoxacin (2007), ceftobiprole (2008), sitafloxacin (2008), tebipenem (2009), telavancin (2009), antofloxacin (2010), ceftaroline (2011), ceftolozane/tazobactam (2014), dalbavancin (2014), oritavancin (2014), tedizolid (2014)",
                         "-"),
  Source = c("IQVIA", "IQVIA", "IQVIA", "IQVIA", "IQVIA"),
  UNIT = c("Defined Daily Doses (DDD)", "DDD", "DDD", "DDD", "DDD per 1000")
)


resistance_indicators <- data.frame(
    Tier1 = c("Resistance (Total)", "Resistance (Total)", "Resistance (Total)"),
    Tier2 = c("MRSA: Methicillin-resistant Staphylococcus aureus", "CR: Carbapenem resistance (average)", "STR: Streptococcal resistance (average)"),
    IndicatorComponent = c("-",
                         "Enterobacteriaceae and other bacteria",
                         "Macrolides and penicillin"),
    Source = c("ResistanceMap", "ResistanceMap", "ResistanceMap"),
    UNIT = c("pct (%)", "pct (%)", "pct (%)")
  )

dri_indicators <- data.frame(
  PATHOGEN = c("Enterococcus faecalis/faecium", 
               "Escherichia coli", 
               "Klebsiella pneumonia", 
               "Pseudomonas aeruginosa", 
               "Staphylococcus aureus"),
  ANTIBIOTICS = c("Aminopenicillins", 
                  "Aminoglycosides, Aminopenicillins, Carbapenems, Cephalosporins (3rd gen), Fluoroquinolones", 
                  "Aminoglycosides, Carbapenems, Cephalosporins (3rd gen), Fluoroquinolones", 
                  "Aminoglycosides, Carbapenems, Cephalosporins (3rd gen), Fluoroquinolones, Piperacillin-tazobactam", 
                  "Oxacillin/Cefoxitin (MRSA)"),
  SOURCE = c("IQVIA, ResistanceMap", "IQVIA, ResistanceMap", "IQVIA, ResistanceMap", "IQVIA, ResistanceMap", "IQVIA, ResistanceMap"),
  UNIT = rep("Unit Free", 5)
)

ecology_indicators <- data.frame(
  CONTEXT = c("Economy", "Economy", "Climate", "Livestock", "Population"),
  COVARIATES = c("GDP", "Gini", "Mean Temperature", "Animal Production", "Population Density"),
  DESCRIPTION = c("GDP (log transformed)", 
                  "Gini index: Inequality –proportion of the lowest 20 % of the national income distribution, derived from country gini index for the years between 2004-2012", 
                  "Mean temperature weighted by population density of 1x1 degree grid cells (weighted)", 
                  "Animal production in mass per country area", 
                  "Human population density (log transformed)"),
  SOURCE = c("The Eora Global Supply Chain Database", 
             "World Income Inequality Database - WIID", 
             "NASA's Socioeconomic Data and Applications Center (SEDAC), NASA Center for Climate Simulation (BioClim)", 
             "Gridded Livestock of the World (GLW)", 
             "NASA's Socioeconomic Data and Applications Center (SEDAC)"),
  UNIT = c("per capita", "pct (%)", "degree C", "animal volume (tonnes) per km2", "1/km2")
)


# Table S3. Action categories and subcategories --------------------------------------------------------------
# Data for the table
action_index_table <- data.frame(
  Domain = c(
    rep("Awareness and Education", 4),
    rep("Monitoring and Surveillance", 4),
    rep("General", 3),
    rep("Regulation", 3),
    rep("Prevention", 2)
  ),
  Topic = c(
    "AMR Training (Human Health) *", "AMR Training (Animal Health and Food Production) *",
    "AMR Awareness (Human Health) *", "AMR Awareness (Animal Health and Food Production) *",
    "Monitoring System for AMU (Animals and Crop) *", "AMR Surveillance System (Humans)*",
    "Monitoring System for AMU (Human Health) *", "AMR Surveillance System (Animals and Foods) *",
    "Veterinary Services", "One Health Arrangements", "NAP progress",
    "AMS and Regulation (Human)*", "AMS and Regulation (Animal & Crop) *", "Contamination Prevention",
    "IPC", "AMU Prevention"
  ),
  `Survey Questions` = c(
    "6.3 Training and professional education on AMR in the human health sector",
    "6.4 Training and professional education on AMR in the veterinary sector",
    "6.1 Raising awareness and understanding of AMR risks and response in human health",
    "6.2 Raising awareness and understanding of AMR risks and response in animal health and food production.",
    "7.2 National monitoring system for antimicrobial use in animals and crop production",
    "7.3 National surveillance system for antimicrobial resistance (AMR) in humans",
    "7.1 National monitoring system for consumption and rational use of antimicrobials in human health",
    "7.4 National surveillance system for antimicrobial resistance (AMR) in animals and foods",
    "6.5 Progress with strengthening veterinary services",
    "4.1 Multi-sector and One Health working arrangement",
    "5.1 Country progress with development of a national action plan on AMR",
    "9.1 Antimicrobial Stewardship & regulation in human health",
    "9.2 Antimicrobial stewardship & regulation in animal and crop production",
    "9.3 Legislation and/or regulations to prevent contamination of the environment with antimicrobials",
    "8.1 Infection Prevention and Control (IPC) in human health care",
    "8.2 Good animal health and management practices and good hygiene to prevent infections in order to reduce the use of antimicrobials in animals and AMR transmission in food production"
  )
)


# S4 Table -------------------------------------------------------------------------------------------------
governance_syndrome <- import(here("data", "S4 Table.xlsx"), colClasses = "character") %>% 
  # fill data of column category based on the above cell
  tidyr::fill(Category,.direction = "downup") %>% 
  # Make the number to be displayed as string
  mutate(across(.col = c(`2016–2017`, `2017–2018`, `2021–2022`, `2022–2023`),
                .fns = as.numeric))


governance_syndrome[3, 3] <- "6.1, 6.2"
governance_syndrome[3, 4] <- "6.1, 6.2"

governance_syndrome[10, 8] <- "4.9, 4.10"
governance_syndrome[10, 9] <- "4.9, 4.10"
  


# S5 Table -------------------------------------------------------------------------------------------------
model_formulas <- import(here("data", "S5 Table.xlsx"))

# S6 Table -------------------------------------------------------------------------------------------------
de_escalation_plots <- import(here("data", "S6 Table.xlsx"))


# S7 Table -------------------------------------------------------------------------------------------------
global_models_data <- import(here("data", "S7 Table.xlsx"))

# S8 Table -------------------------------------------------------------------------------------------------
global_model_formulas_general <- import(here("data", "S8 Table.xlsx"), sheet = "General") %>% tidyr::fill("Global Models with DPSE interaction",.direction = "downup") %>% tidyr::fill("Global Models with income interaction",.direction = "downup")

global_model_formulas_hic <- import(here("data", "S8 Table.xlsx"), sheet = "HIC") %>% tidyr::fill("Global Models with DPSE interaction",.direction = "downup") %>% tidyr::fill("Global Models with income interaction",.direction = "downup")

global_model_formulas_lmic <- import(here("data", "S8 Table.xlsx"), sheet = "LMIC") %>% tidyr::fill("Global Models with DPSE interaction",.direction = "downup") %>% tidyr::fill("Global Models with income interaction",.direction = "downup")

global_model_formulas_binomial <- import(here("data", "S8 Table.xlsx"), sheet = "Binomial") %>% tidyr::fill("Global Models with DPSE interaction",.direction = "downup") %>% tidyr::fill("Global Models with income interaction",.direction = "downup")


# Figure S1 and S5 -----------------------------------------------------------------------------------------
importance_df <- import(here("data", "Fig S1 and S5.csv"))


# Figure S3 ------------------------------------------------------------------------------------------------
best_model_coeffs <- import(here("data", "Fig S3-change-coeffs.rds"), trust = TRUE)
best_model_coeffs_point <- import(here("data", "Fig S3-change-coeffs-point.rds"), trust = TRUE)
best_model_coeffs_tile <- import(here("data", "Fig S3-change-coeffs-tile.rds"), trust = TRUE)


best_model_coeffs_point_binomial <- import(here("data", "Fig S3-bin-coeffs-point.rds"), trust = TRUE)
best_model_coeffs_tile_binomial <- import(here("data", "Fig S3-bin-coeffs-tile.rds"), trust = TRUE)


# Figure S4 ------------------------------------------------------------------------------------------------

rank_df_top5_supp <- import(here("data", "Fig S4.rds"), trust = TRUE)

# Figure S6 ------------------------------------------------------------------------------------------------
# heatmap_df <- import(here("data", "Fig S6.csv"))

heatmap_df_supp_s6 <- import(here("data", "Fig S6_supp.rds"), trust = TRUE)
heatmap_df_tile_s6 <- import(here("data", "Fig S6_tile_supp.rds"), trust = TRUE)
heatmap_df_point_s6 <- import(here("data", "Fig S6_point_supp.rds"), trust = TRUE)

# Figure S2 ------------------------------------------------------------------------------------------------
heatmap_df_main <- import(here("data", "Fig S2_main.rds"), trust = TRUE)
heatmap_df_main_point <- import(here("data", "Fig S2_point_main.rds"), trust = TRUE)
heatmap_df_main_tile <- import(here("data", "Fig S2_tile_main.rds"), trust = TRUE)


heatmap_df_bin <- import(here("data", "Fig S2_bin.rds"), trust = TRUE)
heatmap_df_bin_point <- import(here("data", "Fig S2_point_bin.rds"), trust = TRUE)
heatmap_df_bin_tile <- import(here("data", "Fig S2_tile_bin.rds"), trust = TRUE)

# S22 and S23 Table ----------------------------------------------------------------------------------------

s22_data <- data.frame(
  Model_Formula = c(
    "Declining proportion ~ Action * DPSE + Baseline Mean",
    "Declining proportion ~ Action + DPSE + Baseline Mean",
    "Declining proportion ~ Baseline Mean",
    "Declining proportion ~ Action + Baseline Mean"
  ),
  df = c(186, 189, 193, 192),
  logLik = c(-177.98, -190.45, -252.35, -252.22),
  AICc = c(374.94, 393.34, 508.75, 510.57),
  Delta_AICc = c(0, 18.41, 133.82, 135.63),
  weight = c(1, 0, 0, 0)
)

s23_data <- data.frame(
  Indicators = c("Action (DRIVERS)", "Action (USE-DRIVERS)", "Action (RESISTANCE-DRIVERS)", "Action (DRI-DRIVERS)"),
  Odds_Ratio = c(0.62, 4.14, 2.24, 13.82),
  CI = c("0.42 – 0.93", "2.21 – 7.95", "1.10 – 4.63", "1.90 – 327.5"),
  P_value = c(0.021, "<0.001", "0.027", "0.034")
)


# S24 Table ------------------------------------------------------------------------------------------------
s24_data <- import(here("data", "S24 Table.xlsx"))


# Main figure 2 --------------------------------------------------------------------------------------------
#
dpsir <- import(here("data", "new", "Main_fig2_cont.rds"), trust = TRUE)
dat_text <- import(here("data", "new", "Main_fig2_cont_text.rds"), trust = TRUE)
mr2_pred <- import(here("data", "new", "Main_fig2_pred.rds"), trust = TRUE)
dpsir_bin_plot <- import(here("data", "new", "Main_fig2_bin.rds"), trust = TRUE)
dat_text2 <- import(here("data", "new", "Main_fig2_bin_text.rds"), trust = TRUE)
letter_df <- import(here("data", "new", "Main_fig2_letter.df.rds"), trust = TRUE)


# Main figure 3 --------------------------------------------------------------------------------------------

newdataDPSI <- import(here("data", "new", "Main_fig3.rds"), trust = TRUE)
DRIVERS <- import(here("data", "new", "Main_fig3_drivers.rds"), trust = TRUE)
all_wide <- import(here("data", "new", "Main_fig3_allwide.rds"), trust = TRUE)

# Main figure 4 --------------------------------------------------------------------------------------------
rank_df_main <- import(here("data", "new", "Main_fig4.rds"), trust = TRUE)

# Main figure 5 --------------------------------------------------------------------------------------------
aniRmmmmALLig.df <- import(here("data", "new", "Main_fig5.rds"), trust = TRUE)

# Main figure 6 --------------------------------------------------------------------------------------------
#
data_viz <- import(here("data", "new", "Main_fig6_point.rds"), trust = TRUE) %>% 
  mutate(syndrome = forcats::fct_recode(syndrome,
                                        "Virtuous cycle" = "A",
                                        "Meeting challenge" = "B",
                                        "Relaxed response" = "C",
                                        "Vicious cycle" = "D"))
annotation <- import(here("data", "new", "Main_fig6_bar.rds"), trust = TRUE) %>% 
  mutate(syndrome = forcats::fct_recode(syndrome,
                                        "Virtuous cycle" = "A",
                                        "Meeting challenge" = "B",
                                        "Relaxed response" = "C",
                                        "Vicious cycle" = "D"),
         DPSIR = factor(DPSIR, levels = c("DRI", "RESISTANCE", "USE", "DRIVERS")))


# Choropleth plots  ----------------------------------------------------------------------------------------

dpsir_shiny <- import(here("data", "new", "data_shiny.rds"), trust = TRUE) %>% 
  mutate(level = case_when(
    level == "level 1" ~ "Tier 1",
    level == "level 2" ~ "Tier 2",
    level == "level 3" ~ "Tier 3")) %>% 
  # Multiple value of change with -1 when level = "Tier 3", DPSIR = "DRIVERS" and Group %in% c("Sanitations", "Vaccines", "Workforce")
  mutate(change = case_when(
    level == "Tier 3" & DPSIR == "DRIVERS" & GROUP %in% c("Sanitation", "Vaccines", "Workforce") ~ change * -1,
    TRUE ~ change
  ))


# Governance plot ------------------------------------------------------------------------------------------
# countries to label in the plot
highlight_countries <- c("Norway", "Sweden", "Denmark", "USA", "Netherlands", "UK",
                         "Dominican Republic", "Lebanon", "Ecuador", "Ukraine", "Egypt", "Bangladesh")

governance_begin_after <- import(here("data", "new", "Main_fig6_point.rds"), trust = TRUE) %>% 
  filter(GroupR == "TotalResponse") %>%
  select(name, ISO3, init_resp, end_resp, income) %>%
  distinct() %>%
  pivot_longer(cols = c(init_resp, end_resp), names_to = "response", values_to = "value") %>% 
  mutate(Year = case_when(
    response == "init_resp" ~ 2016,
    response == "end_resp" ~ 2023
  )) %>% 
  # keep countries with data for both years
  group_by(name) %>% 
  filter(n() == 2) %>% 
  mutate(max_value_year = which.max(value),
         trend = ifelse(max_value_year == 1, "Decrease", "Increase")) %>% 
  ungroup() %>% 
  mutate(highlight = case_when(
    name %in% c("Germany", "Australia") ~ "Unchange",
    !(name %in% highlight_countries) ~  "Other",
    TRUE ~ trend))

# governance_begin_after %>% 
#   filter(Year == 2016) %>%
#   arrange(value) 
# #Highest initial response: Netherlands, Sweden, Norway, UK, Austria, Spain, France, USA, Denmark, Australia
# #Lowest initial response: Bangladesh, Egypt, Pakistan, Lebanon, Dominican Republic, Ecuador, Ghana, Luxembourg, Ukraine, Bulgaria
# 
# 
# governance_begin_after %>% 
#   filter(Year == 2023) %>%
#   arrange(desc(value)) 
# 
# #Highest final response: Denmark, France, Japan, Malaysia, Norway, USA, Spain, Sweden, UK, China
# #Lowest final response: Dominican Republic, Poland, Lebanon, Romania, Ecuador, Cyprus, Ukraine, Bulgaria, Egypt, UAE
# 




# 
# governance_begin_after %>%
#   ggplot(aes(factor(Year), value, group = name, color = highlight)) +
#   geom_line(aes(size = ifelse(highlight == "Other", 0.1, 0.7))) +
#   # use 2 geoms to make sure highlighted countries' dots are placed on top
#   geom_point(data = . %>% filter(highlight == "Other"), size = 0.2) +
#   geom_point(data = . %>% filter(highlight != "Other")) +
#   ggrepel::geom_text_repel(
#     data = . %>% filter(highlight != "Other"),
#     aes(x = ifelse(Year == min(Year), 1-0.05, 2+0.05),
#         label = glue::glue("{name} ({scales::number(value, accuracy = 0.01)})"),
#         hjust = ifelse(Year == min(Year), 0.8, 0)),
#     size = 2.5, nudge_x = 0, direction = "y", family = "Fira Sans",
#     segment.size = 0) +
#   scale_x_discrete(position = "top") +
#   scale_size_identity() +
#   coord_cartesian(clip = "off") +
#   scale_color_manual(
#     values = c("other" = "grey60", "Increase" = "#092044", "Decrease" = "#C33C2E",
#                "same" = colorspace::darken("#F0C94C", 0.2))) +
#   guides(col = "none") +
#   labs(title = "Governance action scrore of 2016 and 2023",
#        subtitle = "Governance action scrore reported in Tracking AMR Country Self-Assessment Survey (TrACSS) <b style='color:#092044'>increased</b>
#        in most countries, which can be interpreted as a sign of progress.
#        Notable exception where goverance score <b style='color:#C33C2E'>decreased</b>, is the
#        Netherlands.") +
#   theme_minimal(base_family = "Fira Sans") +
#   theme (
#     plot.background = element_rect(color = NA, fill = "white"),
#     panel.grid = element_blank(),
#     panel.grid.major.x = element_line(color = "#ECEEF2", size = 5),
#     text = element_text(color = "#555555"),
#     axis.title = element_blank(),
#     axis.text.x = element_text(size = 12, face = "bold", color = "grey38"),
#     axis.text.y = element_blank(),
#     plot.margin = margin(t = 6, l = 16, r = 16, b = 4),
#     plot.title = element_text(family = "Playfair Display", size = 14, color = "grey12"),
#     plot.subtitle = element_textbox_simple(
#       margin = margin(t = 6, b = 12)
#     ),
#     plot.caption = element_markdown(
#       hjust = 0, margin = margin(t = 8))
#   )

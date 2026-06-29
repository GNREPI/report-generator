# File: 07_age_pyramids.R ----
# Purpose: Generate age and gender distribution pyramids for all disease programs
# Author: Allene Stephens
# Date: 2026
#
# This script creates age pyramids (population pyramids) showing the distribution
# of cases by age group and gender for each disease program at the district level.
#
# Pyramids are generated for:
# - Vaccine Preventable Diseases (VPD)
# - Enteric Diseases
# - Vectorborne Diseases (VBD)
#
#-------------------------------------------------------------------------------

# Setup ----
source(here("R", "packages.R"))
source(here("R", "functions.R"))
source(here("R", "brand_theme.R"))

# Load de-identified data ----
# Data is already de-identified (no patientid/dob) and age_group is pre-computed.
# Load once and filter by disease_program for each program.
message("Loading and processing data with age groups...")

combo_deid <- read.csv(here("output", "combo_deidentified_clean.csv"))

# Vaccine Preventable Data
vax_deidentified_cleaned <- combo_deid |>
  dplyr::filter(disease_program == "Vaccine Preventable")

# Enteric Data
enteric_deidentified_cleaned <- combo_deid |>
  dplyr::filter(disease_program == "Enteric")

# Vectorborne Data
VBD_deidentified_cleaned <- combo_deid |>
  dplyr::filter(disease_program == "Vectorborne")

# Generate District-Level Age Pyramids ----
message("Generating district-level age pyramids...")

# Vaccine Preventable Diseases
vpds_district_pyramid <- generate_age_pyramid(
  data = vax_deidentified_cleaned,
  title_text = "Vaccine Preventable Diseases - Age and Gender Distribution",
  disease_program = "District Overview (Gwinnett, Newton, Rockdale)"
)

# Enteric Diseases
enterics_district_pyramid <- generate_age_pyramid(
  data = enteric_deidentified_cleaned,
  title_text = "Enteric Diseases - Age and Gender Distribution",
  disease_program = "District Overview (Gwinnett, Newton, Rockdale)"
)

# Vectorborne Diseases
vbds_district_pyramid <- generate_age_pyramid(
  data = VBD_deidentified_cleaned,
  title_text = "Vectorborne Diseases - Age and Gender Distribution",
  disease_program = "District Overview (Gwinnett, Newton, Rockdale)"
)

message("✅ District-level pyramids created successfully")

# Generate County-Level Age Pyramids (Optional) ----
# Uncomment to generate county-specific pyramids

# Gwinnett County
# gwinnett_vax_pyramid <- vax_deidentified_cleaned %>%
#   filter(county == "Gwinnett") %>%
#   generate_age_pyramid(
#     title_text = "Vaccine Preventable Diseases - Gwinnett County",
#     disease_program = "Age and Gender Distribution"
#   )

# Newton County
# newton_vax_pyramid <- vax_deidentified_cleaned %>%
#   filter(county == "Newton") %>%
#   generate_age_pyramid(
#     title_text = "Vaccine Preventable Diseases - Newton County",
#     disease_program = "Age and Gender Distribution"
#   )

# Rockdale County
# rockdale_vax_pyramid <- vax_deidentified_cleaned %>%
#   filter(county == "Rockdale") %>%
#   generate_age_pyramid(
#     title_text = "Vaccine Preventable Diseases - Rockdale County",
#     disease_program = "Age and Gender Distribution"
#   )

# Save Age Pyramid Objects ----
message("Saving age pyramid objects...")

output_dir <- here("output", "age_pyramids")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save individual pyramids
saveRDS(vpds_district_pyramid, here(output_dir, "vpds_district_pyramid.rds"))
saveRDS(enterics_district_pyramid, here(output_dir, "enterics_district_pyramid.rds"))
saveRDS(vbds_district_pyramid, here(output_dir, "vbds_district_pyramid.rds"))

message("✅ Age pyramid objects saved to: ", output_dir)

# Combine all pyramids into a master list (for dashboard integration)
all_age_pyramids <- list(
  VPD = vpds_district_pyramid,
  Enteric = enterics_district_pyramid,
  VBD = vbds_district_pyramid
)

saveRDS(all_age_pyramids, here(output_dir, "all_district_age_pyramids.rds"))
message("✅ Master age pyramid list saved")

# Summary Statistics ----
message("\n📊 Age Pyramid Summary Statistics:\n")

# VPD summary
cat("VACCINE PREVENTABLE DISEASES\n")
cat("Total cases:", nrow(vax_deidentified_cleaned), "\n")
cat("Gender distribution:\n")
print(vax_deidentified_cleaned %>%
  filter(gender %in% c("MALE", "FEMALE")) %>%
  count(gender))
cat("\nAge group distribution:\n")
print(vax_deidentified_cleaned %>%
  count(age_group) %>%
  arrange(desc(n)))

cat("\n---\n")

# Enteric summary
cat("ENTERIC DISEASES\n")
cat("Total cases:", nrow(enteric_deidentified_cleaned), "\n")
cat("Gender distribution:\n")
print(enteric_deidentified_cleaned %>%
  filter(gender %in% c("MALE", "FEMALE")) %>%
  count(gender))
cat("\nAge group distribution:\n")
print(enteric_deidentified_cleaned %>%
  count(age_group) %>%
  arrange(desc(n)))

cat("\n---\n")

# VBD summary
cat("VECTORBORNE DISEASES\n")
cat("Total cases:", nrow(VBD_deidentified_cleaned), "\n")
cat("Gender distribution:\n")
print(VBD_deidentified_cleaned %>%
  filter(gender %in% c("MALE", "FEMALE")) %>%
  count(gender))
cat("\nAge group distribution:\n")
print(VBD_deidentified_cleaned %>%
  count(age_group) %>%
  arrange(desc(n)))

message("\n✅ Age pyramid generation complete!")

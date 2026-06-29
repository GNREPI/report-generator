# Clean VBD Data --------------------------------------------------------------

#Import data
VBD_Dashboard <- import(here:here("exports","VBDs_deidentified_cleaned.csv") %>% 
                          
                          # Filter data
                          filter(adminstatus %in% c("CONFIRMED", "PROBABLE")) %>% 
                          
                          # recode gender
                          mutate(gender = recode(gender,  
                                                 "MALE" = "m",
                                                 "FEMALE" = "f",
                                                 .default = "Unknown"),
                                 gender = replace_na(gender, "Unknown")) %>% 
                          
                          # # Rename Diseases
                          # #  mutate(disease = recode(disease, 
                          #                           "DENGUE" = "Dengue",
                          #                           "MALARIA" = "Malaria",
                          #                           "ROCKY MOUNTAIN SPOTTED FEVER" = "Rocky Mountain Spotted Fever",
                          #                           "WEST NILE (WNV) INFECTION" = "West Nile Virus",
                          #                           "EHRLICHIA INFECTION, OTHER SPP. OR UNSPECIATED" = "Ehrlichia"))
                          
                          # Convert to title case
                          mutate(
                            county = to_title_case(county),
                            race = to_title_case(race),
                            ethnicity = to_title_case(ethnicity),
                            adminstatus = to_title_case(adminstatus),
                            disease = to_title_case(disease)) %>% 
                          
                          
                          
                          # reorder columns
                          select(pseudo_patient_id, disease, adminstatus, county, gender, race, 
                                 ethnicity, doo, age_group) 
                        
                        # Define the accessible color palette ONCE
                        # The `n` argument should be the number of unique values in your `color` variable (~disease)
                        accessible_colors <- viridis(n_distinct(VBD_Dashboard$disease), option = "cividis")
                        
                        # THIS IS THE KEY STEP: Save the cleaned object to a file.
                        # The .rds format is best for R objects, as it saves all the data types and attributes.
                        saveRDS(VBD_deidentified_cleaned, here::here("data", "VBDs_deidentified_cleaned.rds"))
                        
                        # Your R Markdown file is now much cleaner and more focused. Now import the
                        # pre-cleaned data into the .rmd file.
                        
                        
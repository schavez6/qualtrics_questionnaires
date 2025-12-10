Run these scripts prior to running scripts in questionnaire-specific folders.

If the goal is only to update our local cas files, deploy:
  1. Raw_Data_Pull.Rmd
  2. Redcap_Setup.Rmd
  3. Prep_Dates.Rmd

If the goal is to update the daily surveys, use the DSR folder to avoid overriding duplicates from the same wave.
Deploy the raw data pull script, followed by the 'long' export script.

If the goal is to complile a data package for NDA upload, follow these steps:
  1. Raw_Data_Pull.Rmd
  2. Redcap_Setup.Rmd
  3. NDA_Submission_Setup/Guid_Prep.Rmd
  4. NDA_Submission_Setup/

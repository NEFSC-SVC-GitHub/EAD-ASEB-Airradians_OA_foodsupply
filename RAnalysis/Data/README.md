# *Argopecten irradians* OA experiment(s)


### /RAnalysis/Data
 Folder contains raw data for the respiration rate, length data, feeding rate (clearance rate). and seawater chemistry.

#### notes:
- **/Respiration**
  - subfolders of raw respiration data sorted by date
  - Lengths_resp.csv - shell length master file to normalize rates
  - Reference_master.csv - a master index of chamber-to-replicate calls to identify treatments and replicate down wellers for each respiration rate value (we used an 8-channel Loligo and a 24-channel SDR Presens system)
- **/Feeding_rates**
  - 'feeding_rates_with_respiration' raw data record in excel format (with tabs)
  - '' ''equivalent files as .csv for analysis.
  - notes: feeding rates were completed with the *same individuals used for respirometry* and measured after a short acclimation period following respirometry. Thus, can call the lengths in the subfolder '/Respiration' to normalize feeding rates
- **/Seawater_chemistry**
  - 'Water_Chemistry_Scallops_2021' raw data record in excel format (with tabs)
  - '' same raw data organized to single tab .csv for analysis
- **ExperimentMetdata.csv**
  - summarizes the array of identifiers (replicate tanks, treatments, etc.) to call data for stats and visualizations.

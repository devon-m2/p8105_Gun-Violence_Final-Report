Exploring Gun Violence Trends in the United States
================
Yuelin Long (yl3181), Marisa Sobel (ms5533), Eileen Shea (eas2297), Zanis Fang (zf2213), Devon Morgan (dm3175)
12/3/2018

Motivation
----------

Gun violence has been of increasing concern across the United States over the past decade. Our objective was to develop an online resource to explore general trends in gun violence and identify any important variables associated with gun violence incidence. The target audience for this webpage includes the general public, gun violence activists and legislators. This goal of this resource is to raise awareness on the widespread impact of gun violence, and inform future public health interventions and policy changes.

Related Work
------------

Gun violence has received widespread media attention in recent years, particularly due to the increased occurrence of mass shootings. Preventing firearm injury and death has increasingly been considered a major public health issue.

Inspiration was drawn from a variety of background sources including:

-   [Gun Violence Archive](https://www.gunviolencearchive.org/), a nonprofit focused on providing data on gun related incidents in the U.S., which provides helpful visuals related to gun violence incidents across the country.
-   [Wintemute, G. J. (2015). The epidemiology of firearm violence in the twenty-first century United States. Annual Review of Public Health, 36, 5-19.](https://www.annualreviews.org/doi/full/10.1146/annurev-publhealth-031914-122535)
-   [Gun Violence - National Institute for Justice](https://www.nij.gov/topics/crime/gun-violence/pages/welcome.aspx)
-   [Giffords Law Center](https://lawcenter.giffords.org)

The Data Science methods used for this project draw from topics covered throughout P8105. In particular, inspiration on some of the map visualizations was drawn from the in-class visualizations on the Airbnb dataset. Further, as a team comprised of biostatisticians, epidemiologists and environmental health scientists in training, we successfully drew from different areas of expertise to inform the direction of the project.

Initial Questions
-----------------

Questions to address in the project include:

-   How do rates of gun violence differ across states? Which areas have the highest concentrations of gun violence?
-   How have gun violence rates changed over time?
-   Which age groups and races are most impacted by gun violence mortality, and has this changed over time?
-   How does gun law strength relate to firearm mortality? How do gun law strengths differ across the U.S.?
-   How are unemployment rates related to firearm mortality?
-   How are gun license approval rates related to firearm mortality and law strength?
-   How does the gun application percentage(proportion of people applying for the license in total population) relate to firearm mortality and law strength?

Data Sources and Cleaning Method
--------------------------------

Data from five sources were considered for this project. The data sources, scraping methods and cleaning procedures for each dataset are described below.

The Github repository for this project can be found [here](https://github.com/ChristineLong/p8105_Final_Project).

### Gun Violence Incident Data (Gun Violence Archive)

Data on specific Gun Violence incidents throughout the U.S. was downloaded from a data repository [here](https://www.kaggle.com/jameslko/gun-violence-data), which pulls data from the [Gun Violence Archive](http://www.shootingtracker.com/), a "online archive of gun violence incidents collected from over 2,500 media, law enforcement, government and commercial sources daily in an effort to provide near-real time data about the results of gun violence. GVA in an independent data collection and research group with no affiliation with any advocacy organization."

The dataset used from

#### Cleaning

### CDC Firearm Mortality Data

Data on Firearm Mortality was obtained from the [CDC Wonder data query](https://wonder.cdc.gov/ucd-icd10.html). Data was queried on November 13, 2018. Two queries were carried out including the following variables: 1. Firearm mortality: all - `state`, `year`, `n_deaths` (number of firearm deaths), `n_population` (state population), and `crude_rate` (provided by CDC - crude\_rate = \[n\_deaths/n\_population\]\*100000, and 2. Firearm mortality: By Age groups and Race - `state`, `year`, `n_deaths` (number of firearm deaths), `n_population` (state population), `crude_rate`, `age_group` (age ranges for victims), `race` (race of victim - Black or African American, White, American Indian or Alaska Native, Asian or Pacific Islander),

`hispanic_origin` ()

Note that these were pulled in three batches because CDC Wonder database puts limits on the number of variables to query at once.

#### Cleaning

The following steps were used to clean the CDC Firearm data, including adding state abbreviations, and changing `age_groups` into a factor variable.

``` r
# Detailed Firearm Mortality Datatsets by Race, Age
firearm_mortality = read_csv("./data/cdc_firearm_mortality_data.csv", na = "Unreliable") %>% 
    janitor::clean_names() %>% 
    select(-ten_year_age_groups_code, -injury_mechanism_all_other_leading_causes_code, -race_code, death_cause= injury_mechanism_all_other_leading_causes) %>% 
    mutate(ten_year_age_groups = factor(ten_year_age_groups, levels = c("1-4 years", "5-14 years", "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85+ years")))

# Summary Firearm Mortality Dataset by Year
firearm_mortality_summary = read_excel("./data/cdc_firearm_all_ages.xlsx") %>% 
    janitor::clean_names() %>% 
    select(-year_code, -injury_mechanism_all_other_leading_causes_code, death_cause= injury_mechanism_all_other_leading_causes)

# Created table with state name and state abbreviations crosswalk
st_crosswalk = tibble(state = state.name) %>%
   bind_cols(tibble(abb = state.abb)) %>% 
     bind_rows(tibble(state = "District of Columbia", abb = "DC"))

# Joined abbreviation dataset with summary firearm mortality dataset
clean_firearm_mortality = left_join(firearm_mortality_summary, st_crosswalk, by = "state") %>% 
    rename(state_abb = abb)
```

### Gun Law Strength and State Scoring

***Gun law scores of states*** is created by the [Giffords Law Center](https://lawcenter.giffords.org/scorecard/#rankings) using "a comprehensive grading rubric that assigns positive point values to gun safety policies, such as private-sale background checks and extreme risk protection orders, and negative point values to dangerous laws, such as permitless concealed carry".

#### Cleaning

The following

``` r
# website URL
url = "https://lawcenter.giffords.org/scorecard/#rankings"
gun_climate_url = read_html(url)

# extract table and clean data
gun_climate_data = 
    gun_climate_url %>% 
  html_nodes(css = "table") %>% 
  .[[1]] %>% 
  html_table(header = TRUE) %>% 
  as.tibble() %>% 
    janitor::clean_names() %>% 
    rename(
        law_strength = gun_law_strength_ranked, 
        grade_2017 = x2017grade, 
        death_rate_rank = gun_death_rate_ranked, 
        death_rate = gun_death_rate_per_100k) %>% 
    mutate(
    grade_2017 = factor(grade_2017, 
                      levels = c("A", "A-", "B+", "B", "C+", "C", "C-", "D", "D-", "F")), 
    grade_2017 = fct_collapse(grade_2017, 
                                                        A = c("A", "A-"), 
                                                        B = c("B+", "B"), 
                                                        C = c("C+", "C", "C-"), 
                                                        D = c("D", "D-")), 
    state_abb = state.abb) %>% 
        select(-state, -death_rate_rank, -death_rate)
```

### Gun Approval Rate and Licensing of States

Data on gun license applications was taken from the [The NICS background checks](https://www.statista.com/statistics/249687/number-of-background-checks-done-by-the-nics-in-the-us-by-state/) source, used by Federal Firearms Licensees (FFLs) to instantly determine whether a prospective buyer is eligible to buy firearms or explosives. This data is used to approximate how many people tried to apply for license in each state.

Data on [The number of federal firearms licensees in the U.S.](https://www.statista.com/statistics/215670/number-of-federal-firearms-licensees-in-the-us-by-state/) was downloaded from Statistica.com. This source pulls data from the U.S. Bureau of Alcohol, Tobacco, Firearms and Explosives (ATF), and is used to approximate how many people actually got approved for gun licenses in each state.

#### Cleaning

The following steps were used to clean the gun approval rate and license data, including renaming `state` variable. The approval rate uses the proportion of licensees in the total population of the state to indicate the difficulties in getting apprroved for gun license. The application rate uses the proportion of background checks in the total population of the state to indicate people's willingness in applying for guns. For consistency purposes, District of Columbia was not included.

``` r
clean_fun = function(address, area){
  readxl::read_xlsx(address, sheet = "Data", range = area) %>% 
  rename(state = X__1) %>% 
  janitor::clean_names()
}


total_pop = clean_fun("./data/population-in-the-states-of-the-us-as-of-2017.xlsx", "B5:C56")

approved_lic = clean_fun("./data/number-of-federal-firearms-licensees-in-the-us-in-2017-by-state.xlsx", "B5:C57") %>% 
  filter(state != "Other Territories")

back_check = clean_fun("./data/nics-background-checks-done-by-us-firearms-licensees-2017-by-state.xlsx", "B5:C56")

gun_lic = inner_join(approved_lic, back_check, by = "state") %>% 
  inner_join(total_pop, by = "state") %>% 
  mutate(approval_rate = number_of_federal_firearms_licensees/number_of_residents_in_millions/1000000,
         application_rate = number_of_background_checks/number_of_residents_in_millions/1000000)
```

Exploratory Analyses
--------------------

Decided to use the data on firearm mortality from the CDC.

Decided to not focus on subtypes of gun violence, including mass shootings versus domestic violence, but gun violence in general. We acknowledge that risk factors could differ across sub-types. Important to consider in the future -- overall

Regression Analyses
-------------------

### Section 4: Regression Analyses

Discussion and Results
----------------------

### Section 1: Overview of Gun Violence in United States

### Section 2: Exploration of Firearm Mortality in United States

Map visualizing the change in crude death rate over time (1999-2016) for each state.

### Section 3: Role of Gun Control in Gun Violence

### Section 4: Regression Analyses

Exploring Gun Violence Trends in the United States
================
Yuelin Long (yl3181), Marisa Sobel (ms5533), Eileen Shea (eas2297), Zanis Fang (zf2213), Devon Morgan (dm3175)
12/3/2018

Motivation
----------

Gun violence has been of increasing concern across the United States over the past decade. Our objective was to develop an online resource to explore general trends in gun violence and identify any important variables associated with gun violence incidence. The target audience for this webpage includes the general public, gun violence activists and legislators. The goal of this resource is to raise awareness on the widespread impact of gun violence, and inform future public health interventions and policy changes.

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

-   The Github repository for this project can be found [here](https://github.com/ChristineLong/p8105_Final_Project).

-   The final website can be found [here](https://mhsobel.github.io/p8105_Final.github.io/), and corresponding Github repository for the website is located [here](https://github.com/mhsobel/p8105_Final.github.io).

-   The final report is housed in a repository [here](https://github.com/devon-m2/p8105_Gun-Violence_Final-Report).

### Dataset 1: Gun Violence Incident Data (Gun Violence Archive)

Data on specific Gun Violence incidents throughout the U.S. was downloaded from a Kaggle data repository [here](https://www.kaggle.com/jameslko/gun-violence-data), which pulls data from the [Gun Violence Archive](http://www.shootingtracker.com/). Gun Violence Archive is a non-profit formed in 2013 to provide free online public access to accurate information about gun-related violence in the United States. The archive collects information in incidents "from over 2,500 media, law enforcement, government and commercial sources daily in an effort to provide near-real time data about the results of gun violence. GVA in an independent data collection and research group with no affiliation with any advocacy organization." The dataset accessed from Kaggle scraped and partially tided the data from Gun Violence Archive.

#### Cleaning

Data cleaning steps included making individual variables for day, month, and year and creating new variables for the number affected in a given gun violence incident (`n_affected`) and that classify mass shooting status (`mass_shooting`).

``` r
gun_violence_data = read_csv("./gun_violence_data/gun_violence_data_2013_2018.csv")

gun_v_tidy = 
    gun_violence_data %>% 
    select(date:city_or_county, n_killed, n_injured, latitude, longitude) %>% 
    separate(date, into = c("year", "month", "day"), sep = "-") %>% 
    mutate(n_affected = n_killed + n_injured) %>%
    filter(n_affected > 0) %>% 
    mutate(mass_shooting = ifelse(n_affected >= 4, "Yes", "No"))
```

### Dataset 2: CDC Firearm Mortality Data

Data on Firearm Mortality was obtained from the [CDC Wonder data query](https://wonder.cdc.gov/ucd-icd10.html). Data was queried on November 13, 2018. Two queries were carried out including the following variables: 1. Firearm mortality: all - `state`, `year`, `n_deaths` (number of firearm deaths), `n_population` (state population), and `crude_rate` (provided by CDC - crude\_rate = \[n\_deaths/n\_population\]\*100000, and 2. Firearm mortality: By Age groups and Race - `state`, `year`, `n_deaths` (number of firearm deaths), `n_population` (state population), `crude_rate`, `age_group` (age ranges for victims), `race` (race of victim - Black or African American, White, American Indian or Alaska Native, Asian or Pacific Islander),

`hispanic_origin` ()

Note that these were pulled in three batches because CDC Wonder database puts limits on the number of variables to query at once.

Centers for Disease Control and Prevention, National Center for Health Statistics. Underlying Cause of Death 1999-2016 on CDC WONDER Online Database, released December, 2017. Data are from the Multiple Cause of Death Files, 1999-2016, as compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program.

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

### Dataset 3: Gun Law Strength and State Scoring

***Gun law scores of states*** data was taken from the [Giffords Law Center](https://lawcenter.giffords.org/scorecard/#rankings). Gun law scores were developed using "a comprehensive grading rubric that assigns positive point values to gun safety policies, such as private-sale background checks and extreme risk protection orders, and negative point values to dangerous laws, such as permitless concealed carry".

The search for gun law strength came from an interest in the state-specific gun climate. The Gifford’s Law Center Gun Law Scorecard resulted from a quick Google search, and neatly implemented many of the factors we had thought independly would influence the gun climate. The Scorecard factors in different types of gun policies that protect citizens from gun violence, and others that protect tangentially, such as domestic violence.

#### Cleaning

The Scorecard report exists on the Gifford’s Law Center’s website as a table. A css table was extracted from the URL that included all the data from the Scorecard in an already tidied manner, as each row was a different state and each column was a different variable. Variables were cleaned up and renamed for ease. For sake of easier comparisons, the grade scale was collapsed from 10 levels to 5, removing the pluses and minuses and keeping the alphabetic grade. The death rate and death rate rank were removed as we were obtaining this information from other sources.

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

### Dataset 4: Gun Approval Rate and Licensing of States

Data on gun license applications was taken from the [The NICS background checks](https://www.statista.com/statistics/249687/number-of-background-checks-done-by-the-nics-in-the-us-by-state/) source, used by Federal Firearms Licensees (FFLs) to instantly determine whether a prospective buyer is eligible to buy firearms or explosives. This data is used to approximate how many people tried to apply for license in each state.

Data on [The number of federal firearms licensees in the U.S.](https://www.statista.com/statistics/215670/number-of-federal-firearms-licensees-in-the-us-by-state/) was downloaded from Statistica.com. This source pulls data from the U.S. Bureau of Alcohol, Tobacco, Firearms and Explosives (ATF), and is used to approximate how many people actually got approved for gun licenses in each state.

Data on total population was taken from [The total population across different states in the U.S.](https://www.statista.com/statistics/183497/population-in-the-federal-states-of-the-us/) source. It was used to standardize number of approved license and application numbers in different states.

#### Cleaning

The following steps were used to clean the gun approval rate and license data, including renaming `state` variable. The approval rate uses the proportion of licensees in the total population of the state to indicate the difficulties in getting apprroved for gun license. The application rate uses the proportion of background checks in the total population of the state to indicate people's willingness in applying for guns. For consistency purposes, District of Columbia was not included.

``` r
# Create a function to import and clean data
clean_fun = function(address, area){
  readxl::read_xlsx(address, sheet = "Data", range = area) %>% 
  rename(state = X__1) %>% 
  janitor::clean_names()
}

# Import the total population data 
total_pop = clean_fun("./data/population-in-the-states-of-the-us-as-of-2017.xlsx", "B5:C56")

# Import the approved license data
approved_lic = clean_fun("./data/number-of-federal-firearms-licensees-in-the-us-in-2017-by-state.xlsx", "B5:C57") %>% 
  filter(state != "Other Territories")

# Import the background check data
back_check = clean_fun("./data/nics-background-checks-done-by-us-firearms-licensees-2017-by-state.xlsx", "B5:C56")

# Merge the datasets together
gun_lic = inner_join(approved_lic, back_check, by = "state") %>% 
  inner_join(total_pop, by = "state") %>% 
  mutate(approval_rate = number_of_federal_firearms_licensees/number_of_residents_in_millions/1000000,
         application_rate = number_of_background_checks/number_of_residents_in_millions/1000000)
```

##### Numerical variables

``` r
gun_control = gun_climate_data %>% 
  inner_join(gun_lic, by = "state") %>% 
  inner_join(clean_firearm_mortality, by = "state")
skimr::skim(gun_control) %>%
            select(variable:stat,value) %>%
            filter((stat != "hist") , (stat != "top_counts"),(type %in% c("numeric","integer"))) %>% 
            spread(key = stat,value = value) %>% 
  knitr::kable(digits = 1) 
```

##### Categorical variables

``` r
skimr::skim(gun_control) %>%
            select(variable:stat,value) %>%
            filter((stat != "hist") , (stat != "top_counts"),(type %in% c("character","factor"))) %>% 
            spread(key = stat,value = value) %>% 
  knitr::kable(digits = 1) 
```

As we can see, the dataset has no missing data. All the information, including population statistics, background check / applications statistics, approved licensees statistics, are generalized for different states (except for D.C.).

### Dataset 5: Unemployment data

#### Cleaning

Exploratory Analyses
--------------------

Exploratory analyses included summarizing the number of gun violence incidents by states and visualizing this through a bubble map, as well as looking at time trends. A decision was ultimately made to favor the interactive map over the bubble map because of its greater descriptive power and to not include time trend analyses regarding this dataset since the span from 2014-2017 (only full years of data collection) proved too short to notice anything significant/interesting.

Decided to use the data on firearm mortality from the CDC.

Decided to not focus on subtypes of gun violence, including mass shootings versus domestic violence, but gun violence in general. We acknowledge that risk factors could differ across sub-types. Important to consider in the future -- overall

Preliminary maps were made with ggmap showing a gradient of gun law strength as well as the categorical grades.

Regression Analyses
-------------------

Section 4: Regression Analyses

Discussion and Results
----------------------

### Section 1: Overview of Gun Violence in United States

### Section 2: Exploration of Firearm Mortality in United States

Section 2 explores changes in the crude firearm mortality, law strength and unemployment rates across the United States over time. This section is housed in a Shiny dashboard to enable users to toggle between different variables and years. This section pulls data from the **Dataset 2: CDC Firearm Mortality Data**, **Dataset 3: Gun Law Strength and State Scoring**, and **Dataset 5: Unemployment data** sources.

#### Map visualizing the change in crude death rate over time (1999-2016) for each state

-   Crude death rate (CDC data), unemployment, law strength

#### Breakdown of crude death rate over time (1999-2016) by race and age group

#### Breakdown of crude death rate over time (1999-2016) by hispanic origin and age group

### Section 3: Role of Gun Control

Section 3 explores the role of gun control in gun violence, pulling data from the **Dataset 4: Gun Approval Rate and Licensing of States**, **Dataset 3: Gun Law Strength and State Scoring**, and **Dataset 2: CDC Firearm Mortality Data** sources.

#### Approved license vs. law strength

The first plot show the relationship between the percentage of people get approved for gun licenses, mortality rate and the law strength in each state.

``` r
gun_control %>% 
  mutate(text_label = str_c("State:",state,  "\nCrude Rate:", crude_rate)) %>% 
  plot_ly(x = ~approval_rate, y = ~law_strength, 
          type = "scatter", mode = "markers", marker = list(size = ~crude_rate),
          alpha = 0.8, 
          color = ~law_strength,
          text = ~ text_label) %>%
  layout(
    title = "Approval percentage vs. Law Strength",
    xaxis = list(title = "Percentage of people approved for guns among total population"), 
    yaxis = list(title = "Law Strength"),
    annotations = list(
      x = 0.0012,
      y = 55,
      text = "Size of dots shows crude rate",
      xref = "x",
      yref = "y",
      ax = 0,
      ay = 0
))
```

As we can see in this plot, in different states, the percentage of people get approved for gun licenses is positively connected with law strength. This is intuitive because it's easier to get approved for guns in states with less strict gun laws. Also, with less strict gun laws, states generally have higher mortality rate. An regression model is built to further explain this relationship in model panel.

#### Application for guns vs. law strength

The second plot shows the relationship application percentage (proportion of people applying for the license in total population), mortality rate and the law strength.

``` r
gun_control %>% 
  mutate(text_label = str_c("State:",state, '\nCrude Rate: ', crude_rate)) %>% 
  plot_ly(x = ~application_rate, y = ~law_strength,
          type = "scatter", mode = "markers", marker = list(size = ~crude_rate),
          alpha = 0.8, 
          color = ~law_strength,
          text = ~ text_label) %>%
  layout(
    title = "Application percentage vs. Law Strength",
    xaxis = list(title = "Percentage of people applied for guns among total population"), 
    yaxis = list(title = "Law Strength"),
    annotations = list(
  x = 0.8,
  y = 55,
  text = "Size of dots shows crude rate",
  xref = "x",
  yref = "y",
  ax = 0,
  ay = 0
))
```

As we can see in the plot,

-   In most states, the percentage of people tried to apply for gun licenses is relatively same across the country, no matter how strict the law is. This shows that people in different state share equal passion to apply for guns.

-   The only exception is Kentucky. According to [Wikipedia](https://en.wikipedia.org/wiki/Gun_laws_in_Kentucky), people don't need to license or permit to own guns for private uses. This explains exceptional passion to apply for guns in Kentucky, leading to an exceptional high proportion of application for background checks. The reason that the total proportion is greater than one might be companies applying for background checks for public gun sales.

### Section 4: Regression Analyses

Conclusion
----------

Demographic maps and tables
================
Arlie McCarthy
2023-10-31

## Importing data

Read the data from the excel spreadsheet of data from round 1 of the
survey and create an object with only the country information.

``` r
knitr::opts_chunk$set(message=FALSE, warnings=FALSE)
DDF_SurveyRound1Results <- read_excel(here::here("data", "DDF_SurveyRound1Results.xlsx"),
sheet = "csv", col_types = c("numeric",
"numeric", "text", "text", "numeric",
"numeric", "numeric", "numeric",
"text", "text", "text", "numeric",
"numeric", "numeric", "numeric",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"numeric", "numeric"))
```

    ## Warning: Expecting numeric in A2 / R2C1: got a date

    ## Warning: Expecting numeric in B2 / R2C2: got a date

    ## Warning: Coercing boolean to numeric in G2 / R2C7

    ## Warning: Expecting numeric in H2 / R2C8: got a date

    ## Warning: Expecting numeric in A3 / R3C1: got a date

    ## Warning: Expecting numeric in B3 / R3C2: got a date

    ## Warning: Coercing boolean to numeric in G3 / R3C7

    ## Warning: Expecting numeric in H3 / R3C8: got a date

    ## Warning: Expecting numeric in A4 / R4C1: got a date

    ## Warning: Expecting numeric in B4 / R4C2: got a date

    ## Warning: Coercing boolean to numeric in G4 / R4C7

    ## Warning: Expecting numeric in H4 / R4C8: got a date

    ## Warning: Expecting numeric in A5 / R5C1: got a date

    ## Warning: Expecting numeric in B5 / R5C2: got a date

    ## Warning: Coercing boolean to numeric in G5 / R5C7

    ## Warning: Expecting numeric in H5 / R5C8: got a date

    ## Warning: Expecting numeric in A6 / R6C1: got a date

    ## Warning: Expecting numeric in B6 / R6C2: got a date

    ## Warning: Coercing boolean to numeric in G6 / R6C7

    ## Warning: Expecting numeric in H6 / R6C8: got a date

    ## Warning: Expecting numeric in A7 / R7C1: got a date

    ## Warning: Expecting numeric in B7 / R7C2: got a date

    ## Warning: Coercing boolean to numeric in G7 / R7C7

    ## Warning: Expecting numeric in H7 / R7C8: got a date

    ## Warning: Expecting numeric in A8 / R8C1: got a date

    ## Warning: Expecting numeric in B8 / R8C2: got a date

    ## Warning: Coercing boolean to numeric in G8 / R8C7

    ## Warning: Expecting numeric in H8 / R8C8: got a date

    ## Warning: Expecting numeric in A9 / R9C1: got a date

    ## Warning: Expecting numeric in B9 / R9C2: got a date

    ## Warning: Coercing boolean to numeric in G9 / R9C7

    ## Warning: Expecting numeric in H9 / R9C8: got a date

    ## Warning: Expecting numeric in A10 / R10C1: got a date

    ## Warning: Expecting numeric in B10 / R10C2: got a date

    ## Warning: Coercing boolean to numeric in G10 / R10C7

    ## Warning: Expecting numeric in H10 / R10C8: got a date

    ## Warning: Expecting numeric in A11 / R11C1: got a date

    ## Warning: Expecting numeric in B11 / R11C2: got a date

    ## Warning: Coercing boolean to numeric in G11 / R11C7

    ## Warning: Expecting numeric in H11 / R11C8: got a date

    ## Warning: Expecting numeric in A12 / R12C1: got a date

    ## Warning: Expecting numeric in B12 / R12C2: got a date

    ## Warning: Coercing boolean to numeric in G12 / R12C7

    ## Warning: Expecting numeric in H12 / R12C8: got a date

    ## Warning: Expecting numeric in A13 / R13C1: got a date

    ## Warning: Expecting numeric in B13 / R13C2: got a date

    ## Warning: Coercing boolean to numeric in G13 / R13C7

    ## Warning: Expecting numeric in H13 / R13C8: got a date

    ## Warning: Expecting numeric in A14 / R14C1: got a date

    ## Warning: Expecting numeric in B14 / R14C2: got a date

    ## Warning: Coercing boolean to numeric in G14 / R14C7

    ## Warning: Expecting numeric in H14 / R14C8: got a date

    ## Warning: Expecting numeric in A15 / R15C1: got a date

    ## Warning: Expecting numeric in B15 / R15C2: got a date

    ## Warning: Coercing boolean to numeric in G15 / R15C7

    ## Warning: Expecting numeric in H15 / R15C8: got a date

    ## Warning: Expecting numeric in A16 / R16C1: got a date

    ## Warning: Expecting numeric in B16 / R16C2: got a date

    ## Warning: Coercing boolean to numeric in G16 / R16C7

    ## Warning: Expecting numeric in H16 / R16C8: got a date

    ## Warning: Expecting numeric in A17 / R17C1: got a date

    ## Warning: Expecting numeric in B17 / R17C2: got a date

    ## Warning: Coercing boolean to numeric in G17 / R17C7

    ## Warning: Expecting numeric in H17 / R17C8: got a date

    ## Warning: Expecting numeric in A18 / R18C1: got a date

    ## Warning: Expecting numeric in B18 / R18C2: got a date

    ## Warning: Coercing boolean to numeric in G18 / R18C7

    ## Warning: Expecting numeric in H18 / R18C8: got a date

    ## Warning: Expecting numeric in A19 / R19C1: got a date

    ## Warning: Expecting numeric in B19 / R19C2: got a date

    ## Warning: Coercing boolean to numeric in G19 / R19C7

    ## Warning: Expecting numeric in H19 / R19C8: got a date

    ## Warning: Expecting numeric in A20 / R20C1: got a date

    ## Warning: Expecting numeric in B20 / R20C2: got a date

    ## Warning: Coercing boolean to numeric in G20 / R20C7

    ## Warning: Expecting numeric in H20 / R20C8: got a date

    ## Warning: Expecting numeric in A21 / R21C1: got a date

    ## Warning: Expecting numeric in B21 / R21C2: got a date

    ## Warning: Coercing boolean to numeric in G21 / R21C7

    ## Warning: Expecting numeric in H21 / R21C8: got a date

    ## Warning: Expecting numeric in A22 / R22C1: got a date

    ## Warning: Expecting numeric in B22 / R22C2: got a date

    ## Warning: Coercing boolean to numeric in G22 / R22C7

    ## Warning: Expecting numeric in H22 / R22C8: got a date

    ## Warning: Expecting numeric in A23 / R23C1: got a date

    ## Warning: Expecting numeric in B23 / R23C2: got a date

    ## Warning: Coercing boolean to numeric in G23 / R23C7

    ## Warning: Expecting numeric in H23 / R23C8: got a date

    ## Warning: Expecting numeric in A24 / R24C1: got a date

    ## Warning: Expecting numeric in B24 / R24C2: got a date

    ## Warning: Coercing boolean to numeric in G24 / R24C7

    ## Warning: Expecting numeric in H24 / R24C8: got a date

    ## Warning: Expecting numeric in A25 / R25C1: got a date

    ## Warning: Expecting numeric in B25 / R25C2: got a date

    ## Warning: Coercing boolean to numeric in G25 / R25C7

    ## Warning: Expecting numeric in H25 / R25C8: got a date

    ## Warning: Expecting numeric in A26 / R26C1: got a date

    ## Warning: Expecting numeric in B26 / R26C2: got a date

    ## Warning: Coercing boolean to numeric in G26 / R26C7

    ## Warning: Expecting numeric in H26 / R26C8: got a date

    ## Warning: Expecting numeric in A27 / R27C1: got a date

    ## Warning: Expecting numeric in B27 / R27C2: got a date

    ## Warning: Coercing boolean to numeric in G27 / R27C7

    ## Warning: Expecting numeric in H27 / R27C8: got a date

    ## Warning: Expecting numeric in A28 / R28C1: got a date

    ## Warning: Expecting numeric in B28 / R28C2: got a date

    ## Warning: Coercing boolean to numeric in G28 / R28C7

    ## Warning: Expecting numeric in H28 / R28C8: got a date

    ## Warning: Expecting numeric in A29 / R29C1: got a date

    ## Warning: Expecting numeric in B29 / R29C2: got a date

    ## Warning: Coercing boolean to numeric in G29 / R29C7

    ## Warning: Expecting numeric in H29 / R29C8: got a date

    ## Warning: Expecting numeric in A30 / R30C1: got a date

    ## Warning: Expecting numeric in B30 / R30C2: got a date

    ## Warning: Coercing boolean to numeric in G30 / R30C7

    ## Warning: Expecting numeric in H30 / R30C8: got a date

    ## Warning: Expecting numeric in A31 / R31C1: got a date

    ## Warning: Expecting numeric in B31 / R31C2: got a date

    ## Warning: Coercing boolean to numeric in G31 / R31C7

    ## Warning: Expecting numeric in H31 / R31C8: got a date

    ## Warning: Expecting numeric in A32 / R32C1: got a date

    ## Warning: Expecting numeric in B32 / R32C2: got a date

    ## Warning: Coercing boolean to numeric in G32 / R32C7

    ## Warning: Expecting numeric in H32 / R32C8: got a date

    ## Warning: Expecting numeric in A33 / R33C1: got a date

    ## Warning: Expecting numeric in B33 / R33C2: got a date

    ## Warning: Coercing boolean to numeric in G33 / R33C7

    ## Warning: Expecting numeric in H33 / R33C8: got a date

    ## Warning: Expecting numeric in A34 / R34C1: got a date

    ## Warning: Expecting numeric in B34 / R34C2: got a date

    ## Warning: Coercing boolean to numeric in G34 / R34C7

    ## Warning: Expecting numeric in H34 / R34C8: got a date

    ## Warning: Expecting numeric in A35 / R35C1: got a date

    ## Warning: Expecting numeric in B35 / R35C2: got a date

    ## Warning: Coercing boolean to numeric in G35 / R35C7

    ## Warning: Expecting numeric in H35 / R35C8: got a date

    ## Warning: Expecting numeric in A36 / R36C1: got a date

    ## Warning: Expecting numeric in B36 / R36C2: got a date

    ## Warning: Coercing boolean to numeric in G36 / R36C7

    ## Warning: Expecting numeric in H36 / R36C8: got a date

    ## Warning: Expecting numeric in A37 / R37C1: got a date

    ## Warning: Expecting numeric in B37 / R37C2: got a date

    ## Warning: Coercing boolean to numeric in G37 / R37C7

    ## Warning: Expecting numeric in H37 / R37C8: got a date

    ## Warning: Expecting numeric in A38 / R38C1: got a date

    ## Warning: Expecting numeric in B38 / R38C2: got a date

    ## Warning: Coercing boolean to numeric in G38 / R38C7

    ## Warning: Expecting numeric in H38 / R38C8: got a date

    ## Warning: Expecting numeric in A39 / R39C1: got a date

    ## Warning: Expecting numeric in B39 / R39C2: got a date

    ## Warning: Coercing boolean to numeric in G39 / R39C7

    ## Warning: Expecting numeric in H39 / R39C8: got a date

    ## Warning: Expecting numeric in A40 / R40C1: got a date

    ## Warning: Expecting numeric in B40 / R40C2: got a date

    ## Warning: Coercing boolean to numeric in G40 / R40C7

    ## Warning: Expecting numeric in H40 / R40C8: got a date

    ## Warning: Expecting numeric in A41 / R41C1: got a date

    ## Warning: Expecting numeric in B41 / R41C2: got a date

    ## Warning: Coercing boolean to numeric in G41 / R41C7

    ## Warning: Expecting numeric in H41 / R41C8: got a date

    ## Warning: Expecting numeric in A42 / R42C1: got a date

    ## Warning: Expecting numeric in B42 / R42C2: got a date

    ## Warning: Coercing boolean to numeric in G42 / R42C7

    ## Warning: Expecting numeric in H42 / R42C8: got a date

    ## Warning: Expecting numeric in A43 / R43C1: got a date

    ## Warning: Expecting numeric in B43 / R43C2: got a date

    ## Warning: Coercing boolean to numeric in G43 / R43C7

    ## Warning: Expecting numeric in H43 / R43C8: got a date

    ## Warning: Expecting numeric in A44 / R44C1: got a date

    ## Warning: Expecting numeric in B44 / R44C2: got a date

    ## Warning: Coercing boolean to numeric in G44 / R44C7

    ## Warning: Expecting numeric in H44 / R44C8: got a date

    ## Warning: Expecting numeric in A45 / R45C1: got a date

    ## Warning: Expecting numeric in B45 / R45C2: got a date

    ## Warning: Coercing boolean to numeric in G45 / R45C7

    ## Warning: Expecting numeric in H45 / R45C8: got a date

    ## Warning: Expecting numeric in A46 / R46C1: got a date

    ## Warning: Expecting numeric in B46 / R46C2: got a date

    ## Warning: Coercing boolean to numeric in G46 / R46C7

    ## Warning: Expecting numeric in H46 / R46C8: got a date

    ## Warning: Expecting numeric in A47 / R47C1: got a date

    ## Warning: Expecting numeric in B47 / R47C2: got a date

    ## Warning: Coercing boolean to numeric in G47 / R47C7

    ## Warning: Expecting numeric in H47 / R47C8: got a date

    ## Warning: Expecting numeric in A48 / R48C1: got a date

    ## Warning: Expecting numeric in B48 / R48C2: got a date

    ## Warning: Coercing boolean to numeric in G48 / R48C7

    ## Warning: Expecting numeric in H48 / R48C8: got a date

    ## Warning: Expecting numeric in A49 / R49C1: got a date

    ## Warning: Expecting numeric in B49 / R49C2: got a date

    ## Warning: Coercing boolean to numeric in G49 / R49C7

    ## Warning: Expecting numeric in H49 / R49C8: got a date

    ## Warning: Expecting numeric in A50 / R50C1: got a date

    ## Warning: Expecting numeric in B50 / R50C2: got a date

    ## Warning: Coercing boolean to numeric in G50 / R50C7

    ## Warning: Expecting numeric in H50 / R50C8: got a date

    ## Warning: Expecting numeric in A51 / R51C1: got a date

    ## Warning: Expecting numeric in B51 / R51C2: got a date

    ## Warning: Coercing boolean to numeric in G51 / R51C7

    ## Warning: Expecting numeric in H51 / R51C8: got a date

    ## Warning: Expecting numeric in A52 / R52C1: got a date

    ## Warning: Expecting numeric in B52 / R52C2: got a date

    ## Warning: Coercing boolean to numeric in G52 / R52C7

    ## Warning: Expecting numeric in H52 / R52C8: got a date

    ## Warning: Expecting numeric in A53 / R53C1: got a date

    ## Warning: Expecting numeric in B53 / R53C2: got a date

    ## Warning: Coercing boolean to numeric in G53 / R53C7

    ## Warning: Expecting numeric in H53 / R53C8: got a date

    ## Warning: Expecting numeric in A54 / R54C1: got a date

    ## Warning: Expecting numeric in B54 / R54C2: got a date

    ## Warning: Coercing boolean to numeric in G54 / R54C7

    ## Warning: Expecting numeric in H54 / R54C8: got a date

    ## Warning: Expecting numeric in A55 / R55C1: got a date

    ## Warning: Expecting numeric in B55 / R55C2: got a date

    ## Warning: Coercing boolean to numeric in G55 / R55C7

    ## Warning: Expecting numeric in H55 / R55C8: got a date

    ## Warning: Expecting numeric in A56 / R56C1: got a date

    ## Warning: Expecting numeric in B56 / R56C2: got a date

    ## Warning: Coercing boolean to numeric in G56 / R56C7

    ## Warning: Expecting numeric in H56 / R56C8: got a date

    ## Warning: Expecting numeric in A57 / R57C1: got a date

    ## Warning: Expecting numeric in B57 / R57C2: got a date

    ## Warning: Coercing boolean to numeric in G57 / R57C7

    ## Warning: Expecting numeric in H57 / R57C8: got a date

    ## Warning: Expecting numeric in A58 / R58C1: got a date

    ## Warning: Expecting numeric in B58 / R58C2: got a date

    ## Warning: Coercing boolean to numeric in G58 / R58C7

    ## Warning: Expecting numeric in H58 / R58C8: got a date

    ## Warning: Expecting numeric in A59 / R59C1: got a date

    ## Warning: Expecting numeric in B59 / R59C2: got a date

    ## Warning: Coercing boolean to numeric in G59 / R59C7

    ## Warning: Expecting numeric in H59 / R59C8: got a date

    ## Warning: Expecting numeric in A60 / R60C1: got a date

    ## Warning: Expecting numeric in B60 / R60C2: got a date

    ## Warning: Coercing boolean to numeric in G60 / R60C7

    ## Warning: Expecting numeric in H60 / R60C8: got a date

    ## Warning: Expecting numeric in A61 / R61C1: got a date

    ## Warning: Expecting numeric in B61 / R61C2: got a date

    ## Warning: Coercing boolean to numeric in G61 / R61C7

    ## Warning: Expecting numeric in H61 / R61C8: got a date

    ## Warning: Expecting numeric in A62 / R62C1: got a date

    ## Warning: Expecting numeric in B62 / R62C2: got a date

    ## Warning: Coercing boolean to numeric in G62 / R62C7

    ## Warning: Expecting numeric in H62 / R62C8: got a date

    ## Warning: Expecting numeric in A63 / R63C1: got a date

    ## Warning: Expecting numeric in B63 / R63C2: got a date

    ## Warning: Coercing boolean to numeric in G63 / R63C7

    ## Warning: Expecting numeric in H63 / R63C8: got a date

    ## Warning: Expecting numeric in A64 / R64C1: got a date

    ## Warning: Expecting numeric in B64 / R64C2: got a date

    ## Warning: Coercing boolean to numeric in G64 / R64C7

    ## Warning: Expecting numeric in H64 / R64C8: got a date

    ## Warning: Expecting numeric in A65 / R65C1: got a date

    ## Warning: Expecting numeric in B65 / R65C2: got a date

    ## Warning: Coercing boolean to numeric in G65 / R65C7

    ## Warning: Expecting numeric in H65 / R65C8: got a date

    ## Warning: Expecting numeric in A66 / R66C1: got a date

    ## Warning: Expecting numeric in B66 / R66C2: got a date

    ## Warning: Coercing boolean to numeric in G66 / R66C7

    ## Warning: Expecting numeric in H66 / R66C8: got a date

    ## Warning: Expecting numeric in A67 / R67C1: got a date

    ## Warning: Expecting numeric in B67 / R67C2: got a date

    ## Warning: Coercing boolean to numeric in G67 / R67C7

    ## Warning: Expecting numeric in H67 / R67C8: got a date

    ## Warning: Expecting numeric in A68 / R68C1: got a date

    ## Warning: Expecting numeric in B68 / R68C2: got a date

    ## Warning: Coercing boolean to numeric in G68 / R68C7

    ## Warning: Expecting numeric in H68 / R68C8: got a date

    ## Warning: Expecting numeric in A69 / R69C1: got a date

    ## Warning: Expecting numeric in B69 / R69C2: got a date

    ## Warning: Coercing boolean to numeric in G69 / R69C7

    ## Warning: Expecting numeric in H69 / R69C8: got a date

    ## Warning: Expecting numeric in A70 / R70C1: got a date

    ## Warning: Expecting numeric in B70 / R70C2: got a date

    ## Warning: Coercing boolean to numeric in G70 / R70C7

    ## Warning: Expecting numeric in H70 / R70C8: got a date

    ## Warning: Expecting numeric in A71 / R71C1: got a date

    ## Warning: Expecting numeric in B71 / R71C2: got a date

    ## Warning: Coercing boolean to numeric in G71 / R71C7

    ## Warning: Expecting numeric in H71 / R71C8: got a date

    ## Warning: Expecting numeric in A72 / R72C1: got a date

    ## Warning: Expecting numeric in B72 / R72C2: got a date

    ## Warning: Coercing boolean to numeric in G72 / R72C7

    ## Warning: Expecting numeric in H72 / R72C8: got a date

    ## Warning: Expecting numeric in A73 / R73C1: got a date

    ## Warning: Expecting numeric in B73 / R73C2: got a date

    ## Warning: Coercing boolean to numeric in G73 / R73C7

    ## Warning: Expecting numeric in H73 / R73C8: got a date

    ## Warning: Expecting numeric in A74 / R74C1: got a date

    ## Warning: Expecting numeric in B74 / R74C2: got a date

    ## Warning: Coercing boolean to numeric in G74 / R74C7

    ## Warning: Expecting numeric in H74 / R74C8: got a date

    ## Warning: Expecting numeric in A75 / R75C1: got a date

    ## Warning: Expecting numeric in B75 / R75C2: got a date

    ## Warning: Coercing boolean to numeric in G75 / R75C7

    ## Warning: Expecting numeric in H75 / R75C8: got a date

    ## Warning: Expecting numeric in A76 / R76C1: got a date

    ## Warning: Expecting numeric in B76 / R76C2: got a date

    ## Warning: Coercing boolean to numeric in G76 / R76C7

    ## Warning: Expecting numeric in H76 / R76C8: got a date

    ## Warning: Expecting numeric in A77 / R77C1: got a date

    ## Warning: Expecting numeric in B77 / R77C2: got a date

    ## Warning: Coercing boolean to numeric in G77 / R77C7

    ## Warning: Expecting numeric in H77 / R77C8: got a date

    ## Warning: Expecting numeric in A78 / R78C1: got a date

    ## Warning: Expecting numeric in B78 / R78C2: got a date

    ## Warning: Coercing boolean to numeric in G78 / R78C7

    ## Warning: Expecting numeric in H78 / R78C8: got a date

    ## Warning: Expecting numeric in A79 / R79C1: got a date

    ## Warning: Expecting numeric in B79 / R79C2: got a date

    ## Warning: Coercing boolean to numeric in G79 / R79C7

    ## Warning: Expecting numeric in H79 / R79C8: got a date

    ## Warning: Expecting numeric in A80 / R80C1: got a date

    ## Warning: Expecting numeric in B80 / R80C2: got a date

    ## Warning: Coercing boolean to numeric in G80 / R80C7

    ## Warning: Expecting numeric in H80 / R80C8: got a date

    ## Warning: Expecting numeric in A81 / R81C1: got a date

    ## Warning: Expecting numeric in B81 / R81C2: got a date

    ## Warning: Coercing boolean to numeric in G81 / R81C7

    ## Warning: Expecting numeric in H81 / R81C8: got a date

``` r
country_information <- DDF_SurveyRound1Results %>% 
  mutate(country_from = `1.1000000000000001`, country_worked = `1.2`, oceans_worked = `2.6`,
        sector = `2.2000000000000002`, name = `1.3`) %>% 
  select(name, country_from, country_worked, oceans_worked, sector) %>%
  clean_names()

r2_responses_longer <- read_excel(here::here("data","R2_responses.xlsx"), sheet = "responses")

country_information_r2 <- r2_responses_longer %>% 
  clean_names() %>% 
  mutate(country_worked = country_predominantely_work,
         oceans_worked = ocean_basin) %>% 
  select(name, country_from, country_worked, oceans_worked, sector)

r3_responses_longer <- read_excel(here::here("data", "Rd3_Results_Final.xlsx"), sheet = "responses")
```

    ## New names:
    ## • `3.1` -> `3.1...19`
    ## • `3.1` -> `3.1...21`
    ## • `3.1` -> `3.1...37`

``` r
country_information_r3 <- r3_responses_longer %>% 
  clean_names() %>% 
  select(name, sector)
```

Import the country baselayer for mapping.

``` r
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
```

## Check that the countries given in the survey data align with the countries in the mapping data.

``` r
country_information <- country_information %>% 
  mutate(country_from_in_map = case_when(country_from %in% world$name_long ~ TRUE, 
                                         TRUE ~ FALSE),
         country_worked_in_map = case_when(country_worked %in% world$name_long ~ TRUE, 
                                         TRUE ~ FALSE)
  )
country_information %>% 
  filter(country_from_in_map == FALSE) %>% 
  group_by(country_from) %>% 
  select(-name, -country_worked) %>% 
  slice(1)
```

    ## # A tibble: 3 × 5
    ## # Groups:   country_from [3]
    ##   country_from    oceans_worked sector country_from_in_map country_worked_in_map
    ##   <chr>           <chr>         <chr>  <lgl>               <lgl>                
    ## 1 United Kingdom… Atlantic Oce… Civil… FALSE               FALSE                
    ## 2 United States … Atlantic Oce… Civil… FALSE               FALSE                
    ## 3 <NA>            Pacific Ocea… Civil… FALSE               TRUE

``` r
country_information %>% 
  filter(country_worked_in_map == FALSE) %>% 
  select(-name, -country_from) %>% 
  group_by(country_worked) %>% 
  slice(1)
```

    ## # A tibble: 4 × 5
    ## # Groups:   country_worked [4]
    ##   country_worked  oceans_worked sector country_from_in_map country_worked_in_map
    ##   <chr>           <chr>         <chr>  <lgl>               <lgl>                
    ## 1 Hong Kong (S.A… Atlantic Oce… Acade… FALSE               FALSE                
    ## 2 United Kingdom… Atlantic Oce… Indus… TRUE                FALSE                
    ## 3 United States … Atlantic Oce… Civil… TRUE                FALSE                
    ## 4 <NA>            Atlantic Oce… Acade… TRUE                FALSE

From that we see that the following countries are given as locations but
not listed in the map data: - United Kingdom of Great Britain and
Northern Ireland  
- United States of America  
- Hong Kong (S.A.R.)

The equivalent locations in the map data are: - United Kingdom  
- United States  
- Hong Kong

Therefore need to change the names for plotting so that they align with
the map data.

``` r
#create a consistent country information dataframe for R1
country_information <- country_information %>% 
  mutate(country_from = case_when(country_from == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                                  country_from == "UK" ~ "United Kingdom",
                                  country_from == "United States of America" ~ "United States",
                                  country_from == "Hong Kong (S.A.R.)" ~ "Hong Kong",
                                         TRUE ~ country_from),
         country_worked = case_when(country_worked == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                                    country_worked == "UK" ~ "United Kingdom",
                                    country_worked == "United States of America" ~ "United States",
                                    country_worked == "Hong Kong (S.A.R.)" ~ "Hong Kong",
                                         TRUE ~ country_worked)
  )
#create a consistent country information dataframe for R2
country_information_r2 <- country_information_r2 %>% 
  mutate(country_from = case_when(country_from == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                                  country_from == "UK" ~ "United Kingdom",
                                  country_from == "United States of America" ~ "United States",
                                  country_from == "Hong Kong (S.A.R.)" ~ "Hong Kong",
                                         TRUE ~ country_from),
         country_worked = case_when(country_worked == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                                    country_worked == "UK" ~ "United Kingdom",
                                    country_worked == "United States of America" ~ "United States",
                                    country_worked == "Hong Kong (S.A.R.)" ~ "Hong Kong",
                                         TRUE ~ country_worked)
  )
#create a consistent country information dataframe for R3
country_information_r3 <- country_information_r3 %>% 
  full_join(country_information_r2 %>% select(-sector), by = "name") %>% 
  filter(!is.na(sector))
```

## Reshape and joing country information data to mapping

``` r
#country from
country_from_summarised <- country_information %>% 
  dplyr::group_by(country_from) %>% 
  dplyr::summarise(from = n()) %>% 
  dplyr::rename(name_long = country_from, from_r1 = from) %>% 
  dplyr::mutate(from_log_r1 = log10(from_r1))

#country worked
country_worked_summarised <- country_information %>% 
  dplyr::group_by(country_worked) %>% 
  dplyr::summarise(worked = n()) %>% 
  dplyr::rename(name_long = country_worked, worked_r1 = worked) %>% 
  dplyr::mutate(worked_log_r1 = log10(worked_r1))

#all data
survey_data_summarised <- country_from_summarised %>% 
  left_join(country_worked_summarised)
```

Now, prepare data from rounds 2 and 3 to be added to the above data
frame so that the information from all rounds is in the same place.

``` r
#country from R2
country_from_r2_summarised <- country_information_r2 %>% 
  dplyr::group_by(country_from) %>% 
  dplyr::summarise(from = n()) %>% 
  dplyr::rename(name_long = country_from, from_r2 = from) %>% 
  dplyr::mutate(from_log_r2 = log10(from_r2))
#country worked R2
country_worked_r2_summarised <- country_information_r2 %>% 
  dplyr::group_by(country_worked) %>% 
  dplyr::summarise(worked = n()) %>% 
  dplyr::rename(name_long = country_worked, worked_r2 = worked) %>% 
  dplyr::mutate(worked_log_r2 = log10(worked_r2))
#all data R2
survey_data_summarised_r2 <- country_from_r2_summarised %>% 
  left_join(country_worked_r2_summarised)

#country from R3
country_from_r3_summarised <- country_information_r3 %>% 
  dplyr::group_by(country_from) %>% 
  dplyr::summarise(from = n()) %>% 
  dplyr::rename(name_long = country_from, from_r3 = from) %>% 
  dplyr::mutate(from_log_r3 = log10(from_r3))
#country worked R2
country_worked_r3_summarised <- country_information_r3 %>% 
  dplyr::group_by(country_worked) %>% 
  dplyr::summarise(worked = n()) %>% 
  dplyr::rename(name_long = country_worked, worked_r3 = worked) %>% 
  dplyr::mutate(worked_log_r3 = log10(worked_r3))
#all data R2
survey_data_summarised_r3 <- country_from_r3_summarised %>% 
  left_join(country_worked_r3_summarised)
```

Now combining the data from all rounds to generate maps and tables,

``` r
#join the summary data and gathering economic information
country_summaries <- survey_data_summarised %>% 
  full_join(survey_data_summarised_r2) %>% 
  full_join(survey_data_summarised_r3) %>% 
  left_join(world %>% select(name_long, economy, income_grp, region_wb))

#for maps for R1
world_r1 <- world %>% 
  left_join(survey_data_summarised)
```

Table of countries from and worked

``` r
country_summaries$region_wb <- factor(country_summaries$region_wb, levels = c("Europe & Central Asia", "East Asia & Pacific", "North America", "Latin America & Caribbean", "Sub-Saharan Africa", "South Asia", NA))
country_summary_table <- country_summaries %>% 
  select(region_wb, name_long, income_grp, worked_r1, worked_r2, worked_r3, from_r1, from_r2, from_r3, ) %>% 
  mutate(name_long = ifelse(is.na(name_long), "No answer", name_long),
         income_grp = ifelse(is.na(income_grp), "No answer", income_grp),
         region_wb = ifelse(is.na(region_wb), "No answer", region_wb)) %>% 
  arrange(region_wb, desc(worked_r1)) %>% 
  replace(is.na(.), 0)

country_summary_table %>% 
  kable(booktabs = TRUE) %>%
  kable_styling(font_size = 10)
```

<table class="table" style="font-size: 10px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
region_wb
</th>
<th style="text-align:left;">
name_long
</th>
<th style="text-align:left;">
income_grp
</th>
<th style="text-align:right;">
worked_r1
</th>
<th style="text-align:right;">
worked_r2
</th>
<th style="text-align:right;">
worked_r3
</th>
<th style="text-align:right;">
from_r1
</th>
<th style="text-align:right;">
from_r2
</th>
<th style="text-align:right;">
from_r3
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">

1.  High income: OECD
    </td>
    <td style="text-align:right;">
    27
    </td>
    <td style="text-align:right;">
    16
    </td>
    <td style="text-align:right;">
    10
    </td>
    <td style="text-align:right;">
    27
    </td>
    <td style="text-align:right;">
    18
    </td>
    <td style="text-align:right;">
    11
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    1
    </td>
    <td style="text-align:left;">
    Portugal
    </td>
    <td style="text-align:left;">

    1.  High income: OECD
        </td>
        <td style="text-align:right;">
        3
        </td>
        <td style="text-align:right;">
        3
        </td>
        <td style="text-align:right;">
        3
        </td>
        <td style="text-align:right;">
        3
        </td>
        <td style="text-align:right;">
        2
        </td>
        <td style="text-align:right;">
        2
        </td>
        </tr>
        <tr>
        <td style="text-align:left;">
        1
        </td>
        <td style="text-align:left;">
        Denmark
        </td>
        <td style="text-align:left;">

        1.  High income: OECD
            </td>
            <td style="text-align:right;">
            1
            </td>
            <td style="text-align:right;">
            0
            </td>
            <td style="text-align:right;">
            0
            </td>
            <td style="text-align:right;">
            1
            </td>
            <td style="text-align:right;">
            0
            </td>
            <td style="text-align:right;">
            0
            </td>
            </tr>
            <tr>
            <td style="text-align:left;">
            1
            </td>
            <td style="text-align:left;">
            France
            </td>
            <td style="text-align:left;">

            1.  High income: OECD
                </td>
                <td style="text-align:right;">
                1
                </td>
                <td style="text-align:right;">
                2
                </td>
                <td style="text-align:right;">
                2
                </td>
                <td style="text-align:right;">
                1
                </td>
                <td style="text-align:right;">
                2
                </td>
                <td style="text-align:right;">
                2
                </td>
                </tr>
                <tr>
                <td style="text-align:left;">
                1
                </td>
                <td style="text-align:left;">
                Netherlands
                </td>
                <td style="text-align:left;">

                1.  High income: OECD
                    </td>
                    <td style="text-align:right;">
                    1
                    </td>
                    <td style="text-align:right;">
                    1
                    </td>
                    <td style="text-align:right;">
                    1
                    </td>
                    <td style="text-align:right;">
                    3
                    </td>
                    <td style="text-align:right;">
                    3
                    </td>
                    <td style="text-align:right;">
                    3
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    1
                    </td>
                    <td style="text-align:left;">
                    Germany
                    </td>
                    <td style="text-align:left;">

                    1.  High income: OECD
                        </td>
                        <td style="text-align:right;">
                        0
                        </td>
                        <td style="text-align:right;">
                        0
                        </td>
                        <td style="text-align:right;">
                        0
                        </td>
                        <td style="text-align:right;">
                        1
                        </td>
                        <td style="text-align:right;">
                        0
                        </td>
                        <td style="text-align:right;">
                        0
                        </td>
                        </tr>
                        <tr>
                        <td style="text-align:left;">
                        1
                        </td>
                        <td style="text-align:left;">
                        Italy
                        </td>
                        <td style="text-align:left;">

                        1.  High income: OECD
                            </td>
                            <td style="text-align:right;">
                            0
                            </td>
                            <td style="text-align:right;">
                            0
                            </td>
                            <td style="text-align:right;">
                            0
                            </td>
                            <td style="text-align:right;">
                            1
                            </td>
                            <td style="text-align:right;">
                            1
                            </td>
                            <td style="text-align:right;">
                            1
                            </td>
                            </tr>
                            <tr>
                            <td style="text-align:left;">
                            1
                            </td>
                            <td style="text-align:left;">
                            Luxembourg
                            </td>
                            <td style="text-align:left;">

                            1.  High income: OECD
                                </td>
                                <td style="text-align:right;">
                                0
                                </td>
                                <td style="text-align:right;">
                                0
                                </td>
                                <td style="text-align:right;">
                                0
                                </td>
                                <td style="text-align:right;">
                                1
                                </td>
                                <td style="text-align:right;">
                                1
                                </td>
                                <td style="text-align:right;">
                                1
                                </td>
                                </tr>
                                <tr>
                                <td style="text-align:left;">
                                1
                                </td>
                                <td style="text-align:left;">
                                Spain
                                </td>
                                <td style="text-align:left;">

                                1.  High income: OECD
                                    </td>
                                    <td style="text-align:right;">
                                    0
                                    </td>
                                    <td style="text-align:right;">
                                    0
                                    </td>
                                    <td style="text-align:right;">
                                    0
                                    </td>
                                    <td style="text-align:right;">
                                    3
                                    </td>
                                    <td style="text-align:right;">
                                    3
                                    </td>
                                    <td style="text-align:right;">
                                    2
                                    </td>
                                    </tr>
                                    <tr>
                                    <td style="text-align:left;">
                                    2
                                    </td>
                                    <td style="text-align:left;">
                                    New Zealand
                                    </td>
                                    <td style="text-align:left;">

                                    1.  High income: OECD
                                        </td>
                                        <td style="text-align:right;">
                                        5
                                        </td>
                                        <td style="text-align:right;">
                                        4
                                        </td>
                                        <td style="text-align:right;">
                                        3
                                        </td>
                                        <td style="text-align:right;">
                                        4
                                        </td>
                                        <td style="text-align:right;">
                                        4
                                        </td>
                                        <td style="text-align:right;">
                                        3
                                        </td>
                                        </tr>
                                        <tr>
                                        <td style="text-align:left;">
                                        2
                                        </td>
                                        <td style="text-align:left;">
                                        Thailand
                                        </td>
                                        <td style="text-align:left;">

                                        3.  Upper middle income
                                            </td>
                                            <td style="text-align:right;">
                                            2
                                            </td>
                                            <td style="text-align:right;">
                                            1
                                            </td>
                                            <td style="text-align:right;">
                                            1
                                            </td>
                                            <td style="text-align:right;">
                                            2
                                            </td>
                                            <td style="text-align:right;">
                                            1
                                            </td>
                                            <td style="text-align:right;">
                                            1
                                            </td>
                                            </tr>
                                            <tr>
                                            <td style="text-align:left;">
                                            2
                                            </td>
                                            <td style="text-align:left;">
                                            Australia
                                            </td>
                                            <td style="text-align:left;">

                                            1.  High income: OECD
                                                </td>
                                                <td style="text-align:right;">
                                                1
                                                </td>
                                                <td style="text-align:right;">
                                                1
                                                </td>
                                                <td style="text-align:right;">
                                                1
                                                </td>
                                                <td style="text-align:right;">
                                                4
                                                </td>
                                                <td style="text-align:right;">
                                                3
                                                </td>
                                                <td style="text-align:right;">
                                                2
                                                </td>
                                                </tr>
                                                <tr>
                                                <td style="text-align:left;">
                                                2
                                                </td>
                                                <td style="text-align:left;">
                                                Cambodia
                                                </td>
                                                <td style="text-align:left;">

                                                5.  Low income
                                                    </td>
                                                    <td style="text-align:right;">
                                                    1
                                                    </td>
                                                    <td style="text-align:right;">
                                                    1
                                                    </td>
                                                    <td style="text-align:right;">
                                                    1
                                                    </td>
                                                    <td style="text-align:right;">
                                                    1
                                                    </td>
                                                    <td style="text-align:right;">
                                                    1
                                                    </td>
                                                    <td style="text-align:right;">
                                                    1
                                                    </td>
                                                    </tr>
                                                    <tr>
                                                    <td style="text-align:left;">
                                                    2
                                                    </td>
                                                    <td style="text-align:left;">
                                                    Fiji
                                                    </td>
                                                    <td style="text-align:left;">

                                                    4.  Lower middle
                                                        income
                                                        </td>
                                                        <td style="text-align:right;">
                                                        1
                                                        </td>
                                                        <td style="text-align:right;">
                                                        1
                                                        </td>
                                                        <td style="text-align:right;">
                                                        1
                                                        </td>
                                                        <td style="text-align:right;">
                                                        1
                                                        </td>
                                                        <td style="text-align:right;">
                                                        1
                                                        </td>
                                                        <td style="text-align:right;">
                                                        1
                                                        </td>
                                                        </tr>
                                                        <tr>
                                                        <td style="text-align:left;">
                                                        2
                                                        </td>
                                                        <td style="text-align:left;">
                                                        Indonesia
                                                        </td>
                                                        <td style="text-align:left;">

                                                        4.  Lower middle
                                                            income
                                                            </td>
                                                            <td style="text-align:right;">
                                                            1
                                                            </td>
                                                            <td style="text-align:right;">
                                                            0
                                                            </td>
                                                            <td style="text-align:right;">
                                                            0
                                                            </td>
                                                            <td style="text-align:right;">
                                                            1
                                                            </td>
                                                            <td style="text-align:right;">
                                                            0
                                                            </td>
                                                            <td style="text-align:right;">
                                                            0
                                                            </td>
                                                            </tr>
                                                            <tr>
                                                            <td style="text-align:left;">
                                                            2
                                                            </td>
                                                            <td style="text-align:left;">
                                                            Taiwan
                                                            </td>
                                                            <td style="text-align:left;">

                                                            2.  High
                                                                income:
                                                                nonOECD
                                                                </td>
                                                                <td style="text-align:right;">
                                                                1
                                                                </td>
                                                                <td style="text-align:right;">
                                                                0
                                                                </td>
                                                                <td style="text-align:right;">
                                                                0
                                                                </td>
                                                                <td style="text-align:right;">
                                                                1
                                                                </td>
                                                                <td style="text-align:right;">
                                                                0
                                                                </td>
                                                                <td style="text-align:right;">
                                                                0
                                                                </td>
                                                                </tr>
                                                                <tr>
                                                                <td style="text-align:left;">
                                                                3
                                                                </td>
                                                                <td style="text-align:left;">
                                                                United
                                                                States
                                                                </td>
                                                                <td style="text-align:left;">

                                                                1.  High
                                                                    income:
                                                                    OECD
                                                                    </td>
                                                                    <td style="text-align:right;">
                                                                    6
                                                                    </td>
                                                                    <td style="text-align:right;">
                                                                    3
                                                                    </td>
                                                                    <td style="text-align:right;">
                                                                    3
                                                                    </td>
                                                                    <td style="text-align:right;">
                                                                    4
                                                                    </td>
                                                                    <td style="text-align:right;">
                                                                    2
                                                                    </td>
                                                                    <td style="text-align:right;">
                                                                    2
                                                                    </td>
                                                                    </tr>
                                                                    <tr>
                                                                    <td style="text-align:left;">
                                                                    3
                                                                    </td>
                                                                    <td style="text-align:left;">
                                                                    Canada
                                                                    </td>
                                                                    <td style="text-align:left;">

                                                                    1.  High
                                                                        income:
                                                                        OECD
                                                                        </td>
                                                                        <td style="text-align:right;">
                                                                        1
                                                                        </td>
                                                                        <td style="text-align:right;">
                                                                        1
                                                                        </td>
                                                                        <td style="text-align:right;">
                                                                        0
                                                                        </td>
                                                                        <td style="text-align:right;">
                                                                        3
                                                                        </td>
                                                                        <td style="text-align:right;">
                                                                        2
                                                                        </td>
                                                                        <td style="text-align:right;">
                                                                        0
                                                                        </td>
                                                                        </tr>
                                                                        <tr>
                                                                        <td style="text-align:left;">
                                                                        4
                                                                        </td>
                                                                        <td style="text-align:left;">
                                                                        Argentina
                                                                        </td>
                                                                        <td style="text-align:left;">

                                                                        3.  Upper
                                                                            middle
                                                                            income
                                                                            </td>
                                                                            <td style="text-align:right;">
                                                                            2
                                                                            </td>
                                                                            <td style="text-align:right;">
                                                                            1
                                                                            </td>
                                                                            <td style="text-align:right;">
                                                                            1
                                                                            </td>
                                                                            <td style="text-align:right;">
                                                                            2
                                                                            </td>
                                                                            <td style="text-align:right;">
                                                                            1
                                                                            </td>
                                                                            <td style="text-align:right;">
                                                                            1
                                                                            </td>
                                                                            </tr>
                                                                            <tr>
                                                                            <td style="text-align:left;">
                                                                            4
                                                                            </td>
                                                                            <td style="text-align:left;">
                                                                            Chile
                                                                            </td>
                                                                            <td style="text-align:left;">

                                                                            3.  Upper
                                                                                middle
                                                                                income
                                                                                </td>
                                                                                <td style="text-align:right;">
                                                                                1
                                                                                </td>
                                                                                <td style="text-align:right;">
                                                                                1
                                                                                </td>
                                                                                <td style="text-align:right;">
                                                                                1
                                                                                </td>
                                                                                <td style="text-align:right;">
                                                                                1
                                                                                </td>
                                                                                <td style="text-align:right;">
                                                                                1
                                                                                </td>
                                                                                <td style="text-align:right;">
                                                                                1
                                                                                </td>
                                                                                </tr>
                                                                                <tr>
                                                                                <td style="text-align:left;">
                                                                                4
                                                                                </td>
                                                                                <td style="text-align:left;">
                                                                                Colombia
                                                                                </td>
                                                                                <td style="text-align:left;">

                                                                                3.  Upper
                                                                                    middle
                                                                                    income
                                                                                    </td>
                                                                                    <td style="text-align:right;">
                                                                                    1
                                                                                    </td>
                                                                                    <td style="text-align:right;">
                                                                                    1
                                                                                    </td>
                                                                                    <td style="text-align:right;">
                                                                                    1
                                                                                    </td>
                                                                                    <td style="text-align:right;">
                                                                                    1
                                                                                    </td>
                                                                                    <td style="text-align:right;">
                                                                                    1
                                                                                    </td>
                                                                                    <td style="text-align:right;">
                                                                                    1
                                                                                    </td>
                                                                                    </tr>
                                                                                    <tr>
                                                                                    <td style="text-align:left;">
                                                                                    4
                                                                                    </td>
                                                                                    <td style="text-align:left;">
                                                                                    Ecuador
                                                                                    </td>
                                                                                    <td style="text-align:left;">

                                                                                    3.  Upper
                                                                                        middle
                                                                                        income
                                                                                        </td>
                                                                                        <td style="text-align:right;">
                                                                                        1
                                                                                        </td>
                                                                                        <td style="text-align:right;">
                                                                                        0
                                                                                        </td>
                                                                                        <td style="text-align:right;">
                                                                                        0
                                                                                        </td>
                                                                                        <td style="text-align:right;">
                                                                                        1
                                                                                        </td>
                                                                                        <td style="text-align:right;">
                                                                                        0
                                                                                        </td>
                                                                                        <td style="text-align:right;">
                                                                                        0
                                                                                        </td>
                                                                                        </tr>
                                                                                        <tr>
                                                                                        <td style="text-align:left;">
                                                                                        4
                                                                                        </td>
                                                                                        <td style="text-align:left;">
                                                                                        Mexico
                                                                                        </td>
                                                                                        <td style="text-align:left;">

                                                                                        3.  Upper
                                                                                            middle
                                                                                            income
                                                                                            </td>
                                                                                            <td style="text-align:right;">
                                                                                            1
                                                                                            </td>
                                                                                            <td style="text-align:right;">
                                                                                            0
                                                                                            </td>
                                                                                            <td style="text-align:right;">
                                                                                            0
                                                                                            </td>
                                                                                            <td style="text-align:right;">
                                                                                            1
                                                                                            </td>
                                                                                            <td style="text-align:right;">
                                                                                            0
                                                                                            </td>
                                                                                            <td style="text-align:right;">
                                                                                            0
                                                                                            </td>
                                                                                            </tr>
                                                                                            <tr>
                                                                                            <td style="text-align:left;">
                                                                                            4
                                                                                            </td>
                                                                                            <td style="text-align:left;">
                                                                                            Peru
                                                                                            </td>
                                                                                            <td style="text-align:left;">

                                                                                            3.  Upper
                                                                                                middle
                                                                                                income
                                                                                                </td>
                                                                                                <td style="text-align:right;">
                                                                                                1
                                                                                                </td>
                                                                                                <td style="text-align:right;">
                                                                                                0
                                                                                                </td>
                                                                                                <td style="text-align:right;">
                                                                                                0
                                                                                                </td>
                                                                                                <td style="text-align:right;">
                                                                                                1
                                                                                                </td>
                                                                                                <td style="text-align:right;">
                                                                                                0
                                                                                                </td>
                                                                                                <td style="text-align:right;">
                                                                                                0
                                                                                                </td>
                                                                                                </tr>
                                                                                                <tr>
                                                                                                <td style="text-align:left;">
                                                                                                4
                                                                                                </td>
                                                                                                <td style="text-align:left;">
                                                                                                Uruguay
                                                                                                </td>
                                                                                                <td style="text-align:left;">

                                                                                                3.  Upper
                                                                                                    middle
                                                                                                    income
                                                                                                    </td>
                                                                                                    <td style="text-align:right;">
                                                                                                    1
                                                                                                    </td>
                                                                                                    <td style="text-align:right;">
                                                                                                    1
                                                                                                    </td>
                                                                                                    <td style="text-align:right;">
                                                                                                    1
                                                                                                    </td>
                                                                                                    <td style="text-align:right;">
                                                                                                    1
                                                                                                    </td>
                                                                                                    <td style="text-align:right;">
                                                                                                    1
                                                                                                    </td>
                                                                                                    <td style="text-align:right;">
                                                                                                    1
                                                                                                    </td>
                                                                                                    </tr>
                                                                                                    <tr>
                                                                                                    <td style="text-align:left;">
                                                                                                    4
                                                                                                    </td>
                                                                                                    <td style="text-align:left;">
                                                                                                    Paraguay
                                                                                                    </td>
                                                                                                    <td style="text-align:left;">

                                                                                                    4.  Lower
                                                                                                        middle
                                                                                                        income
                                                                                                        </td>
                                                                                                        <td style="text-align:right;">
                                                                                                        0
                                                                                                        </td>
                                                                                                        <td style="text-align:right;">
                                                                                                        0
                                                                                                        </td>
                                                                                                        <td style="text-align:right;">
                                                                                                        0
                                                                                                        </td>
                                                                                                        <td style="text-align:right;">
                                                                                                        1
                                                                                                        </td>
                                                                                                        <td style="text-align:right;">
                                                                                                        1
                                                                                                        </td>
                                                                                                        <td style="text-align:right;">
                                                                                                        0
                                                                                                        </td>
                                                                                                        </tr>
                                                                                                        <tr>
                                                                                                        <td style="text-align:left;">
                                                                                                        5
                                                                                                        </td>
                                                                                                        <td style="text-align:left;">
                                                                                                        Mozambique
                                                                                                        </td>
                                                                                                        <td style="text-align:left;">

                                                                                                        5.  Low
                                                                                                            income
                                                                                                            </td>
                                                                                                            <td style="text-align:right;">
                                                                                                            2
                                                                                                            </td>
                                                                                                            <td style="text-align:right;">
                                                                                                            1
                                                                                                            </td>
                                                                                                            <td style="text-align:right;">
                                                                                                            1
                                                                                                            </td>
                                                                                                            <td style="text-align:right;">
                                                                                                            1
                                                                                                            </td>
                                                                                                            <td style="text-align:right;">
                                                                                                            1
                                                                                                            </td>
                                                                                                            <td style="text-align:right;">
                                                                                                            1
                                                                                                            </td>
                                                                                                            </tr>
                                                                                                            <tr>
                                                                                                            <td style="text-align:left;">
                                                                                                            5
                                                                                                            </td>
                                                                                                            <td style="text-align:left;">
                                                                                                            Cameroon
                                                                                                            </td>
                                                                                                            <td style="text-align:left;">

                                                                                                            4.  Lower
                                                                                                                middle
                                                                                                                income
                                                                                                                </td>
                                                                                                                <td style="text-align:right;">
                                                                                                                1
                                                                                                                </td>
                                                                                                                <td style="text-align:right;">
                                                                                                                1
                                                                                                                </td>
                                                                                                                <td style="text-align:right;">
                                                                                                                1
                                                                                                                </td>
                                                                                                                <td style="text-align:right;">
                                                                                                                1
                                                                                                                </td>
                                                                                                                <td style="text-align:right;">
                                                                                                                1
                                                                                                                </td>
                                                                                                                <td style="text-align:right;">
                                                                                                                1
                                                                                                                </td>
                                                                                                                </tr>
                                                                                                                <tr>
                                                                                                                <td style="text-align:left;">
                                                                                                                5
                                                                                                                </td>
                                                                                                                <td style="text-align:left;">
                                                                                                                South
                                                                                                                Africa
                                                                                                                </td>
                                                                                                                <td style="text-align:left;">

                                                                                                                3.  Upper
                                                                                                                    middle
                                                                                                                    income
                                                                                                                    </td>
                                                                                                                    <td style="text-align:right;">
                                                                                                                    1
                                                                                                                    </td>
                                                                                                                    <td style="text-align:right;">
                                                                                                                    0
                                                                                                                    </td>
                                                                                                                    <td style="text-align:right;">
                                                                                                                    0
                                                                                                                    </td>
                                                                                                                    <td style="text-align:right;">
                                                                                                                    3
                                                                                                                    </td>
                                                                                                                    <td style="text-align:right;">
                                                                                                                    1
                                                                                                                    </td>
                                                                                                                    <td style="text-align:right;">
                                                                                                                    1
                                                                                                                    </td>
                                                                                                                    </tr>
                                                                                                                    <tr>
                                                                                                                    <td style="text-align:left;">
                                                                                                                    5
                                                                                                                    </td>
                                                                                                                    <td style="text-align:left;">
                                                                                                                    Namibia
                                                                                                                    </td>
                                                                                                                    <td style="text-align:left;">

                                                                                                                    3.  Upper
                                                                                                                        middle
                                                                                                                        income
                                                                                                                        </td>
                                                                                                                        <td style="text-align:right;">
                                                                                                                        0
                                                                                                                        </td>
                                                                                                                        <td style="text-align:right;">
                                                                                                                        0
                                                                                                                        </td>
                                                                                                                        <td style="text-align:right;">
                                                                                                                        0
                                                                                                                        </td>
                                                                                                                        <td style="text-align:right;">
                                                                                                                        1
                                                                                                                        </td>
                                                                                                                        <td style="text-align:right;">
                                                                                                                        0
                                                                                                                        </td>
                                                                                                                        <td style="text-align:right;">
                                                                                                                        0
                                                                                                                        </td>
                                                                                                                        </tr>
                                                                                                                        <tr>
                                                                                                                        <td style="text-align:left;">
                                                                                                                        6
                                                                                                                        </td>
                                                                                                                        <td style="text-align:left;">
                                                                                                                        Bangladesh
                                                                                                                        </td>
                                                                                                                        <td style="text-align:left;">

                                                                                                                        5.  Low
                                                                                                                            income
                                                                                                                            </td>
                                                                                                                            <td style="text-align:right;">
                                                                                                                            1
                                                                                                                            </td>
                                                                                                                            <td style="text-align:right;">
                                                                                                                            0
                                                                                                                            </td>
                                                                                                                            <td style="text-align:right;">
                                                                                                                            0
                                                                                                                            </td>
                                                                                                                            <td style="text-align:right;">
                                                                                                                            1
                                                                                                                            </td>
                                                                                                                            <td style="text-align:right;">
                                                                                                                            0
                                                                                                                            </td>
                                                                                                                            <td style="text-align:right;">
                                                                                                                            0
                                                                                                                            </td>
                                                                                                                            </tr>
                                                                                                                            <tr>
                                                                                                                            <td style="text-align:left;">
                                                                                                                            6
                                                                                                                            </td>
                                                                                                                            <td style="text-align:left;">
                                                                                                                            India
                                                                                                                            </td>
                                                                                                                            <td style="text-align:left;">

                                                                                                                            4.  Lower
                                                                                                                                middle
                                                                                                                                income
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                1
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                1
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                1
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                1
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                1
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                1
                                                                                                                                </td>
                                                                                                                                </tr>
                                                                                                                                <tr>
                                                                                                                                <td style="text-align:left;">
                                                                                                                                No
                                                                                                                                answer
                                                                                                                                </td>
                                                                                                                                <td style="text-align:left;">
                                                                                                                                No
                                                                                                                                answer
                                                                                                                                </td>
                                                                                                                                <td style="text-align:left;">
                                                                                                                                No
                                                                                                                                answer
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                2
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                2
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                3
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                1
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                1
                                                                                                                                </td>
                                                                                                                                <td style="text-align:right;">
                                                                                                                                2
                                                                                                                                </td>
                                                                                                                                </tr>
                                                                                                                                </tbody>
                                                                                                                                </table>

## Making maps of country information

``` r
library(viridis)
new_pal <- c("#0D0887FF", "#42049EFF", "#6A00A8FF", "#900DA4FF", "#B12A90FF", "#CC4678FF", "#E16462FF", "#F1844BFF", "#FCA636FF", "#FCCE25FF")
plot_countries_from <- ggplot() +
  geom_sf(data = world_r1,
          aes(fill = from_r1,
              colour = from_r1)
          ) +
  scale_fill_gradientn(name = "", na.value="snow2", trans = "log", colours = new_pal, breaks= breaks_pretty()) +
  scale_colour_gradientn(name = "", na.value="snow3", trans = "log", colours = new_pal, breaks= breaks_pretty()) +
  labs(title="Number of respondents from each country - Round 1") +
  theme_minimal()

plot_countries_from
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Demographic-maps_files/figure-gfm/making%20demographic%20maps-1.png)<!-- -->

``` r
plot_countries_worked <- ggplot() +
  geom_sf(data = world_r1,
          aes(fill = worked_r1,
              colour = worked_r1)
          ) +
  scale_fill_gradientn(name = "", na.value="snow2", trans = "log", colours = new_pal, breaks= pretty_breaks()) +
  scale_colour_gradientn(name = "", na.value="snow3", trans = "log", colours = new_pal, breaks= pretty_breaks())  +
  labs(title="Number of respondents working in each country") +
  theme_minimal()

plot_countries_worked
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Demographic-maps_files/figure-gfm/making%20demographic%20maps-2.png)<!-- -->

``` r
plot_both <- plot_grid(plot_countries_from, plot_countries_worked, ncol = 1)
plot_both
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Demographic-maps_files/figure-gfm/making%20demographic%20maps-3.png)<!-- -->

``` r
#save_plot(here("survey_demographics_map.pdf"), plot_both, base_width = 183, base_height = 200, units = "mm")
#save_plot(here("survey_demographics_R1_country_from.pdf"), plot_countries_from, base_width = 183, base_height = 100, units = "mm")
#save_plot(here("survey_demographics_R1_country_from.png"), plot_countries_from, base_width = 183, base_height = 100, units = "mm")
```

## Table of sector that people (predominantly) work in

For round 1, respondents could list as many sectors as they like. For
rounds 2 and 3 they were asked to select the sector that best represents
their career. First step, need to extract the information from round 1,
similar to how the counts for each ocean were extracted.

``` r
sector_r1 <- country_information %>% 
  mutate("Industry (commercial fishing)" = str_count(country_information$sector, "Industry \\(commercial fishing\\)"),
         "Civil society (environmental NGO)" = str_count(country_information$sector, "Civil society \\(environmental NGO\\)"),
         "Intergovernmental body" = str_count(country_information$sector, "Intergovernmental body"),
         "Civil society (other NGO)" = str_count(country_information$sector, "Civil society \\(other NGO\\)"),
         "Academia" = str_count(country_information$sector, "Academia"),
         "Government (environment)" = str_count(country_information$sector, "Government \\(environment\\)"),
          "Industry (other)" = str_count(country_information$sector, "Industry \\(other\\)"),
          "Government (fisheries management)" = str_count(country_information$sector, "Government \\(fisheries management\\)"),
         "Civil society (Small-scale fisheries/Rights holder institution)" = str_count(country_information$sector, "Civil society \\(Small-scale fisheries/Rights holder institution\\)")
        ) %>% 
  select("Industry (commercial fishing)", "Industry (other)",  "Government (environment)", "Government (fisheries management)",  "Intergovernmental body", "Academia", "Civil society (environmental NGO)", "Civil society (Small-scale fisheries/Rights holder institution)", "Civil society (other NGO)") %>%
  pivot_longer(cols = everything(), names_to = "sector", values_to = "num_responses") %>% 
  group_by(sector) %>% 
  summarise(num_responses_r1 = sum(num_responses))
```

Now for rounds 2 and 3

``` r
sector_r2 <- country_information_r2 %>% 
  dplyr::group_by(sector) %>% 
  dplyr::summarise(num_responses_r2 = n())

sector_r3 <- country_information_r3 %>% 
  dplyr::group_by(sector) %>% 
  dplyr::summarise(num_responses_r3 = n())
```

combine them all

``` r
sector_all <- sector_r1 %>% 
  full_join(sector_r2) %>% 
  full_join(sector_r3) %>% 
  arrange(desc(num_responses_r1)) %>% 
  replace(is.na(.), 0)

sector_all %>% 
  kable(booktabs = TRUE) %>%
  kable_styling(font_size = 10)
```

<table class="table" style="font-size: 10px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
sector
</th>
<th style="text-align:right;">
num_responses_r1
</th>
<th style="text-align:right;">
num_responses_r2
</th>
<th style="text-align:right;">
num_responses_r3
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Civil society (environmental NGO)
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
Academia
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Government (fisheries management)
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Industry (commercial fishing)
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Civil society (other NGO)
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Government (environment)
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Industry (other)
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Intergovernmental body
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Civil society (Small-scale fisheries/Rights holder institution)
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

## Map of oceans with respondents expertise

``` r
ocean_expertise <- country_information %>% 
  mutate(atlantic_ocean_north = str_count(country_information$oceans_worked, "Atlantic Ocean \\(North\\)"),
         atlantic_ocean_south = str_count(country_information$oceans_worked, "Atlantic Ocean \\(South\\)"),
         pacific_ocean_north = str_count(country_information$oceans_worked, "Pacific Ocean \\(North\\)"),
         pacific_ocean_south = str_count(country_information$oceans_worked, "Pacific Ocean \\(South\\)"),
         indian_ocean = str_count(country_information$oceans_worked, "Indian Ocean"),
         southern_ocean = str_count(country_information$oceans_worked, "Southern Ocean")
        ) %>% 
  select(atlantic_ocean_north, atlantic_ocean_south, pacific_ocean_north, pacific_ocean_south, indian_ocean, southern_ocean) %>%
  pivot_longer(cols = everything(), names_to = "ocean", values_to = "num_responses") %>% 
  group_by(ocean) %>% 
  summarise(num_responses = sum(num_responses))
#create coordinate information for each ocean
ocean_expertise <- ocean_expertise %>% 
  mutate(x = c(-45, -10, 80, -140, -140, 0),
         y = c(35, -20, -15, 35, -35, -60)
  ) %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 4326) %>% 
  st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
```

Now that I have the data in the format I need, now I need to plot it on
a map.

``` r
plot_oceans <- ggplot() +
  geom_sf(data = world_r1,
         fill = "snow2",
         colour = "snow2") +
  geom_sf(data = ocean_expertise,
          colour = "skyblue", 
          #size = 10,
          aes(
            size = num_responses
            )) +
  geom_sf_text(data = ocean_expertise,
               aes(label = num_responses)) +
  labs(title="Number of respondents with expertise in each ocean") +
  scale_size(range = c(3, 15), name = "") +
  xlab(label = NULL) +
  ylab(label = NULL) +
  theme_minimal()
plot_oceans
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Demographic-maps_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
save_plot(here(here::here("figures", "Q2-6ocean_expertise_map.png")), plot_oceans, base_width = 183, base_height = 100, units = "mm")
```

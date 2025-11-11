Depression_LinReg
================
Toheeb
2024-11-28

``` r
library(haven) #contains the read_dta function
```

    ## Warning: package 'haven' was built under R version 4.3.2

``` r
library(tidyverse) #contains many of the functions to be used in regression modeling, and dplyr
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.3.2

    ## Warning: package 'tidyr' was built under R version 4.2.3

    ## Warning: package 'readr' was built under R version 4.2.3

    ## Warning: package 'purrr' was built under R version 4.3.2

    ## Warning: package 'dplyr' was built under R version 4.3.2

    ## Warning: package 'stringr' was built under R version 4.3.2

    ## Warning: package 'forcats' was built under R version 4.3.2

    ## Warning: package 'lubridate' was built under R version 4.3.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rsample) 
```

    ## Warning: package 'rsample' was built under R version 4.2.3

``` r
library(naniar)
```

    ## Warning: package 'naniar' was built under R version 4.3.2

``` r
library(skimr)
```

    ## Warning: package 'skimr' was built under R version 4.3.2

    ## 
    ## Attaching package: 'skimr'
    ## 
    ## The following object is masked from 'package:naniar':
    ## 
    ##     n_complete

``` r
library(PerformanceAnalytics)
```

    ## Warning: package 'PerformanceAnalytics' was built under R version 4.3.2

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 4.2.3

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.3.2

    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric
    ## 
    ## 
    ## ######################### Warning from 'xts' package ##########################
    ## #                                                                             #
    ## # The dplyr lag() function breaks how base R's lag() function is supposed to  #
    ## # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
    ## # source() into this session won't work correctly.                            #
    ## #                                                                             #
    ## # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
    ## # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
    ## # dplyr from breaking base R's lag() function.                                #
    ## #                                                                             #
    ## # Code in packages is not affected. It's protected by R's namespace mechanism #
    ## # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
    ## #                                                                             #
    ## ###############################################################################
    ## 
    ## Attaching package: 'xts'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last
    ## 
    ## 
    ## Attaching package: 'PerformanceAnalytics'
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

``` r
library(recipes)
```

    ## Warning: package 'recipes' was built under R version 4.2.3

    ## 
    ## Attaching package: 'recipes'
    ## 
    ## The following object is masked from 'package:stringr':
    ## 
    ##     fixed
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(parsnip)
```

    ## Warning: package 'parsnip' was built under R version 4.2.3

``` r
library(workflows)
```

    ## Warning: package 'workflows' was built under R version 4.2.3

``` r
library(yardstick)
```

    ## Warning: package 'yardstick' was built under R version 4.2.3

    ## 
    ## Attaching package: 'yardstick'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     spec

``` r
library(ggplot2)
library(tune)
```

    ## Warning: package 'tune' was built under R version 4.2.3

``` r
library(broom)
```

    ## Warning: package 'broom' was built under R version 4.3.2

``` r
library(MASS)
```

    ## Warning: package 'MASS' was built under R version 4.2.3

    ## 
    ## Attaching package: 'MASS'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

## Exploratory Data Analysis

### Depression data

``` r
dep <- read_dta("C:/Users/tohaj/Desktop/DS-Projects/Linear Regression_Depression/data files/depression.dta")
```

``` r
dim(dep)
```

    ## [1] 8965   11

``` r
tail(dep,100)
```

    ## # A tibble: 100 × 11
    ##      seqn dpq010 dpq020 dpq030 dpq040 dpq050 dpq060 dpq070 dpq080 dpq090 dpq100
    ##     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ##  1 124663      1      1      0      1      0      0      0      0      0      0
    ##  2 124665      0      0      0      0      0      0      0      0      0     NA
    ##  3 124667      0      0      0      0      0      0      0      0      0     NA
    ##  4 124668      0      0      0      0      0      0      0      0      0     NA
    ##  5 124670      0      0      0      0      0      0      0      0      0     NA
    ##  6 124671      0      0      1      0      0      0      0      0      0      0
    ##  7 124676      0      0      0      1      0      0      0      0      0      0
    ##  8 124683      0      0      0      0      0      0      0      0      0     NA
    ##  9 124684      1      0      1      1      0      0      0      0      0      1
    ## 10 124685      1      1      3      1      1      1      1      0      0      2
    ## # ℹ 90 more rows

``` r
# check the unique values in each column of the data
unique_value <- function(data){
  for (i in data) {
    print(unique(i))
  }
}

# unique_value(dep)
```

``` r
#recode 7's and 9's as missing
dep[dep==7 | dep==9] <- NA
```

``` r
#recheck the unique values
# unique_value(dep)
```

``` r
#create a new column for depression score
dep$dep_score <- rowSums(dep[, c("dpq010","dpq020","dpq030","dpq040","dpq050",
                                 "dpq060","dpq070","dpq080","dpq090")], na.rm = TRUE)
```

``` r
#retain only ID and depression score columns
depression <- dep[,c(1,12)]
head(depression)
```

    ## # A tibble: 6 × 2
    ##     seqn dep_score
    ##    <dbl>     <dbl>
    ## 1 109266         0
    ## 2 109271         5
    ## 3 109273        15
    ## 4 109274         0
    ## 5 109282         5
    ## 6 109284         0

### Demographics data

``` r
dem <- read_dta("C:/Users/tohaj/Desktop/DS-Projects/Linear Regression_Depression/data files/demographics.dta")
head(dem)
```

    ## # A tibble: 6 × 29
    ##     seqn sddsrvyr ridstatr riagendr ridageyr ridagemn ridreth1 ridreth3 ridexmon
    ##    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 109263       66        2        1        2       NA        5        6        2
    ## 2 109264       66        2        2       13       NA        1        1        2
    ## 3 109265       66        2        1        2       NA        3        3        2
    ## 4 109266       66        2        2       29       NA        5        6        2
    ## 5 109267       66        1        2       21       NA        2        2       NA
    ## 6 109268       66        1        2       18       NA        3        3       NA
    ## # ℹ 20 more variables: dmdborn4 <dbl>, dmdyrusz <dbl>, dmdeduc2 <dbl>,
    ## #   dmdmartz <dbl>, ridexprg <dbl>, sialang <dbl>, siaproxy <dbl>,
    ## #   siaintrp <dbl>, fialang <dbl>, fiaproxy <dbl>, fiaintrp <dbl>,
    ## #   mialang <dbl>, miaproxy <dbl>, miaintrp <dbl>, aialanga <dbl>,
    ## #   wtintprp <dbl>, wtmecprp <dbl>, sdmvpsu <dbl>, sdmvstra <dbl>,
    ## #   indfmpir <dbl>

``` r
dim(dem)
```

    ## [1] 15560    29

``` r
demographics <- dem[, c("seqn", "riagendr", "ridageyr", "ridreth3", 
                        "dmdeduc2", "dmdmartz")]
head(demographics)
```

    ## # A tibble: 6 × 6
    ##     seqn riagendr ridageyr ridreth3 dmdeduc2 dmdmartz
    ##    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 109263        1        2        6       NA       NA
    ## 2 109264        2       13        1       NA       NA
    ## 3 109265        1        2        3       NA       NA
    ## 4 109266        2       29        6        5        3
    ## 5 109267        2       21        2        4        3
    ## 6 109268        2       18        3       NA       NA

Demographics code book:  
riagendr (Sex): 1=male, 2=female  
riadageyr (Age): 0-79; 80=80yrs and older ridreth3 (race/ethnicity):
1=Mex. American, 2=Other Hispanic, 3=NH White, 4=NH Black, 6=NH Asian,
7= Other race  
dmdeduc2 (education level): 1=less than 9th grade, 2=9-11th grade, 3=HS
Graduate/GED equiv, 4=some college or AA degree, 5=College graduate or
above, 7=Refused, 9=Don’t know  
dmdmartz (marital status): 1=married/living with partner,
2=widowed/divorced/separated, 3=never married, 77=Refused, 99=Don’t know

``` r
#rename columns
demo <- demographics %>%
  rename(
    sex = riagendr,
    age = ridageyr,
    race_eth = ridreth3,
    educ = dmdeduc2,
    marital = dmdmartz
  )

# unique_value(demo)
```

``` r
head(demo)
```

    ## # A tibble: 6 × 6
    ##     seqn   sex   age race_eth  educ marital
    ##    <dbl> <dbl> <dbl>    <dbl> <dbl>   <dbl>
    ## 1 109263     1     2        6    NA      NA
    ## 2 109264     2    13        1    NA      NA
    ## 3 109265     1     2        3    NA      NA
    ## 4 109266     2    29        6     5       3
    ## 5 109267     2    21        2     4       3
    ## 6 109268     2    18        3    NA      NA

``` r
#recode "refused" and "unknown" as missing values
demo_recode <- demo %>%
  mutate(
    educ = na_if(educ,7),
    educ = na_if(educ,9),
    marital = na_if(marital,77),
    marital = na_if(marital,99)
  )

# check that the recoding worked
unique(demo_recode["educ"])
```

    ## # A tibble: 6 × 1
    ##    educ
    ##   <dbl>
    ## 1    NA
    ## 2     5
    ## 3     4
    ## 4     2
    ## 5     3
    ## 6     1

``` r
unique(demo_recode["marital"])
```

    ## # A tibble: 4 × 1
    ##   marital
    ##     <dbl>
    ## 1      NA
    ## 2       3
    ## 3       1
    ## 4       2

``` r
#recode values in other columns
demo_recode <- demo_recode %>%
  mutate(
    race_eth = case_when(
      race_eth == 2 ~ 1, # Hispanic
      race_eth == 3 ~ 2, # NH White
      race_eth == 4 ~ 3, # NH Black
      race_eth == 6 ~ 4, # NH Asian
      race_eth == 7 ~ 5, # Others
      TRUE ~ race_eth # retains other values as is 
    )
  )

# check
unique(demo_recode$race_eth)
```

    ## [1] 4 1 2 3 5

``` r
#check data type of each column
sapply(demo_recode, class) 
```

    ##      seqn       sex       age  race_eth      educ   marital 
    ## "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"

``` r
#can also use "str(df)" for the same task
```

``` r
#convert categorical variables to factor type, and label accordingly
demogr_final <- demo_recode %>%
  mutate(
    sex = factor(sex,
                 levels=c(1,2),
                 labels=c("M", "F")), 
  #NB: the first level in the list will be the reference level when one_hot argument is set to False (later in the step_dummy function)
    race_eth = factor(race_eth,
                      levels = c(2, 1, 3, 4, 5),
                      labels=c("NHWhite", "Hispanic", "NHBlack", "NHAsian", "Others")),
    educ = factor(educ,
                  levels=c(3, 1, 2, 4, 5),
                  labels=c("HS/GED", "<9th grade", "9-11th grade", "some college", ">= college grad")),
    marital = factor(marital,
                     levels=c(1,2,3),
                     labels=c("mard./liv. w/part", "wid./div./sep.", "nev. mard."))
  )

#Check:
sapply(demogr_final, class) 
```

    ##      seqn       sex       age  race_eth      educ   marital 
    ## "numeric"  "factor" "numeric"  "factor"  "factor"  "factor"

``` r
head(demogr_final)
```

    ## # A tibble: 6 × 6
    ##     seqn sex     age race_eth educ            marital   
    ##    <dbl> <fct> <dbl> <fct>    <fct>           <fct>     
    ## 1 109263 M         2 NHAsian  <NA>            <NA>      
    ## 2 109264 F        13 Hispanic <NA>            <NA>      
    ## 3 109265 M         2 NHWhite  <NA>            <NA>      
    ## 4 109266 F        29 NHAsian  >= college grad nev. mard.
    ## 5 109267 F        21 Hispanic some college    nev. mard.
    ## 6 109268 F        18 NHWhite  <NA>            <NA>

### Physical activity data

``` r
phys_act <- read_dta("C:/Users/tohaj/Desktop/DS-Projects/Linear Regression_Depression/data files/physical-activity.dta")
```

``` r
head(phys_act)
```

    ## # A tibble: 6 × 17
    ##     seqn paq605 paq610 pad615 paq620 paq625 pad630 paq635 paq640 pad645 paq650
    ##    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 109266      2     NA     NA      2     NA     NA      2     NA     NA      1
    ## 2 109267      2     NA     NA      2     NA     NA      2     NA     NA      1
    ## 3 109268      1      5    540      1      5    300      2     NA     NA      2
    ## 4 109271      2     NA     NA      1      2    120      2     NA     NA      2
    ## 5 109273      1      3    240      2     NA     NA      2     NA     NA      2
    ## 6 109274      1      7    480      1      6    360      1      7     60      2
    ## # ℹ 6 more variables: paq655 <dbl>, pad660 <dbl>, paq665 <dbl>, paq670 <dbl>,
    ## #   pad675 <dbl>, pad680 <dbl>

paq665 (engaged in any moderate-intensity sports, fitness, or
recreational activities that cause a small increase in breathing or
heart rate such as brisk walking, bicycling, swimming, or volleyball for
at least 10 minutes continuously?): 1=yes, 2=no

``` r
#subset only the needed columns
physAct <- phys_act[, c("seqn", "paq665")]
head(physAct)
```

    ## # A tibble: 6 × 2
    ##     seqn paq665
    ##    <dbl>  <dbl>
    ## 1 109266      1
    ## 2 109267      2
    ## 3 109268      2
    ## 4 109271      2
    ## 5 109273      1
    ## 6 109274      1

``` r
#rename variable
physAct <- physAct %>%
  rename(physact = paq665)

sapply(physAct, class)
```

    ##      seqn   physact 
    ## "numeric" "numeric"

``` r
#recode values as needed
physAct <- physAct %>%
  mutate(
    physact = case_when(
      physact == 7~NA,
      physact == 9 ~ NA,
      TRUE ~ physact),
    physact = factor(physact,
                     levels=c(1,2),
                     labels=c("Active", "Inactive"))
  )

unique(physAct$physact)
```

    ## [1] Active   Inactive <NA>    
    ## Levels: Active Inactive

``` r
head(physAct)
```

    ## # A tibble: 6 × 2
    ##     seqn physact 
    ##    <dbl> <fct>   
    ## 1 109266 Active  
    ## 2 109267 Inactive
    ## 3 109268 Inactive
    ## 4 109271 Inactive
    ## 5 109273 Active  
    ## 6 109274 Active

### Alcohol use data

``` r
drink <- read_dta("C:/Users/tohaj/Desktop/DS-Projects/Linear Regression_Depression/data files/alcohol-use.dta")

head(drink)
```

    ## # A tibble: 6 × 10
    ##     seqn alq111 alq121 alq130 alq142 alq270 alq280 alq290 alq151 alq170
    ##    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 109266      1     10      1      0     NA     NA     NA      2      0
    ## 2 109271      1      0     NA     NA     NA     NA     NA      1     NA
    ## 3 109273      1      0     NA     NA     NA     NA     NA      2     NA
    ## 4 109274      1      4      2      5      7      0     NA      2      0
    ## 5 109282      1      0     NA     NA     NA     NA     NA      2     NA
    ## 6 109284      2     NA     NA     NA     NA     NA     NA     NA     NA

alq130: Average \# alcoholic drinks/day - past 12 mos

``` r
#obtain only the needed columns
drinking <- drink[, c("seqn", "alq130")]
names(drinking)[names(drinking) == "alq130"] <- "alc_drinks"
head(drinking)
```

    ## # A tibble: 6 × 2
    ##     seqn alc_drinks
    ##    <dbl>      <dbl>
    ## 1 109266          1
    ## 2 109271         NA
    ## 3 109273         NA
    ## 4 109274          2
    ## 5 109282         NA
    ## 6 109284         NA

``` r
#recode values
drinking[drinking == 777 | drinking == 999] <- NA
```

### Cigarette use

``` r
smoking <- read_dta("C:/Users/tohaj/Desktop/DS-Projects/Linear Regression_Depression/data files/cigarette-use.dta")

head(smoking)
```

    ## # A tibble: 6 × 16
    ##     seqn smq020 smd030 smq040 smq050q smq050u smd057 smq078 smd641 smd650
    ##    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 109264     NA     NA     NA      NA      NA     NA     NA     NA     NA
    ## 2 109266      2     NA     NA      NA      NA     NA     NA     NA     NA
    ## 3 109267      2     NA     NA      NA      NA     NA     NA     NA     NA
    ## 4 109268      2     NA     NA      NA      NA     NA     NA     NA     NA
    ## 5 109271      1     18      1      NA      NA     NA      2     30     20
    ## 6 109273      1     18      1      NA      NA     NA      1     30     15
    ## # ℹ 6 more variables: smd100fl <dbl>, smd100mn <dbl>, smq670 <dbl>,
    ## #   smq621 <dbl>, smd630 <dbl>, smaquex2 <dbl>

``` r
#obtain needed columns
cig_use <- smoking[, c("seqn", "smq020")]

#rename column
names(cig_use)[names(cig_use) == "smq020"] <- "cig100"
```

smq020: smoked at least 100 cigarettes in life

``` r
#recode values as needed
cig_use <- cig_use %>%
  mutate(
    cig100 = case_when(
      cig100 == 7~NA,
      cig100 == 9 ~ NA,
      TRUE ~ cig100),
    cig100 = factor(cig100,
                     levels=c(1,2),
                     labels=c("Yes", "No"))
  )

unique(cig_use$cig100)
```

    ## [1] <NA> No   Yes 
    ## Levels: Yes No

``` r
head(cig_use)
```

    ## # A tibble: 6 × 2
    ##     seqn cig100
    ##    <dbl> <fct> 
    ## 1 109264 <NA>  
    ## 2 109266 No    
    ## 3 109267 No    
    ## 4 109268 No    
    ## 5 109271 Yes   
    ## 6 109273 Yes

### BMI

``` r
bmi <- read_xpt("C:/Users/tohaj/Desktop/DS-Projects/Linear Regression_Depression/data files/p_bmx.xpt")

head(bmi)
```

    ## # A tibble: 6 × 22
    ##     SEQN BMDSTATS BMXWT BMIWT BMXRECUM BMIRECUM BMXHEAD BMIHEAD BMXHT BMIHT
    ##    <dbl>    <dbl> <dbl> <dbl>    <dbl>    <dbl>   <dbl>   <dbl> <dbl> <dbl>
    ## 1 109263        4  NA      NA     NA         NA      NA      NA  NA      NA
    ## 2 109264        1  42.2    NA     NA         NA      NA      NA 155.     NA
    ## 3 109265        1  12      NA     91.6       NA      NA      NA  89.3    NA
    ## 4 109266        1  97.1    NA     NA         NA      NA      NA 160.     NA
    ## 5 109269        3  13.6    NA     90.9       NA      NA      NA  NA       1
    ## 6 109270        1  75.3    NA     NA         NA      NA      NA 156      NA
    ## # ℹ 12 more variables: BMXBMI <dbl>, BMDBMIC <dbl>, BMXLEG <dbl>, BMILEG <dbl>,
    ## #   BMXARML <dbl>, BMIARML <dbl>, BMXARMC <dbl>, BMIARMC <dbl>, BMXWAIST <dbl>,
    ## #   BMIWAIST <dbl>, BMXHIP <dbl>, BMIHIP <dbl>

bmxbmi: bmi in kg/m2

``` r
#obtain the columns of interest
bmi_data <- bmi[, c("SEQN","BMXBMI")]

#rename column
names(bmi_data)[names(bmi_data) == "SEQN"] <- "seqn"
names(bmi_data)[names(bmi_data) == "BMXBMI"] <- "bmi"

head(bmi_data)
```

    ## # A tibble: 6 × 2
    ##     seqn   bmi
    ##    <dbl> <dbl>
    ## 1 109263  NA  
    ## 2 109264  17.6
    ## 3 109265  15  
    ## 4 109266  37.8
    ## 5 109269  NA  
    ## 6 109270  30.9

### Sleep

``` r
sleep <- read_dta("C:/Users/tohaj/Desktop/DS-Projects/Linear Regression_Depression/data files/sleep-disorder.dta")

head(sleep)
```

    ## # A tibble: 6 × 11
    ##     seqn slq300 slq310 sld012 slq320 slq330 sld013 slq030 slq040 slq050 slq120
    ##    <dbl> <chr>  <chr>   <dbl> <chr>  <chr>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 109266 22:00  05:30     7.5 23:00  07:00     8        1      0      2      0
    ## 2 109267 00:00  08:00     8   03:00  11:00     8        0      0      2      2
    ## 3 109268 22:00  06:30     8.5 23:00  07:00     8        0      0      2      1
    ## 4 109271 23:00  09:00    10   23:00  12:00    13        0      0      1      3
    ## 5 109273 08:00  14:35     6.5 21:00  05:00     8        0      0      1      2
    ## 6 109274 21:30  07:00     9.5 21:30  07:00     9.5      1      0      2      0

sld012 (Number of hours usually sleep on weekdays or workdays):
3-13.5=range of values, 2=less than 3 hrs, 14=13hours or more.

``` r
#obtain only the needed columns
sleep_data <- sleep[, c("seqn","sld012")]

#rename column
names(sleep_data)[names(sleep_data) == "sld012"] <- "sleep_hrs"

head(sleep_data)
```

    ## # A tibble: 6 × 2
    ##     seqn sleep_hrs
    ##    <dbl>     <dbl>
    ## 1 109266       7.5
    ## 2 109267       8  
    ## 3 109268       8.5
    ## 4 109271      10  
    ## 5 109273       6.5
    ## 6 109274       9.5

### Merge data

``` r
data_list <- list(depression,demogr_final,physAct,
                                   drinking,cig_use,bmi_data,sleep_data)
nhanes_2017_Mar2020 <- reduce(data_list, full_join, by="seqn")

head(nhanes_2017_Mar2020)
```

    ## # A tibble: 6 × 12
    ##     seqn dep_score sex     age race_eth educ   marital physact alc_drinks cig100
    ##    <dbl>     <dbl> <fct> <dbl> <fct>    <fct>  <fct>   <fct>        <dbl> <fct> 
    ## 1 109266         0 F        29 NHAsian  >= co… nev. m… Active           1 No    
    ## 2 109271         5 M        49 NHWhite  9-11t… nev. m… Inacti…         NA Yes   
    ## 3 109273        15 M        36 NHWhite  some … nev. m… Active          NA Yes   
    ## 4 109274         0 M        68 Others   some … nev. m… Active           2 No    
    ## 5 109282         5 M        76 NHWhite  >= co… mard./… Inacti…         NA Yes   
    ## 6 109284         0 F        44 Hispanic 9-11t… mard./… Inacti…         NA No    
    ## # ℹ 2 more variables: bmi <dbl>, sleep_hrs <dbl>

### Check for missing values and patterns

#### Missing values

``` r
#get the data shape
dim(nhanes_2017_Mar2020)
```

    ## [1] 15560    12

``` r
glimpse(nhanes_2017_Mar2020)
```

    ## Rows: 15,560
    ## Columns: 12
    ## $ seqn       <dbl> 109266, 109271, 109273, 109274, 109282, 109284, 109286, 109…
    ## $ dep_score  <dbl> 0, 5, 15, 0, 5, 0, 0, 2, 8, 8, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0…
    ## $ sex        <fct> F, M, M, M, M, F, F, F, F, M, M, F, F, M, F, M, M, M, F, M,…
    ## $ age        <dbl> 29, 49, 36, 68, 76, 44, 33, 68, 42, 58, 44, 54, 30, 68, 54,…
    ## $ race_eth   <fct> NHAsian, NHWhite, NHWhite, Others, NHWhite, Hispanic, NHAsi…
    ## $ educ       <fct> >= college grad, 9-11th grade, some college, some college, …
    ## $ marital    <fct> nev. mard., nev. mard., nev. mard., nev. mard., mard./liv. …
    ## $ physact    <fct> Active, Inactive, Active, Active, Inactive, Inactive, Inact…
    ## $ alc_drinks <dbl> 1, NA, NA, 2, NA, NA, NA, NA, NA, 6, NA, NA, 2, NA, NA, NA,…
    ## $ cig100     <fct> No, Yes, Yes, No, Yes, No, No, No, No, No, No, No, No, No, …
    ## $ bmi        <dbl> 37.8, 29.7, 21.9, 30.2, 26.6, 39.1, 28.9, 28.1, 31.3, 30.5,…
    ## $ sleep_hrs  <dbl> 7.5, 10.0, 6.5, 9.5, 7.0, 8.0, 8.5, 4.0, 11.0, 4.5, 7.5, 7.…

``` r
skim(nhanes_2017_Mar2020)
```

|                                                  |                     |
|:-------------------------------------------------|:--------------------|
| Name                                             | nhanes_2017_Mar2020 |
| Number of rows                                   | 15560               |
| Number of columns                                | 12                  |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                     |
| Column type frequency:                           |                     |
| factor                                           | 6                   |
| numeric                                          | 6                   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                     |
| Group variables                                  | None                |

Data summary

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts |
|:---|---:|---:|:---|---:|:---|
| sex | 0 | 1.00 | FALSE | 2 | F: 7839, M: 7721 |
| race_eth | 0 | 1.00 | FALSE | 5 | NHW: 5271, NHB: 4098, His: 3534, NHA: 1638 |
| educ | 6343 | 0.59 | FALSE | 5 | som: 2975, \>= : 2257, HS/: 2225, 9-1: 1041 |
| marital | 6338 | 0.59 | FALSE | 3 | mar: 5279, wid: 2148, nev: 1795 |
| physact | 5869 | 0.62 | FALSE | 2 | Ina: 5787, Act: 3904 |
| cig100 | 5872 | 0.62 | FALSE | 2 | No: 5799, Yes: 3889 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| seqn | 0 | 1.00 | 117042.50 | 4491.93 | 109263.0 | 113152.8 | 117042.5 | 120932.2 | 124822.0 | ▇▇▇▇▇ |
| dep_score | 6595 | 0.58 | 3.06 | 4.21 | 0.0 | 0.0 | 1.0 | 4.0 | 27.0 | ▇▁▁▁▁ |
| age | 0 | 1.00 | 33.74 | 25.32 | 0.0 | 10.0 | 30.0 | 56.0 | 80.0 | ▇▃▃▃▃ |
| alc_drinks | 9707 | 0.38 | 2.53 | 2.11 | 1.0 | 1.0 | 2.0 | 3.0 | 15.0 | ▇▂▁▁▁ |
| bmi | 2423 | 0.84 | 26.66 | 8.42 | 11.9 | 20.4 | 25.8 | 31.4 | 92.3 | ▇▅▁▁▁ |
| sleep_hrs | 5455 | 0.65 | 7.64 | 1.68 | 2.0 | 7.0 | 7.5 | 8.5 | 14.0 | ▁▃▇▂▁ |

``` r
#check number of missing values in each column
nhanes_2017_Mar2020 %>% 
  map(is.na) %>%
  map(sum)
```

    ## $seqn
    ## [1] 0
    ## 
    ## $dep_score
    ## [1] 6595
    ## 
    ## $sex
    ## [1] 0
    ## 
    ## $age
    ## [1] 0
    ## 
    ## $race_eth
    ## [1] 0
    ## 
    ## $educ
    ## [1] 6343
    ## 
    ## $marital
    ## [1] 6338
    ## 
    ## $physact
    ## [1] 5869
    ## 
    ## $alc_drinks
    ## [1] 9707
    ## 
    ## $cig100
    ## [1] 5872
    ## 
    ## $bmi
    ## [1] 2423
    ## 
    ## $sleep_hrs
    ## [1] 5455

``` r
vis_miss(nhanes_2017_Mar2020)
```

![](Depression_LinReg_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
nhanes_2017_Mar2020_nomissing <- nhanes_2017_Mar2020 %>% drop_na()

dim(nhanes_2017_Mar2020_nomissing)
```

    ## [1] 5514   12

``` r
skim(nhanes_2017_Mar2020_nomissing)
```

|  |  |
|:---|:---|
| Name | nhanes_2017_Mar2020_nomis… |
| Number of rows | 5514 |
| Number of columns | 12 |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |  |
| Column type frequency: |  |
| factor | 6 |
| numeric | 6 |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |  |
| Group variables | None |

Data summary

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts |
|:---|---:|---:|:---|---:|:---|
| sex | 0 | 1 | FALSE | 2 | M: 2823, F: 2691 |
| race_eth | 0 | 1 | FALSE | 5 | NHW: 2011, NHB: 1467, His: 1238, NHA: 520 |
| educ | 0 | 1 | FALSE | 5 | som: 1968, \>= : 1473, HS/: 1294, 9-1: 513 |
| marital | 0 | 1 | FALSE | 3 | mar: 3216, nev: 1179, wid: 1119 |
| physact | 0 | 1 | FALSE | 2 | Ina: 3062, Act: 2452 |
| cig100 | 0 | 1 | FALSE | 2 | No: 3042, Yes: 2472 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| seqn | 0 | 1 | 117138.07 | 4512.13 | 109266.0 | 113222.2 | 117142.5 | 121081.5 | 124822.0 | ▇▇▇▇▇ |
| dep_score | 0 | 1 | 3.26 | 4.20 | 0.0 | 0.0 | 2.0 | 5.0 | 27.0 | ▇▂▁▁▁ |
| age | 0 | 1 | 47.99 | 17.02 | 20.0 | 33.0 | 48.0 | 62.0 | 80.0 | ▇▇▇▇▅ |
| alc_drinks | 0 | 1 | 2.51 | 2.11 | 1.0 | 1.0 | 2.0 | 3.0 | 15.0 | ▇▂▁▁▁ |
| bmi | 0 | 1 | 30.10 | 7.60 | 14.6 | 25.0 | 28.8 | 33.8 | 92.3 | ▇▆▁▁▁ |
| sleep_hrs | 0 | 1 | 7.51 | 1.58 | 2.0 | 6.5 | 7.5 | 8.5 | 14.0 | ▁▃▇▁▁ |

#### Correlation

``` r
nhanes_2017_Mar2020_nomissing %>% 
  select_if(is.numeric) %>%
  chart.Correlation()
```

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

![](Depression_LinReg_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

#### Density Plot

``` r
#Check the distribution of the DV: depression score
ggplot(nhanes_2017_Mar2020_nomissing, aes(dep_score)) + geom_density()
```

![](Depression_LinReg_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
#the DV is not normally distributed
```

``` r
#Re-Check the distribution of the DV
par(mfrow=c(2,2))
ggplot(nhanes_2017_Mar2020_nomissing, aes(dep_score)) + geom_density() #raw value
```

![](Depression_LinReg_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
ggplot(nhanes_2017_Mar2020_nomissing, aes(log(dep_score))) + geom_density() #log transformation
```

    ## Warning: Removed 1802 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](Depression_LinReg_files/figure-gfm/unnamed-chunk-44-2.png)<!-- -->

``` r
ggplot(nhanes_2017_Mar2020_nomissing, aes(sqrt(dep_score))) + geom_density() #square root transformation
```

![](Depression_LinReg_files/figure-gfm/unnamed-chunk-44-3.png)<!-- -->

``` r
ggplot(nhanes_2017_Mar2020_nomissing, aes(sqrt(sqrt(dep_score)))) + geom_density() #4th root transformation
```

![](Depression_LinReg_files/figure-gfm/unnamed-chunk-44-4.png)<!-- -->

There is no notable improvement in the shape, even after log, square
root, and 4th root transformations.

Go ahead and build the model; run the assumption checks after.

## Model A (With Raw DV)

#### Split the data into training and test sets

``` r
set.seed(58)
nhanes_split <- initial_split(nhanes_2017_Mar2020_nomissing, prop = 3/4) #75% for training set

nhanes_split # count of training vs. test vs total
```

    ## <Training/Testing/Total>
    ## <4135/1379/5514>

``` r
train_nhanes <- training(nhanes_split)
test_nhanes <- testing(nhanes_split)
```

### Data preprocessing

``` r
names(train_nhanes)
```

    ##  [1] "seqn"       "dep_score"  "sex"        "age"        "race_eth"  
    ##  [6] "educ"       "marital"    "physact"    "alc_drinks" "cig100"    
    ## [11] "bmi"        "sleep_hrs"

``` r
#specify variable roles (predictors vs outcome)
dep_recipe <- train_nhanes %>%
  recipe(dep_score ~ sex + age + race_eth + educ + marital + physact + alc_drinks + 
           cig100 + bmi + sleep_hrs)

dep_recipe
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:    1
    ## predictor: 10

``` r
# 1 outcome, 10 features/predictors
```

``` r
summary(dep_recipe)
```

    ## # A tibble: 11 × 4
    ##    variable   type      role      source  
    ##    <chr>      <list>    <chr>     <chr>   
    ##  1 sex        <chr [3]> predictor original
    ##  2 age        <chr [2]> predictor original
    ##  3 race_eth   <chr [3]> predictor original
    ##  4 educ       <chr [3]> predictor original
    ##  5 marital    <chr [3]> predictor original
    ##  6 physact    <chr [3]> predictor original
    ##  7 alc_drinks <chr [2]> predictor original
    ##  8 cig100     <chr [3]> predictor original
    ##  9 bmi        <chr [2]> predictor original
    ## 10 sleep_hrs  <chr [2]> predictor original
    ## 11 dep_score  <chr [2]> outcome   original

In the above output, \<chr\[3\]\> and \<chr\[2\> denote character and
numeric types, respectively.

``` r
#dummy recode categorical predictors
dep_recipe <- dep_recipe %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE)
#one_hot=FALSE: omits the first dummy variable (to serve as reference category) in each categorical predictor. In anticipation of this, the desired reference group was placed first when naming labels and levels (in the EDA section above).

# Otherwise, one_hot=TRUE: omits no category, if no reference category is needed
dep_recipe
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:    1
    ## predictor: 10

    ## 

    ## ── Operations

    ## • Dummy variables from: all_nominal_predictors()

#### Model specification

``` r
ols_model <- linear_reg()
```

``` r
ols_model <- ols_model %>% 
  set_engine("lm") %>%
  set_mode("regression")

ols_model
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

#### Simulate the preprocessing

``` r
recipe_prepped <- prep(dep_recipe, verbose = TRUE, retain = TRUE)
```

    ## oper 1 step dummy [training] 
    ## The retained training set is ~ 0.57 Mb  in memory.

``` r
recipe_prepped
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:    1
    ## predictor: 10

    ## 

    ## ── Training information

    ## Training data contained 4135 data points and no incomplete rows.

    ## 

    ## ── Operations

    ## • Dummy variables from: sex, race_eth, educ, marital, physact, ... | Trained

``` r
prepped_train <- bake(recipe_prepped, new_data = NULL)
glimpse(prepped_train)
```

    ## Rows: 4,135
    ## Columns: 18
    ## $ age                    <dbl> 69, 75, 57, 67, 33, 21, 34, 23, 29, 60, 44, 35,…
    ## $ alc_drinks             <dbl> 2, 6, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 3,…
    ## $ bmi                    <dbl> 31.1, 22.9, 23.8, 31.1, 30.4, 39.1, 36.4, 27.7,…
    ## $ sleep_hrs              <dbl> 9.0, 14.0, 8.0, 10.0, 7.5, 5.0, 7.0, 6.5, 5.5, …
    ## $ dep_score              <dbl> 0, 2, 7, 0, 1, 6, 11, 4, 4, 0, 0, 0, 0, 0, 0, 0…
    ## $ sex_F                  <dbl> 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0,…
    ## $ race_eth_Hispanic      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0,…
    ## $ race_eth_NHBlack       <dbl> 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1,…
    ## $ race_eth_NHAsian       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ race_eth_Others        <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ educ_X.9th.grade       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ educ_X9.11th.grade     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,…
    ## $ educ_some.college      <dbl> 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0,…
    ## $ educ_X...college.grad  <dbl> 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,…
    ## $ marital_wid..div..sep. <dbl> 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ marital_nev..mard.     <dbl> 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ physact_Inactive       <dbl> 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1,…
    ## $ cig100_No              <dbl> 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,…

#### Model Fitting

``` r
ols_workflow <- workflow() %>%
  add_recipe(dep_recipe) %>%
  add_model(ols_model)

ols_workflow
```

    ## ══ Workflow ════════════════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: linear_reg()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 1 Recipe Step
    ## 
    ## • step_dummy()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
#Now estimate the model parameters/constants using the training data, i.e., fit the model using the data
ols_fit <- fit(ols_workflow, data=train_nhanes)

ols_fit
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: linear_reg()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 1 Recipe Step
    ## 
    ## • step_dummy()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## 
    ## Call:
    ## stats::lm(formula = ..y ~ ., data = data)
    ## 
    ## Coefficients:
    ##            (Intercept)                     age              alc_drinks  
    ##                2.71230                -0.01896                 0.11663  
    ##                    bmi               sleep_hrs                   sex_F  
    ##                0.05838                -0.12355                 1.03509  
    ##      race_eth_Hispanic        race_eth_NHBlack        race_eth_NHAsian  
    ##               -0.44581                -0.60269                -0.50355  
    ##        race_eth_Others        educ_X.9th.grade      educ_X9.11th.grade  
    ##                0.50000                 0.57652                 0.66845  
    ##      educ_some.college   educ_X...college.grad  marital_wid..div..sep.  
    ##               -0.14655                -0.57567                 1.20262  
    ##     marital_nev..mard.        physact_Inactive               cig100_No  
    ##                0.66648                 0.39424                -0.77434

#### Model Evaluation

``` r
ols_fitstats <- ols_fit %>%
  extract_fit_parsnip()

head(ols_fitstats$fit$fitted.values)
```

    ##        1        2        3        4        5        6 
    ## 1.986387 2.591858 3.968830 1.680077 2.095856 4.814690

``` r
predict(ols_fit, new_data = test_nhanes)
```

    ## # A tibble: 1,379 × 1
    ##    .pred
    ##    <dbl>
    ##  1 3.41 
    ##  2 4.31 
    ##  3 3.13 
    ##  4 3.78 
    ##  5 3.23 
    ##  6 2.50 
    ##  7 2.04 
    ##  8 0.351
    ##  9 1.62 
    ## 10 3.47 
    ## # ℹ 1,369 more rows

Get more information on the prediction/evaluation performance:

``` r
ols_fitted_values <- augment(ols_fitstats$fit, data = prepped_train)
```

``` r
some_columns <- ols_fitted_values[, c("dep_score", ".fitted", ".std.resid")]
some_columns
```

    ## # A tibble: 4,135 × 3
    ##    dep_score .fitted .std.resid
    ##        <dbl>   <dbl>      <dbl>
    ##  1         0    1.99    -0.496 
    ##  2         2    2.59    -0.148 
    ##  3         7    3.97     0.759 
    ##  4         0    1.68    -0.420 
    ##  5         1    2.10    -0.274 
    ##  6         6    4.81     0.296 
    ##  7        11    4.45     1.64  
    ##  8         4    4.91    -0.227 
    ##  9         4    4.34    -0.0844
    ## 10         0    2.09    -0.522 
    ## # ℹ 4,125 more rows

``` r
# obtain the actual rmse value
rmse(ols_fitted_values, truth = dep_score, estimate = .fitted)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rmse    standard        4.00

RMSE is 4.00 (pretty high), model appears to perform poorly.

Obtain a visual on model performance:

``` r
ols_fitted_values %>%
  ggplot(aes(x = dep_score, y = .fitted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs( x = "True depression score", y = "Predicted depression score")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Depression_LinReg_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

Try Boxcox transformation of the DV

## Model B (Box-Cox-transformed DV)

``` r
#apply the boxcox function
#The function works only with positive values. Dv contains 0 values, so add a constant across:
nhanes_2017_Mar2020_nomissing["dep_modified"] <- nhanes_2017_Mar2020_nomissing$dep_score + 1
# add 1 as a constant

boxcox_trans <- boxcox(lm(dep_modified ~ sex + age + race_eth + educ + marital + physact + alc_drinks + 
           cig100 + bmi + sleep_hrs, data = nhanes_2017_Mar2020_nomissing), 
           lambda = seq(-2,2,0.1))
```

![](Depression_LinReg_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

``` r
#Determine the optimal lambda value
lambda_opt <- boxcox_trans$x[which.max(boxcox_trans$y)]
lambda_opt
```

    ## [1] -0.1818182

``` r
#add a new column containing the transformed value of the Dv
nhanes_2017_Mar2020_nomissing$dep_boxcox <- 
  ifelse(lambda_opt==0,
         log(nhanes_2017_Mar2020_nomissing$dep_modified),
         (nhanes_2017_Mar2020_nomissing$dep_modified ^ lambda_opt - 1) / lambda_opt)

# View(nhanes_2017_Mar2020_nomissing)
# the transformed DV values are the same (=0)
# this may be due to the lambda optimal value (-0.1818182) being close to zero. Try log instead
```

## Model C (Log-transformed DV)

``` r
nhanes_2017_Mar2020_nomissing$dep_log <- log(nhanes_2017_Mar2020_nomissing$dep_modified)
nhanes_log <- nhanes_2017_Mar2020_nomissing[c(1,3:12,15)]
names(nhanes_log)
```

    ##  [1] "seqn"       "sex"        "age"        "race_eth"   "educ"      
    ##  [6] "marital"    "physact"    "alc_drinks" "cig100"     "bmi"       
    ## [11] "sleep_hrs"  "dep_log"

#### Split the data into training and test sets

``` r
set.seed(58)
nhanes_log_split <- initial_split(nhanes_log, prop = 3/4) #75% for training set

nhanes_log_split # count of training vs. test vs total
```

    ## <Training/Testing/Total>
    ## <4135/1379/5514>

``` r
train_nhanes_log <- training(nhanes_log_split)
test_nhanes_log <- testing(nhanes_log_split)
```

### Data preprocessing

``` r
names(train_nhanes_log)
```

    ##  [1] "seqn"       "sex"        "age"        "race_eth"   "educ"      
    ##  [6] "marital"    "physact"    "alc_drinks" "cig100"     "bmi"       
    ## [11] "sleep_hrs"  "dep_log"

``` r
#specify variable roles (predictors vs outcome)
log_recipe <- train_nhanes_log %>%
  recipe(dep_log ~ sex + age + race_eth + educ + marital + physact + alc_drinks + 
           cig100 + bmi + sleep_hrs)

log_recipe
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:    1
    ## predictor: 10

``` r
# 1 outcome, 10 features/predictors
```

``` r
summary(log_recipe)
```

    ## # A tibble: 11 × 4
    ##    variable   type      role      source  
    ##    <chr>      <list>    <chr>     <chr>   
    ##  1 sex        <chr [3]> predictor original
    ##  2 age        <chr [2]> predictor original
    ##  3 race_eth   <chr [3]> predictor original
    ##  4 educ       <chr [3]> predictor original
    ##  5 marital    <chr [3]> predictor original
    ##  6 physact    <chr [3]> predictor original
    ##  7 alc_drinks <chr [2]> predictor original
    ##  8 cig100     <chr [3]> predictor original
    ##  9 bmi        <chr [2]> predictor original
    ## 10 sleep_hrs  <chr [2]> predictor original
    ## 11 dep_log    <chr [2]> outcome   original

In the above output, \<chr\[3\]\> and \<chr\[2\> denote character and
numeric types, respectively.

``` r
#dummy recode categorical predictors
log_recipe <- log_recipe %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE)

log_recipe
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:    1
    ## predictor: 10

    ## 

    ## ── Operations

    ## • Dummy variables from: all_nominal_predictors()

#### Model specification

``` r
log_OLSmodel <- linear_reg()
```

``` r
log_OLSmodel <- log_OLSmodel %>% 
  set_engine("lm") %>%
  set_mode("regression")

log_OLSmodel
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

#### Simulate the preprocessing

``` r
LOGrecipe_prepped <- prep(log_recipe, verbose = TRUE, retain = TRUE)
```

    ## oper 1 step dummy [training] 
    ## The retained training set is ~ 0.57 Mb  in memory.

``` r
LOGrecipe_prepped
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:    1
    ## predictor: 10

    ## 

    ## ── Training information

    ## Training data contained 4135 data points and no incomplete rows.

    ## 

    ## ── Operations

    ## • Dummy variables from: sex, race_eth, educ, marital, physact, ... | Trained

``` r
LOGprepped_train <- bake(LOGrecipe_prepped, new_data = NULL)
glimpse(LOGprepped_train)
```

    ## Rows: 4,135
    ## Columns: 18
    ## $ age                    <dbl> 69, 75, 57, 67, 33, 21, 34, 23, 29, 60, 44, 35,…
    ## $ alc_drinks             <dbl> 2, 6, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 3,…
    ## $ bmi                    <dbl> 31.1, 22.9, 23.8, 31.1, 30.4, 39.1, 36.4, 27.7,…
    ## $ sleep_hrs              <dbl> 9.0, 14.0, 8.0, 10.0, 7.5, 5.0, 7.0, 6.5, 5.5, …
    ## $ dep_log                <dbl> 0.0000000, 1.0986123, 2.0794415, 0.0000000, 0.6…
    ## $ sex_F                  <dbl> 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0,…
    ## $ race_eth_Hispanic      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0,…
    ## $ race_eth_NHBlack       <dbl> 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1,…
    ## $ race_eth_NHAsian       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ race_eth_Others        <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ educ_X.9th.grade       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ educ_X9.11th.grade     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,…
    ## $ educ_some.college      <dbl> 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0,…
    ## $ educ_X...college.grad  <dbl> 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,…
    ## $ marital_wid..div..sep. <dbl> 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ marital_nev..mard.     <dbl> 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ physact_Inactive       <dbl> 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1,…
    ## $ cig100_No              <dbl> 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,…

#### Model Fitting

``` r
log_workflow <- workflow() %>%
  add_recipe(log_recipe) %>%
  add_model(log_OLSmodel)

log_workflow
```

    ## ══ Workflow ════════════════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: linear_reg()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 1 Recipe Step
    ## 
    ## • step_dummy()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
#Now estimate the model parameters/constants using the training data
logOLS_fit <- fit(log_workflow, data=train_nhanes_log)

logOLS_fit
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: linear_reg()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 1 Recipe Step
    ## 
    ## • step_dummy()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## 
    ## Call:
    ## stats::lm(formula = ..y ~ ., data = data)
    ## 
    ## Coefficients:
    ##            (Intercept)                     age              alc_drinks  
    ##               0.949511               -0.005143                0.022973  
    ##                    bmi               sleep_hrs                   sex_F  
    ##               0.012285               -0.022435                0.264653  
    ##      race_eth_Hispanic        race_eth_NHBlack        race_eth_NHAsian  
    ##              -0.110707               -0.154294               -0.155479  
    ##        race_eth_Others        educ_X.9th.grade      educ_X9.11th.grade  
    ##               0.117581                0.024873                0.134967  
    ##      educ_some.college   educ_X...college.grad  marital_wid..div..sep.  
    ##              -0.031408               -0.140263                0.260106  
    ##     marital_nev..mard.        physact_Inactive               cig100_No  
    ##               0.153481                0.077465               -0.150796

#### Model Evaluation

``` r
LOGols_fitstats <- logOLS_fit %>%
  extract_fit_parsnip()

head(LOGols_fitstats$fit$fitted.values)
```

    ##         1         2         3         4         5         6 
    ## 0.7125083 0.8521473 1.1929025 0.6683329 0.7943715 1.4231538

``` r
predict(ols_fit, new_data = test_nhanes)
```

    ## # A tibble: 1,379 × 1
    ##    .pred
    ##    <dbl>
    ##  1 3.41 
    ##  2 4.31 
    ##  3 3.13 
    ##  4 3.78 
    ##  5 3.23 
    ##  6 2.50 
    ##  7 2.04 
    ##  8 0.351
    ##  9 1.62 
    ## 10 3.47 
    ## # ℹ 1,369 more rows

Obtain more information on the prediction/evaluation performance:

``` r
LOGols_fitted_values <- augment(LOGols_fitstats$fit, data = LOGprepped_train)
```

``` r
check <- LOGols_fitted_values[, c("dep_log", ".fitted", ".std.resid")]
check
```

    ## # A tibble: 4,135 × 3
    ##    dep_log .fitted .std.resid
    ##      <dbl>   <dbl>      <dbl>
    ##  1   0       0.713     -0.834
    ##  2   1.10    0.852      0.290
    ##  3   2.08    1.19       1.04 
    ##  4   0       0.668     -0.783
    ##  5   0.693   0.794     -0.119
    ##  6   1.95    1.42       0.613
    ##  7   2.48    1.34       1.34 
    ##  8   1.61    1.47       0.167
    ##  9   1.61    1.16       0.531
    ## 10   0       0.736     -0.861
    ## # ℹ 4,125 more rows

``` r
# obtain the actual rmse value
rmse(LOGols_fitted_values, truth = dep_log, estimate = .fitted)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rmse    standard       0.853

RMSE is 0.85, much more lower than the initial value of 4.00 (from the
model with raw DV). Model appears to have improved tremendously with log
transformation

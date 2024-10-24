my project and dataset
================
Jack Sutton
2024-10-10

# Is the relationship between peer influence and relational aggression different between young men and women?

# load packages

``` r
library(haven)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(tidyr)
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(bruceR)
```

    ## 
    ## bruceR (v2024.6)
    ## Broadly Useful Convenient and Efficient R functions
    ## 
    ## Packages also loaded:
    ## ✔ data.table ✔ emmeans
    ## ✔ dplyr      ✔ lmerTest
    ## ✔ tidyr      ✔ effectsize
    ## ✔ stringr    ✔ performance
    ## ✔ ggplot2    ✔ interactions
    ## 
    ## Main functions of `bruceR`:
    ## cc()             Describe()  TTEST()
    ## add()            Freq()      MANOVA()
    ## .mean()          Corr()      EMMEANS()
    ## set.wd()         Alpha()     PROCESS()
    ## import()         EFA()       model_summary()
    ## print_table()    CFA()       lavaan_summary()
    ## 
    ## For full functionality, please install all dependencies:
    ## install.packages("bruceR", dep=TRUE)
    ## 
    ## Online documentation:
    ## https://psychbruce.github.io/bruceR
    ## 
    ## To use this package in publications, please cite:
    ## Bao, H.-W.-S. (2024). bruceR: Broadly useful convenient and efficient R functions (Version 2024.6) [Computer software]. https://CRAN.R-project.org/package=bruceR

    ## 
    ## These packages are dependencies of `bruceR` but not installed:
    ## - pacman, openxlsx, ggtext, lmtest, vars, phia, MuMIn, GGally
    ## 
    ## ***** Install all dependencies *****
    ## install.packages("bruceR", dep=TRUE)

``` r
library(ggstatsplot)
```

    ## You can cite this package as:
    ##      Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
    ##      Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

``` r
library(performance)
library(sjPlot)
```

    ## Install package "strengejacke" from GitHub (`devtools::install_github("strengejacke/strengejacke")`) to load all sj-packages at once!

\<\<\<\<\<\<\< HEAD \# load

``` r
load("C:/Users/ajsut/Documents/GitHub/myproject/36850-0003-Data.rda")
```

# ignore other variables

``` r
new_dataset <- da36850.0003 %>%
  select(RESPEERT2, RELAGGT2, DEMGEN2)
```

# RESPEERT2 is a premade composite of Revised Peer Influence Scale Items with reverse codings corrected. 1 represents low peer resistance, 4 represents high peer resistance.

# RELAGGT2 is a premade composite of Relational Aggression, 1 Represents low tendency towards relational aggression, 4 represents high tendency towards relational aggression.

# DEMGEN2 is a grouping variable by gender demographic.

``` r
summary(new_dataset)
```

    ##    RESPEERT2        RELAGGT2            DEMGEN2   
    ##  Min.   :1.300   Min.   :0.0000   (0) Male  :332  
    ##  1st Qu.:2.600   1st Qu.:0.0000   (1) Female:401  
    ##  Median :3.000   Median :0.0000   NA's      :  1  
    ##  Mean   :2.975   Mean   :0.1992                   
    ##  3rd Qu.:3.400   3rd Qu.:0.2857                   
    ##  Max.   :4.000   Max.   :3.0000                   
    ##  NA's   :4

\#summary indicates NA values for some variables.

``` r
list_new_dataset <- drop_na(new_dataset)
```

\#NA are dropped listwise to clean data. Participants with NA entries on
any variable are omitted from the dataset.

``` r
summary(list_new_dataset)
```

    ##    RESPEERT2        RELAGGT2            DEMGEN2   
    ##  Min.   :1.300   Min.   :0.0000   (0) Male  :331  
    ##  1st Qu.:2.600   1st Qu.:0.0000   (1) Female:398  
    ##  Median :3.000   Median :0.0000                   
    ##  Mean   :2.974   Mean   :0.2003                   
    ##  3rd Qu.:3.400   3rd Qu.:0.2857                   
    ##  Max.   :4.000   Max.   :3.0000

\#Summary indicates no more missing values in new dataset
(list_new_dataset)

\#normality check of the variables, Shapiro-Wilk and GGPlot

``` r
describe(list_new_dataset$RELAGGT2)
```

    ##    vars   n mean   sd median trimmed mad min max range skew kurtosis   se
    ## X1    1 729  0.2 0.38      0    0.11   0   0   3     3 3.58    17.53 0.01

``` r
shapiro.test(list_new_dataset$RELAGGT2)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  list_new_dataset$RELAGGT2
    ## W = 0.57438, p-value < 2.2e-16

``` r
ggplot(list_new_dataset, aes(x = RELAGGT2)) + geom_histogram(binwidth = 1)+ theme_light()
```

![](myproject--with-transformation-_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(list_new_dataset, aes(x = RELAGGT2)) + geom_density(adjust = 2)  + theme_classic()
```

![](myproject--with-transformation-_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
qq<-ggplot(list_new_dataset, aes(sample = RELAGGT2)) + geom_qq()  + theme_classic()

qq+ geom_qq_line()
```

![](myproject--with-transformation-_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->
Result of Shapiro-Wilk test indicates a low W-Value of 0.57438 and a
p-value of p-value \< 2.2e-16. This means that the distribution is very
significantly non-normal. GGplot also indicates a positive skew, it is
visually non-normal.

\#Use the describeBy() function to get skewness and kurtosis by group

``` r
describeBy(RELAGGT2 ~ DEMGEN2, data = list_new_dataset)
```

    ## 
    ##  Descriptive statistics by group 
    ## DEMGEN2: (0) Male
    ##          vars   n mean   sd median trimmed mad min max range skew kurtosis   se
    ## RELAGGT2    1 331 0.24 0.45      0    0.13   0   0   3     3 3.12    11.96 0.02
    ## ------------------------------------------------------------ 
    ## DEMGEN2: (1) Female
    ##          vars   n mean  sd median trimmed mad min max range skew kurtosis   se
    ## RELAGGT2    1 398 0.17 0.3      0    0.11   0   0   3     3 3.85    24.44 0.01

``` r
list_new_dataset %>%
  group_by(DEMGEN2) %>%
  summarize(W = shapiro.test(RELAGGT2)$statistic, p_value = shapiro.test(RELAGGT2)$p.value)
```

    ## # A tibble: 2 × 3
    ##   DEMGEN2        W  p_value
    ##   <fct>      <dbl>    <dbl>
    ## 1 (0) Male   0.583 2.00e-27
    ## 2 (1) Female 0.598 2.35e-29

\#For both males and females, the results of skew tests indicate a
positive right skew above +1. Kurtosis tests indicated that the extent
of the skew for each group will result in a high frequency of extreme
values.

\#equal variance check for DV not necessasry since my question is an
outcome, not a simple difference. \#correlation testing does not require
normality so I can move on to testing… but first some assumption checks

\#LAB 8 STARTS HERE

``` r
model<-lm(RELAGGT2 ~ RESPEERT2 + DEMGEN2, data = list_new_dataset)

check_model(model)
```

![](myproject--with-transformation-_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

\#assumption of colinearity is met (\<5)

\#all of the below tests are correlations. I ran a correlation to see
the relation between my IV and DV, then I ran the same tests for gender,
and race (white, nonwhite) to see whether the correlation differs in
those groups.

\#first check is if RELAGGT2 is related to RESPEERT2 without any
grouping variable…

``` r
Corr(list_new_dataset)
```

    ## NOTE: `DEMGEN2` transformed to numeric.
    ## 
    ## Pearson's r and 95% confidence intervals:
    ## ──────────────────────────────────────────────────────
    ##                         r       [95% CI]     p       N
    ## ──────────────────────────────────────────────────────
    ## RESPEERT2-RELAGGT2  -0.25 [-0.32, -0.18] <.001 *** 729
    ## RESPEERT2-DEMGEN2    0.08 [ 0.01,  0.15]  .026 *   729
    ## RELAGGT2-DEMGEN2    -0.09 [-0.16, -0.02]  .013 *   729
    ## ──────────────────────────────────────────────────────

![](myproject--with-transformation-_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    ## Correlation matrix is displayed in the RStudio `Plots` Pane.

\#Initial correlation shows resistance to peer influence is
significantly inversely correlated with relational aggression. As
resistance to peer influence increases, relational aggression decreases
and as resistance to peer influence decreases, relational aggression
increases (r= -.25).

\#now, to test this correlation on the grouping variable of gender…

``` r
PROCESS(list_new_dataset, y = "RELAGGT2", x = "RESPEERT2", mods = c("DEMGEN2"))
```

    ## 
    ## ****************** PART 1. Regression Model Summary ******************
    ## 
    ## PROCESS Model Code : 1 (Hayes, 2018; www.guilford.com/p/hayes3)
    ## PROCESS Model Type : Simple Moderation
    ## -    Outcome (Y) : RELAGGT2
    ## -  Predictor (X) : RESPEERT2
    ## -  Mediators (M) : -
    ## - Moderators (W) : DEMGEN2
    ## - Covariates (C) : -
    ## -   HLM Clusters : -
    ## 
    ## All numeric predictors have been grand-mean centered.
    ## (For details, please see the help page of PROCESS.)
    ## 
    ## Formula of Outcome:
    ## -    RELAGGT2 ~ RESPEERT2*DEMGEN2
    ## 
    ## CAUTION:
    ##   Fixed effect (coef.) of a predictor involved in an interaction
    ##   denotes its "simple effect/slope" at the other predictor = 0.
    ##   Only when all predictors in an interaction are mean-centered
    ##   can the fixed effect denote the "main effect"!
    ##   
    ## Model Summary
    ## 
    ## ───────────────────────────────────────────────────────
    ##                              (1) RELAGGT2  (2) RELAGGT2
    ## ───────────────────────────────────────────────────────
    ## (Intercept)                    0.200 ***     0.226 *** 
    ##                               (0.014)       (0.020)    
    ## RESPEERT2                     -0.167 ***    -0.249 *** 
    ##                               (0.024)       (0.035)    
    ## DEMGEN2(1) Female                           -0.053 *   
    ##                                             (0.027)    
    ## RESPEERT2:DEMGEN2(1) Female                  0.158 *** 
    ##                                             (0.047)    
    ## ───────────────────────────────────────────────────────
    ## R^2                            0.063         0.082     
    ## Adj. R^2                       0.062         0.079     
    ## Num. obs.                    729           729         
    ## ───────────────────────────────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## ************ PART 2. Mediation/Moderation Effect Estimate ************
    ## 
    ## Package Use : ‘interactions’ (v1.2.0)
    ## Effect Type : Simple Moderation (Model 1)
    ## Sample Size : 729
    ## Random Seed : -
    ## Simulations : -
    ## 
    ## Interaction Effect on "RELAGGT2" (Y)
    ## ────────────────────────────────────────────
    ##                          F df1 df2     p    
    ## ────────────────────────────────────────────
    ## RESPEERT2 * DEMGEN2  11.12   1 725 <.001 ***
    ## ────────────────────────────────────────────
    ## 
    ## Simple Slopes: "RESPEERT2" (X) ==> "RELAGGT2" (Y)
    ## ────────────────────────────────────────────────────────────
    ##  "DEMGEN2"  Effect    S.E.      t     p             [95% CI]
    ## ────────────────────────────────────────────────────────────
    ##  (0) Male   -0.249 (0.035) -7.105 <.001 *** [-0.318, -0.180]
    ##  (1) Female -0.091 (0.032) -2.827  .005 **  [-0.154, -0.028]
    ## ────────────────────────────────────────────────────────────

\#These results indicate that there is a significant difference for the
peer influence and relational aggression relationship between men and
women. For men, this relationship is significantly inversely related
(r=-.249). For women, it is inversely related as well, but not to a
significant extent (r=-.091). \#let’s visualize…

``` r
ggplot(list_new_dataset, aes(x = RESPEERT2, y = RELAGGT2)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~DEMGEN2) + theme_bruce()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](myproject--with-transformation-_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
Research Question, First Analysis Interpretation \#My research question
is whether men and women differ significantly in the extent to which
peer influence relates to relational aggression behaviors. From this
correlation, we can interpret that men are less resistant to peer
influence may relate to more relational aggression compared to women. We
can see the same pattern in the plots. The line is clearly steeper for
men.

\#now I’ll try this with a different grouping variable…

``` r
#get the excluded variables back and select new ones...
new_dataset_white <- da36850.0003 %>%
  select(RESPEERT2, RELAGGT2, DEMRA2010)
#exclude NA
list_new_dataset_white <- drop_na(new_dataset_white)
#check correlation
PROCESS(list_new_dataset_white, y = "RELAGGT2", x = "RESPEERT2", mods = c("DEMRA2010"))
```

    ## 
    ## ****************** PART 1. Regression Model Summary ******************
    ## 
    ## PROCESS Model Code : 1 (Hayes, 2018; www.guilford.com/p/hayes3)
    ## PROCESS Model Type : Simple Moderation
    ## -    Outcome (Y) : RELAGGT2
    ## -  Predictor (X) : RESPEERT2
    ## -  Mediators (M) : -
    ## - Moderators (W) : DEMRA2010
    ## - Covariates (C) : -
    ## -   HLM Clusters : -
    ## 
    ## All numeric predictors have been grand-mean centered.
    ## (For details, please see the help page of PROCESS.)
    ## 
    ## Formula of Outcome:
    ## -    RELAGGT2 ~ RESPEERT2*DEMRA2010
    ## 
    ## CAUTION:
    ##   Fixed effect (coef.) of a predictor involved in an interaction
    ##   denotes its "simple effect/slope" at the other predictor = 0.
    ##   Only when all predictors in an interaction are mean-centered
    ##   can the fixed effect denote the "main effect"!
    ##   
    ## Model Summary
    ## 
    ## ──────────────────────────────────────────────────────
    ##                             (1) RELAGGT2  (2) RELAGGT2
    ## ──────────────────────────────────────────────────────
    ## (Intercept)                   0.200 ***     0.258 *** 
    ##                              (0.014)       (0.024)    
    ## RESPEERT2                    -0.167 ***    -0.245 *** 
    ##                              (0.024)       (0.040)    
    ## DEMRA2010(1) Yes                           -0.082 **  
    ##                                            (0.029)    
    ## RESPEERT2:DEMRA2010(1) Yes                  0.116 *   
    ##                                            (0.049)    
    ## ──────────────────────────────────────────────────────
    ## R^2                           0.064         0.080     
    ## Adj. R^2                      0.062         0.076     
    ## Num. obs.                   730           730         
    ## ──────────────────────────────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## ************ PART 2. Mediation/Moderation Effect Estimate ************
    ## 
    ## Package Use : ‘interactions’ (v1.2.0)
    ## Effect Type : Simple Moderation (Model 1)
    ## Sample Size : 730
    ## Random Seed : -
    ## Simulations : -
    ## 
    ## Interaction Effect on "RELAGGT2" (Y)
    ## ─────────────────────────────────────────────
    ##                           F df1 df2     p    
    ## ─────────────────────────────────────────────
    ## RESPEERT2 * DEMRA2010  5.53   1 726  .019 *  
    ## ─────────────────────────────────────────────
    ## 
    ## Simple Slopes: "RESPEERT2" (X) ==> "RELAGGT2" (Y)
    ## ─────────────────────────────────────────────────────────────
    ##  "DEMRA2010" Effect    S.E.      t     p             [95% CI]
    ## ─────────────────────────────────────────────────────────────
    ##  (0) No      -0.245 (0.040) -6.141 <.001 *** [-0.323, -0.167]
    ##  (1) Yes     -0.129 (0.029) -4.395 <.001 *** [-0.186, -0.071]
    ## ─────────────────────────────────────────────────────────────

``` r
ggplot(list_new_dataset_white, aes(x = RESPEERT2, y = RELAGGT2)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~DEMRA2010) + theme_bruce()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](myproject--with-transformation-_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
Second Analysis Interpretation \#Comparing the same relationship in
terms of being white or non-white… both groups show this significacnt
relationship. Both are significant, however those who are not white show
a slightly greater inverse relationship than those who are white. This
could be interpreted as non-white people may be more influenced by peers
in turn relating to greater relational aggression. Again, in the plots,
the line is visually steeper for non-white participants than white
participants. Meaning the relationship was stronger in that group.

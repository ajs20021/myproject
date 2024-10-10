my project and dataset
================
Jack Sutton
2024-10-10

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
```

# ignore other variables

``` r
#new_dataset <- dataset %>%
 # select(variable 1, variable 2, variable 3)
```

# possibly recoding variables

May need to recode from numeric to characters(in case of nominal
entries), or from numeric to numeric (to swap reverse coded entries)

\#creating composites Taken from Lab3, either average across rows or get
sum of entries.

``` r
#Change lab3 directory to my dataset, change RSE vars to my own variables.

#lab3data <- lab3data %>%
 # mutate(RSE = rowMeans(cbind(RSE1, RSE2_R, RSE3, RSE4, RSE5_R, RSE6_R, RSE7, RSE8_R, RSE9_R, RSE10)))

#This function used to get the sum of a row
#lab3data <- lab3data %>%
  #mutate(SWL = rowSums(cbind(SWL1, SWL2, SWL3, SWL4, SWL5)))
```

\#normality check of the variables\*\* if the p-value exceeds .05 = not
normal remember to change lab specific directories to the current data
set…

``` r
#Use the describeBy() function to get skewness and kurtosis by group

#?describeBy()

#describeBy(Performance ~ Group, data = lab4data)

#Use the group by function to get shapiro test results by group
#lab4data %>%
#  group_by(Group) %>%
 # summarize(W = shapiro.test(Performance)$statistic, p_value = shapiro.test(Performance)$p.value)
```

\#equal variance check for DV, RQ type 1 difference –\> do mena nd women
have diff self-esteem RQ type 1 outcome –\> what can predict
self-esteem? GPA?

If I have an outcome question I don’t need to check equal variance.

``` r
#leveneTest(Performance~Group, lab4data)

#MANOVA(lab4data, dv = "Performance", between = "Group")

#What if you want to test equal variance between 2 groups specifically? 

#lab4dataConG1<-lab4data %>%
 # filter(Group == "Control" | Group == "G1")

#leveneTest(Performance~Group, lab4dataConG1)
```

\#If any assumptions are violated I would transform the data according
to the formulas/codes covered in lab 4. Mursal and I are gonna visualize
each other’s data to visually check for normality after transforming as
well. We both had some problems with lab 4 so we figured we’d help each
other get our code for this section right.

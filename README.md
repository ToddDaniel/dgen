# dgen

dgen makes classroom datasets whose statistics come out to the numbers you
choose. Type in the mean, standard deviation, correlation or counts you want,
and dgen gives you data that produce exactly those values when your students
analyze them.

It is written for instructors who do not program. Each function is named for
the test you are teaching, and each one shows you the result your students
will get before you hand out the data.

## Installation

Until dgen is on CRAN, install it from GitHub. In R or RStudio, run these two
lines once:

```r
install.packages("remotes")
remotes::install_github("ToddDaniel/dgen")
```

Then, at the start of each session:

```r
library(dgen)
```

## How it works

Every function works the same way:

1. You type the numbers you want.
2. dgen prints a short summary: your targets beside what the data actually
   have, the matching test, and an effect size.
3. The data come back as an ordinary table (a data frame). Give it a name
   with `<-` so you can use it afterwards.

## One variable with an exact mean and standard deviation

Thirty exam scores with a mean of 75.4 and a standard deviation of 8.2:

```r
scores <- dgen(n = 30, m = 75.4, sd = 8.2, decimals = 1,
               var_name = "score", seed = 1)
```

```
One variable with an exact mean and SD

       n Mean (target) Mean (data) SD (target) SD (data) Smallest Largest
score 30         75.40       75.40        8.20      8.20     55.0    88.8

30 rows, 1 column (score). Rounded to 1 decimal place.
See this again with dgen_results(your_data).
```


The data are in `scores`:

```r
head(scores, 4)
```

```
  score
1  69.1
2  76.3
3  67.3
4  88.8
```


`seed = 1` makes the result repeatable: the same seed always gives the same
data. Leave it out and you get new data every time, with the same mean and
standard deviation.

## t tests

Two sections of a course, one online and one in the classroom:

```r
exam <- t_indep_gen(n1 = 25, n2 = 25, m = c(72, 78), sd = c(9, 10),
                    labels = c("Online", "Classroom"),
                    var_name = "score", decimals = 0, seed = 1)
```

```
Independent-samples t test: score by group

           n Mean (target) Mean (data) SD (target) SD (data) Smallest Largest
Online    25          72.0        72.0         9.0       9.0       49      86
Classroom 25          78.0        78.0        10.0      10.0       60      98

Difference in means (Classroom - Online) = 6.0
t(48) = 2.22, p = .031   (equal variances)
t(47.5) = 2.22, p = .031   (Welch, unequal variances)
95% CI for the difference: 0.6 to 11.4
Cohen's d = 0.63

50 rows, 2 columns (group, score). Whole numbers.
See this again with dgen_results(your_data).
```


`t_one_gen()` does the same for a one-sample t test and `t_paired_gen()` for
before-and-after measurements on the same people.

## ANOVA

Sales in three regions:

```r
sales <- anova_gen(n = 12, m = c(70, 75, 82), sd = c(8, 9, 8),
                   labels = c("North", "South", "West"),
                   var_name = "sales", decimals = 0, seed = 1)
```

```
One-way ANOVA: sales by group

       n Mean (target) Mean (data) SD (target) SD (data) Smallest Largest
North 12          70.0        70.0         8.0       8.0       59      83
South 12          75.0        75.0         9.0       9.0       57      84
West  12          82.0        82.0         8.0       8.0       71      94

                    SS df     MS    F    p
Between groups  872.00  2 436.00 6.29 .005
Within groups  2286.00 33  69.27
Total          3158.00 35

Eta squared = 0.28   Omega squared = 0.23

36 rows, 2 columns (group, sales). Whole numbers.
See this again with dgen_results(your_data).
```


## Correlation and regression

Hours studied and exam score, correlated .55:

```r
study <- cor_gen(n = 40, m = c(6.5, 78), sd = c(2.1, 9.5), r = 0.55,
                 var_name = c("hours", "exam"), decimals = 1, seed = 1)
```

```
Correlation between hours and exam

       n Mean (target) Mean (data) SD (target) SD (data) Smallest Largest
hours 40          6.50        6.50        2.10      2.10      1.0    10.1
exam  40         78.00       78.00        9.50      9.50     58.3   101.2

               r (target) r (data) r squared    t df      p
hours and exam       0.55     0.55      0.31 4.09 38 < .001

40 rows, 2 columns (hours, exam). Rounded to 1 decimal place.
See this again with dgen_results(your_data).
```


`reg_gen()` builds data for simple and multiple regression in the same way:
you say how strongly each predictor is related to the outcome.

## Proportions and chi-square

Eighty viewers chose between two endings. Is one preferred?

```r
endings <- chisq_gen(counts = c(Happy = 56, Tragic = 24),
                     expected = c(0.5, 0.5), var_name = "ending", seed = 1)
```

```
Chi-square goodness-of-fit test: ending

       Observed Expected proportion Expected count
Happy        56               0.500           40.0
Tragic       24               0.500           40.0

Chi-square(1) = 12.80, p < .001
Cohen's w = 0.40

80 rows, 1 column (ending).
See this again with dgen_results(your_data).
```


`prop_one_gen()` and `prop_two_gen()` cover tests of one and two proportions,
and `chisq_gen()` also takes a table of counts for a test of independence.

## Saving the data and seeing the results again

To save the data as a file you can open in Excel or give to students, add
`export_csv`:

```r
scores <- dgen(n = 30, m = 75.4, sd = 8.2, export_csv = "exam_scores.csv")
```

`export_csv = TRUE` names the file after the function. dgen never replaces a
file that already exists; it adds a number to the new name instead.

To see a summary again later:

```r
dgen_results(scores)
```

For the Excel Data Analysis ToolPak, which wants one column per group, add
`layout = "wide"` to `t_indep_gen()` or `anova_gen()`.

## What "exact" means

The mean and standard deviation of the data, rounded to one more decimal
place than the data themselves, equal the numbers you typed. Correlations
match to 2 decimal places, and counts match exactly. Standard deviations are
sample standard deviations (the `n - 1` kind; `STDEV.S` in Excel).

Two things to know:

- The data are bell-shaped, with no minimum or maximum. Check the Smallest
  and Largest columns of the summary for values that make no sense, such as
  a negative price.
- Some requests are impossible: five whole numbers cannot average 75.5. dgen
  tells you so and names the nearest values that work. Once in a while a
  very demanding request takes several seconds before dgen reports that it
  cannot be met.

## The functions

| To teach | Use |
|---|---|
| Descriptive statistics; a variable with a given mean and SD | `dgen()` |
| One-sample t test | `t_one_gen()` |
| Independent-samples t test | `t_indep_gen()` |
| Paired t test | `t_paired_gen()` |
| One-way ANOVA | `anova_gen()` |
| Correlation; simple regression | `cor_gen()` |
| Simple and multiple regression | `reg_gen()` |
| One-proportion z test | `prop_one_gen()` |
| Two-proportion z test | `prop_two_gen()` |
| Chi-square: goodness of fit, independence | `chisq_gen()` |
| See a summary again | `dgen_results()` |

Each function has a help page with examples: type `?t_indep_gen`, for
instance. `vignette("getting-started", package = "dgen")` opens a longer
introduction.


## Origins

dgen began as a rescue. In 2012, Dr. Brendan Morse of Bridgewater State
University published MorseGen, a small R package with two elegant functions:
`stat.gen()`, which builds a variable with an exact mean and standard
deviation, and `cor.gen()`, which builds two variables with an exact
correlation. I found it so useful for creating classroom datasets that I kept
coming back to it. After Dr. Morse's death in 2018 the package went
unmaintained, and in 2022 CRAN archived it, so it can no longer be installed
the usual way.

dgen restores that idea and extends it. Its engine generalizes Morse's method
to any number of variables with an exact correlation matrix, keeps every
target exact in the rounded data students actually see, and wraps it in
functions named for the tests instructors teach, from t tests and ANOVA to
chi-square and regression. The code is new; the inspiration is his.


## License

MIT


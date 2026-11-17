# dgen 1.0.0

First release.

dgen generates classroom datasets whose summary statistics match the targets
you choose. Means, standard deviations, correlations and counts hold exactly
in the rounded data students receive.

## Functions

Each function is named for the test being taught, prints a short summary
(targets beside what the data actually have, the matching test, and an
effect size), and returns the data as a plain data frame.

- `dgen()`: one variable, or several independent variables, with an exact
  mean and standard deviation.
- `t_one_gen()`, `t_indep_gen()`, `t_paired_gen()`: one-sample,
  independent-samples and paired t tests. Each accepts Cohen's d in place of
  a mean.
- `anova_gen()`: one-way analysis of variance.
- `cor_gen()`: two variables with an exact correlation, or several variables
  from a correlation table.
- `reg_gen()`: simple and multiple linear regression.
- `prop_one_gen()`, `prop_two_gen()`: one- and two-proportion z tests.
- `chisq_gen()`: chi-square tests of goodness of fit and independence.
- `dgen_results()`: shows a data set's summary again.

## Shared settings

- `decimals` sets the rounding of the data; `seed` makes data reproducible.
- `export_csv` saves the data to a CSV file and never replaces an existing
  file.
- `show_results = FALSE` turns the printed summary off.
- `layout = "wide"` in `t_indep_gen()` and `anova_gen()` gives one column
  per group, for the Excel Data Analysis ToolPak.
- Differences are always the first group (or measurement) minus the second
  in `t_indep_gen()`, `t_paired_gen()` and `prop_two_gen()`, and every
  summary names the direction.

## What "exact" means

The mean and standard deviation of the returned data, rounded to
`decimals + 1` places, equal the targets. Correlations match to 2 decimal
places. Counts match exactly. Requests that cannot be met are refused with a
plain-language message that names the nearest values that can.

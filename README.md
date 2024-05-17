# IdentifyASE

IdentifyASE is an R package designed to process daily patient data to define acute sepsis episodes and discharge information. It includes steps for data formatting, selecting sub-cohorts, creating transfer indicators, and applying transformations to identify sepsis.

## Installation

To install the IdentifyASE package from GitHub, you need to use the `devtools` package. Make sure you have a personal access token if the repository is private.

### Step-by-Step Installation

1. **Install and load `devtools`**

    ```r
    install.packages("devtools")
    library(devtools)
    ```

2. **Install the package from GitHub**

    ```r
    # Replace 'yourusername' and 'yourpackagename' with your GitHub username and repository name
    # Replace 'your_personal_access_token' with your GitHub personal access token
    install_github("yourusername/yourpackagename", auth_token = "your_personal_access_token")
    ```

3. **Load the package**

    ```r
    library(identifyASE)
    ```

## Dependencies

This package depends on the following R packages:

- `dplyr`
- `purrr`
- `future`
- `furrr`
- `roxygen2`
- `devtools`

Make sure these packages are installed and loaded in your R environment.

```r
install.packages(c("dplyr", "purrr", "future", "furrr", "roxygen2", "devtools"))
```

## Usage

Here is an example of how to use the main functions in this package:

```{r}
daily_data <- data.frame(
  unique_pt_id = c(1, 1, 1, 2, 2, 2),
  seqnum = c(1, 1, 1, 1, 1, 1),
  day = c(0, 1, 2, 0, 1, 2),
  death = c(0, 0, 0, 0, 1, 0),
  ALL_DAYS = c(3, 3, 3, 3, 3, 3),
  bcx_daily = c(1, 0, 0, 0, 1, 0),
  vasop_daily = c(0, 1, 0, 0, 0, 1),
  imv_daily = c(0, 0, 1, 0, 1, 0),
  lact_daily_hi = c(1.5, 2.5, 1.8, 1.9, 2.1, 3.0),
  tbili_daily_hi = c(30, 35, 40, 32, 36, 38),
  tbili_baseline = c(20, 20, 20, 25, 25, 25),
  creat_daily_hi = c(40, 45, 50, 35, 60, 55),
  creat_baseline = c(20, 20, 20, 25, 25, 25),
  plt_daily_lo = c(150, 80, 90, 110, 70, 50),
  plt_baseline = c(200, 200, 200, 180, 180, 180),
  ELX_All_33 = c(0, 0, 0, 0, 0, 0),
  new_abx_start = c(0, 1, 0, 0, 1, 0),
  abx_daily = c(0, 1, 1, 0, 1, 1)
)

transferout_id <- c(1)
cohort_id <- list(c(1, 2), c(1, 1))

# Define Acute Sepsis Episodes and Discharge Information
result <- define_ase_disch(daily_data = daily_data, transferout_id = transferout_id, cohort_id = cohort_id)
print(result)
```

## License

This package is licensed under the GPL-3 License. See the LICENSE file for details.


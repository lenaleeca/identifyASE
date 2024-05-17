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

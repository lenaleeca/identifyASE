# IdentifyASE

IdentifyASE is an R package designed to process daily patient data to identify adult sepsis events (ASE) in hospitalized patients using the CDC's ASE Toolkit. 
The `define_ase` function sits at the top of the hierarchical structure of the package and is the main function users will interact with. This function initiates the analysis by sequentially executing key functions to check the ASE criteria. 
The first criterion, presumed infection, is checked by identifying blood culture events through `slice_bcx_data` and qualifying antimicrobial medications through `qualifying_abx_duration`. 
The second criterion, acute organ dysfunction, is evaluated using `add_aod_daily`. These key functions are supported by helper functions at a lower level, which handle specific data transformations and calculations.

The package contains preset parameters aligning to the ASE toolkit's criteria, such as blood culture window period, lab thresholds for organ dysfunction, and required consecutive days of antimicrobial treatments.  
Researchers can adjust these parameters, select sub-groups, and indicate patients who were transferred out to an acute hospital to meet their research needs.

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
    # Replace 'your_personal_access_token' with your GitHub personal access token
    install_github("lenaleeca/identifyASE", auth_token = "your_personal_access_token")
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
- `devtools`

Make sure these packages are installed and loaded in your R environment.

```r
install.packages(c("dplyr", "purrr", "future", "furrr", "devtools"))
```
```{r}
library(dplyr)
library(purrr)
library(future)
library(furrr)
library(devtools)
```

## Usage

Here is an example of how to identify ASE cases using the main function `define_ase`:

```{r}
daily_data <- data.frame(
  unique_pt_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4),
  seqnum = c(12602, 12602, 12602, 18613, 18613, 18613, 54928, 54928, 54928, 
             27201, 27201, 27201, 27201, 27201),
  day = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 3, 4),
  death = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  ALL_DAYS = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5),
  bcx_daily = c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
  vasop_daily = c(0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0),
  imv_daily = c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0),
  lact_daily_hi = c(1.5, 2.5, 1.8, 1.9, 2.1, 3.0, 1.5, 2.5, 1.8, 
                    1.5, 1.9, 2.1, 2.1, 3.0),
  tbili_daily_hi = c(30, 35, 40, 32, 36, 38, 30, 35, 40, 30, 32, 35, 36, 50),
  tbili_daily_lo = c(30, 35, 40, 32, 36, 38, 30, 35, 40, 30, 32, 35, 36, 50),
  tbili_baseline = c(20, 20, 20, 25, 25, 25, 20, 20, 20, 25, 25, 25, 25, 25),
  creat_daily_hi = c(40, 45, 50, 35, 60, 55, 40, 45, 50, 35, 35, 45, 60, 55),
  creat_daily_lo = c(40, 45, 50, 35, 60, 55, 40, 45, 50, 35, 35, 45, 60, 55),
  creat_baseline = c(20, 20, 20, 25, 25, 25, 20, 20, 20, 25, 25, 25, 25, 25),
  plt_daily_hi = c(150, 80, 90, 110, 70, 50, 150, 80, 90, 150, 140, 70, 60, 50),
  plt_daily_lo = c(150, 80, 90, 110, 70, 50, 150, 80, 90, 150, 140, 70, 60, 50),
  plt_baseline = c(200, 200, 200, 180, 180, 180, 200, 200, 200, 
                   180, 180, 180, 180, 180),
  esrd_icd = c(0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0,0,0),
  new_abx_start = c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0),
  abx_daily = c(0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
)

transferout_id <- c(12602,54928,27201)


# Idenfify Adult Sepsis Events
result <- define_ase(daily_data = daily_data, transferout_id = transferout_id)
```
## Authorship

**Ambiguities and Challenges in Translating Clinical Criteria into Code: Insights from Coding the Adult Sepsis Event Toolkit**  

Na Li<sup>1,2,3</sup>, April Nguyen<sup>2</sup>, Rachel Nguyen<sup>2</sup>, Kednapa Thavorn<sup>5,6</sup>, Jennifer Ziegler<sup>7</sup>, Peter Dodek<sup>8</sup>, Allan Garland<sup>6</sup>  

<sup>1</sup> Department of Community Health Sciences, Cumming School of Medicine, University of Calgary, Calgary, Alberta, Canada  
<sup>2</sup> Centre for Health Informatics, University of Calgary, Alberta, Canada  
<sup>3</sup> Department of Computing and Software and Department of Medicine, McMaster University, Hamilton, Ontario, Canada  
<sup>4</sup> Department of Medicine, University of Michigan, Ann Arbor, MI, USA  
<sup>5</sup> Ottawa Hospital Research Institute, Ottawa, Ontario, Canada  
<sup>6</sup> School of Epidemiology and Public Health, University of Ottawa, Ottawa, Ontario, Canada  
<sup>7</sup> Departments of Medicine and Community Health Sciences, University of Manitoba, Winnipeg, Manitoba, Canada  
<sup>8</sup> Division of Critical Care Medicine and Center for Advancing Health Outcomes, St. Paul's Hospital and University of British Columbia, Vancouver, BC, Canada

## License

This package is licensed under the MIT License. See the LICENSE file for details.


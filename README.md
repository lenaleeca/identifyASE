# IdentifyASE

IdentifyASE is an R package designed to process daily patient data to identify adult sepsis events (ASE) in hospitalized patients using the CDC's ASE Toolkit. 
The `define_ase` function sits at the top of the hierarchical structure of the package and is the main function users will interact with. This function initiates the analysis by sequentially executing key functions to check the ASE criteria. 
The first criterion, presumed infection, is checked by identifying blood culture events through `slice_bcx_data` and qualifying antimicrobial medications through `qualifying_abx_duration`. 
The second criterion, acute organ dysfunction, is evaluated using `add_aod_daily`. These key functions are supported by helper functions at a lower level, which handle specific data transformations and calculations.

The package contains preset parameters aligning to the ASE toolkit's criteria, such as blood culture window period, lab thresholds for organ dysfunction, and required consecutive days of antimicrobial treatments.  
Researchers can adjust these parameters, select sub-groups, and indicate patients who were transferred out to an acute hospital to meet their research needs.

## Installation

To install the IdentifyASE package from GitHub, you need to use the `devtools` package. 

### Step-by-Step Installation

If the repo is private: 

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

If the repo is public:

1. **Install and load `devtools`**

    ```r
    install.packages("devtools")
    library(devtools)
    ```

2. **Install the package from GitHub**

    ```r
    install_github("lenaleeca/identifyASE")
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
<<<<<<< HEAD
For more detailed information on preparing input data and other package usage, please refer to the vignette.

## Citation

If you find IdentifyASE useful for your research or development, please cite the following source:

```bibtex
@article{identifyASE,
  title = {Ambiguities and Challenges in Translating Clinical Criteria into Code: Insights from Coding the Adult Sepsis Event Toolkit},
  author = {Li, Na and Nguyen, April and Nguyen, Rachel and Thavorn, Kednapa and Ziegler, Jennifer and Dodek, Peter and Garland, Allan},
  journal = {TBD},  
  year = {2025},
  volume = {TBD},   
  number = {TBD},   
  pages = {TBD}   
}
```

## License

This package is licensed under the MIT License. See the LICENSE file for details.


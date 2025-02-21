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
    install_github("lenaleeca/IdentifyASE")
    ```

3. **Load the package**

    ```r
    library(IdentifyASE)
    ```

If the repo is public:

1. **Install and load `devtools`**

    ```r
    install.packages("devtools")
    library(devtools)
    ```

2. **Install the package from GitHub**

    ```r
    install_github("lenaleeca/IdentifyASE")
    ```

3. **Load the package**

    ```r
    library(IdentifyASE)
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
# Get the path to the example data file inside the package
data_path <- system.file("extdata", "example_input_data", "daily_data.csv", package = "IdentifyASE")

# Read the data 
daily_data <- read.csv(data_path)

transferout_id <- c(12602,54928,27201)


# Idenfify Adult Sepsis Events
result <- define_ase(daily_data = daily_data, transferout_id = transferout_id)
```

For more detailed information on preparing input data and other package usage, please refer to the vignette.

You can view the vignette locally after installing the package by running:

```r
vignette("vignette_ase")
```

or 

```r
browseVignettes("IdentifyASE")
```

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


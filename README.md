# trinaryMaps

## Overview
The `trinaryMaps` package is a collection of functions designed to create, analyze, and visualize trinary maps. A trinary map is a categorization of a continuous raster species range estimates where each pixel has one of three possible values: 0 (definitely unoccupied habitat), 1 (marginal, occasional, or uncertain occupied habitat), or 2 (likely occupied habitat).

## Installation
To install the Trinary Maps package, use the following command:

```r
remotes:install_github("gepinillab/trinaryMaps") # Change to cmerow when published.
```

Then, load the package using:
```r
library(trinaryMaps)
```

## Functions
The Trinary Maps package includes several functions to create and analyze trinary maps. Some of the key functions include:
- `trinaryMapWorkflow()`: This function takes in the species presence and background data, along with a model prediction, and generates trinary maps. It also calculates the information for ROC curves.
- `trinaryROCRoots()`: Fit a smoothed ROC curve, find bounds for threshold and report 
 partial AUC statistics. Used internally also in `trinaryMapWorkflow()`.
- `trinaryROCPlot()`: Plot trinary ROC curve and, optionally, save the output as pdf file.
- `trinaryMap()`: Function to generate trinary map (spatRaster) based on thresholds calculated in `trinaryROCRoots()`. Used internally also in `trinaryMapWorkflow()`.

## Examples
For more information and examples, refer to the package vignette:
```r
browseVignettes("trinarymaps")
```

## Troubleshooting
For any issues or questions, please refer to the package documentation and vignette.

## License
The trinaryMaps package is licensed under the MIT License. See LICENSE.md for more information.

## Contributing
We welcome contributions from users and developers. Please submit pull requests or open issues on GitHub.

## Acknowledgments
This project was developed with support from NSF Grant 1046328 and NASA grant 80NSSC18K0435.
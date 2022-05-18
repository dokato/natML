# natML

Machine learning on neurons made easy. This package serves as an interface between the `nat` package for geometric neurons analysis and other R packages for machine learning. It let's you extract the features from your neurons seamlessly, regardless whether you want to access predefined columns, or calculate new features. It comes with extra functions returning some of the most popular scalar features for neurons classification.

## Installation

```r
# install.packages("remotes")
remotes::install_github("dokato/natML")
```

## Example

```r
library(nat)
library(natML)

# named or unnamed list of features
features_list <- list("upstream", "downstream", "voxels", "cellBodyFiber", get_cable_length)

kc_features <- extract_features(
  kc_train,
  features_list = features_list,
  to_numeric = TRUE,
  normalise = "zscore"
)

clusters <- kmeans(kc_features, 3)
```


# AgFun
Functions for agricultural data analysis. 

## Cate-Nelson
`CateNelson.R`. Graphical function used to classify a continuous index and a continuous response to a binary perspective. I namely used it in the paper [Plant ionome diagnosis using sound balances: case study with mango (Mangifera Indica)](http://journal.frontiersin.org/article/10.3389/fpls.2013.00449/full).

## CodaDendrogram 2
`codadend2.R`. Function similar to compositions::CoDaDendrogram, but with extended capabilities, like plotting densities or boxplots per group over the horizontal bars.

## Compositional confidence intervals
`compci.R`. A function that computes confidence intervals on compositional data based on Monte-Carlo simulations.

## Diagnosis and recommandation integrated system
`DRIS.R`. A function that conpute DRIS indices with several methods to assess the leaf nutritionnal status of plants.

## Isometric log-ratios definitions
`ilrDefinition.R`. Using a sequential binary partition, this function creates a vector of strings representing balances of components computed as isometric log-ratios in the form of, for example, [A,B | C,D] where components A and B is balanced with components C and D.

## Replace zeroes by NAs in isometric log-ratio matrices
`ilrNA.R`. When computing ilrs through BDL compositional values, the compositions::ilr function sets the ilr to 0. However, often, BDL values origins from missing data parsed through the compositions::acomp function. Where there is at least one NA in a balance in the original data set, this function sets the ilr value to be also NA. This function is inefficient and can sometimes be a long process.

## Check if a coordinate is inside a polygon
`is.inpoly.R`. Does what is says it does.

## Computes mahalanobis distance from an itterated reference
`sign1_ref.R`. For details check the paper [Plant ionome diagnosis using sound balances: case study with mango (Mangifera Indica)](http://journal.frontiersin.org/article/10.3389/fpls.2013.00449/full).

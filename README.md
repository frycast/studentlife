### Install from GitHub with
```r
# install.packages("devtools")
devtools::install_github("frycast/studentlife")
```

You will also need to download and extract the StudentLife data, currently available [here](https://studentlife.cs.dartmouth.edu/dataset.html). On windows you can use 7zip twice (once for the .tar and once for the .bz2). On Linux there are currently unresolved issues caused by question mark in the filename "Do Campbell's Jokes Suck?"

Extraction produces two folders: 'rawaccfeat' and 'dataset'. We will use the path to the 'dataset' folder.

```r
p <- "D:/Datasets/studentlife/dataset"
studs <- studentlife::read_from_SL(location = p)
```





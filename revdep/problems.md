# idiogramFISH

<details>

* Version: 2.0.13
* GitHub: NA
* Source code: https://github.com/cran/idiogramFISH
* Date/Publication: 2023-08-22 16:50:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "idiogramFISH")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R     1.5Mb
        doc   2.0Mb
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘index.Rmd’
      ...
    > if (requireNamespace("RCurl", quietly = TRUE)) {
    +     v <- sub("Version: ", "", readLines("../DESCRIPTION")[3])
    +     pkg <- "idiogramFISH"
    +     l .... [TRUNCATED] 
    Warning in file(con, "r") :
      cannot open file '../DESCRIPTION': No such file or directory
    
      When sourcing ‘index.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘AVignette.Rmd’ using ‘UTF-8’... OK
      ‘index.Rmd’ using ‘UTF-8’... failed
    ```

# iheatmapr

<details>

* Version: 0.7.1
* GitHub: https://github.com/ropensci/iheatmapr
* Source code: https://github.com/cran/iheatmapr
* Date/Publication: 2024-01-25 23:50:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "iheatmapr")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R             1.5Mb
        htmlwidgets   3.5Mb
    ```

# polmineR

<details>

* Version: 0.8.9
* GitHub: https://github.com/PolMine/polmineR
* Source code: https://github.com/cran/polmineR
* Date/Publication: 2023-10-29 21:50:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "polmineR")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R         2.0Mb
        extdata   1.9Mb
    ```


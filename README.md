# realscrape
rvest wrapper for scraping sales data from AU House Prices website

First install realscrape
```r
devtools::install_github("frycast/realscrape")
library(realscrape)
```

Scrape just one ad page (P) from one board (B) for each suburb (quick)
```r
data <- scrape_sbc( STA3LM_suburbs_by_council, B = 1, P = 1 )
```

Take a look at the result
```r
data
```
```
# A tibble: 81 x 18
   address   price type    bed  bath   car   CBD date       agency images  land  med_h  med_u school station suburb postcode
   <chr>     <int> <chr> <int> <int> <dbl> <dbl> <date>     <chr>   <int> <dbl>  <int>  <int>  <dbl>   <dbl> <chr>  <chr>   
 1 1/9 Gl~      NA unit      2     1     2  27.6 2019-08-12 Harco~      8   386 695250 516000   0.77    1.28 Baysw~ 3153    
 2 19 Gar~      NA house     3     1     2  29.9 2019-08-19 voglw~      7   685 736000 552000   0.83    1.82 Croyd~ 3136    
 3 14 Bow~  803000 house     4     2     2  27.5 2019-08-08 Carte~     11   644 840000 860000   0.54    2.84 Croyd~ 3136    
 4 15 Kin~  765200 house     3     1     0  29.0 2019-08-09 Rosie~     12  1077 792500 599975  NA       2.81 Croyd~ 3136    
 5 22 Aza~  682000 house     3     2     2  28.0 2019-08-15 Stock~     10   594 727500     NA   0.37    2.44 Croyd~ 3136    
 6 63 Dic~ 1080000 house     3     2     2  25.2 2019-08-17 Wooda~     14  1067 890000 600000   0.94    0.9  Heath~ 3135    
 7 29 Sno~  632000 house     3     2     3  31.0 2019-07-19 Ray W~     11   511 742000     NA   0.86    4.16 Kilsy~ 3137    
 8 209/42~  660000 apar~     3     2     2  22.7 2019-08-19 Carte~      3    -1 823000 540000   0.99    0.84 Ringw~ 3134    
 9 25 Hum~  791150 house     3     1     2  26.0 2019-08-19 Fletc~      8    -1 816250 595000   0.63    1.33 Ringw~ 3135    
10 649 Ri~      NA house     3     1     1  24.1 2019-08-07 Barry~      7  5017 879500     NA  NA       3.44 Ringw~ 3134    
# ... with 71 more rows, and 1 more variable: council <chr>
```

Drop all rows containing NA
```r
data_clean <- tidyr::drop_na( data )
nrow(data); nrow(data_clean)
```

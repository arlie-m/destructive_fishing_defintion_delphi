Round 1 - Gear analysis
================
Arlie McCarthy and Josh Brian
2023-10-31

## Load data

``` r
surveydata <- read_excel(here::here("data", "R1_gear_results.xlsx"), sheet = "Sheet1")
```

    ## Warning: Expecting numeric in F77 / R77C6: got a date

``` r
gear_groups <- read_excel(here::here("data", "2021_04_fishing_gear_matrix_REV.xlsx"), sheet = "gear_categories")
```

## Generate figures showing

General figure for the manuscript showing the groups of gear types and
the classifications from survey respondents. Groups are based on larger
categories.

``` r
long_data <- pivot_longer(surveydata %>% select("name", "Purse seines":"Blast/explosives/dynamite"), cols = "Purse seines":"Blast/explosives/dynamite", values_to = "response", names_to = "Gear", names_repair = "check_unique") %>% 
  join(gear_groups) %>% 
  drop_na() %>% 
  dplyr::group_by(Gear) %>% 
  dplyr::mutate(gear_median = median(response)) %>% 
  dplyr::group_by(Category) %>% 
  dplyr::mutate(cat_median = median(response))
```

    ## Joining by: Gear

``` r
cat_levels <- long_data %>% 
  dplyr::group_by(Category) %>% 
  dplyr::summarise(mean(cat_median))
cat_levels
```

    ## # A tibble: 11 × 2
    ##    Category                     `mean(cat_median)`
    ##    <chr>                                     <dbl>
    ##  1 Chemical and Blast                            5
    ##  2 Dredges                                       4
    ##  3 Falling gear                                  2
    ##  4 Gillnets and entangling nets                  4
    ##  5 Hooks and lines                               2
    ##  6 Lift nets                                     2
    ##  7 Miscellaneous                                 2
    ##  8 Seine nets                                    3
    ##  9 Surrounding nets                              3
    ## 10 Traps                                         2
    ## 11 Trawls                                        4

``` r
long_data$Category <- factor(long_data$Category, levels = c("Chemical and Blast", "Dredges", "Gillnets and entangling nets", "Trawls", "Seine nets", "Surrounding nets", "Falling gear", "Hooks and lines", "Lift nets", "Traps", "Miscellaneous"))
```

Now that the data is in the correct format, we can visualise it as a
violin plot.

``` r
gear_plot <- ggplot(long_data, aes(x = short_name, y = response, fill = gear_median)) + 
  geom_violin(alpha = 0.7, colour = "darkgrey") +
  stat_summary(fun = "median", colour = "black", size = 2, geom = "point") +
  scale_fill_viridis_c(name = "Median response", alpha = 0.7) +
  scale_color_viridis_c() +
  facet_wrap(~Category, scales = "free_x", shrink = FALSE) +
  theme_cowplot() +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(axis.text.x = element_text(size = 7, angle = 30),
        legend.position = c(0.78, 0.15),
        axis.text.y = element_text(size = 7),
        strip.text.y = element_text(size = 9,
                                    angle = 0),
        axis.title = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 9))+
  labs(x = "Fishing gear or practice", y = "Potential destructiveness")
gear_plot
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Round1-Gear_analysis_pca_files/figure-gfm/gear%20plot-1.png)<!-- -->
save the plot

``` r
save_plot(here::here("figures", "gear_plot.png"), gear_plot, base_width = 180, base_height = 125, units = "mm")
```

Create table showing the summary statistics for each gear.

``` r
summary_data <- long_data %>% 
  drop_na() %>% 
  dplyr::group_by(Category, Gear) %>% 
  dplyr::summarise(count = length(response),
            mean = mean(response), 
            stdev = sd(response),
            median = median(response)) %>% 
  dplyr::select(Category, Gear, median, everything())
```

    ## `summarise()` has grouped output by 'Category'. You can override using the
    ## `.groups` argument.

``` r
summary_data
```

    ## # A tibble: 48 × 6
    ## # Groups:   Category [11]
    ##    Category                     Gear                    median count  mean stdev
    ##    <fct>                        <chr>                    <dbl> <int> <dbl> <dbl>
    ##  1 Chemical and Blast           Blast/explosives/dynam…      5    69  4.83 0.766
    ##  2 Chemical and Blast           Chemicals/poison/synth…      5    68  4.79 0.682
    ##  3 Dredges                      Hand dredges                 3    55  3.04 1.36 
    ##  4 Dredges                      Mechanized dredges           5    59  4.37 0.908
    ##  5 Dredges                      Towed dredges                5    66  4.33 0.982
    ##  6 Gillnets and entangling nets Combined gillnets-tram…      4    52  3.23 1.44 
    ##  7 Gillnets and entangling nets Drift gillnets               4    71  3.79 1.37 
    ##  8 Gillnets and entangling nets Encircling gillnets          3    65  3.23 1.38 
    ##  9 Gillnets and entangling nets Fixed gillnets (on sta…      3    63  3.19 1.28 
    ## 10 Gillnets and entangling nets Set gillnets (anchored)      3    70  3.26 1.27 
    ## # ℹ 38 more rows

# Run Principal Components Analysis

We first run a standard PCA analysis.

``` r
#gearpca <- prcomp(surveydata[ ,c(8:55)], center=TRUE, scale. = TRUE)
```

Note that this doesn’t work because there are missing values. To correct
for this we need to impute data. We ese imputePCA function from missMDA
package as it is the best and most defendable method (see Dray, S., &
Josse, J. (2015). Principal component analysis with missing values:a
comparative survey of methods. Plant Ecology, 216(5), 657-667.)

``` r
#just select the columns of interest
datatoimpute <- select(surveydata, "Purse seines":"Blast/explosives/dynamite")

#impute the missing values 

imputeddata <- imputePCA(datatoimpute, ncp=2)
imputedmatrix <- as.data.frame(imputeddata$completeObs)

#Extract possible correlating variables

variables <- select(surveydata, homecontinent:oceanregion)

#combine the variables and imputed data

pcadata <- bind_cols(variables, imputedmatrix)
```

Now we can actually run the PCA

``` r
gearpca <- prcomp(pcadata[ ,c(7:54)], center=TRUE, scale. = TRUE)
summary(gearpca)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     4.5724 2.8290 1.78200 1.30890 1.21727 1.12545 1.07228
    ## Proportion of Variance 0.4356 0.1667 0.06616 0.03569 0.03087 0.02639 0.02395
    ## Cumulative Proportion  0.4356 0.6023 0.66845 0.70414 0.73501 0.76140 0.78536
    ##                            PC8     PC9    PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     1.04970 0.94347 0.87062 0.86116 0.77702 0.73031 0.69344
    ## Proportion of Variance 0.02296 0.01854 0.01579 0.01545 0.01258 0.01111 0.01002
    ## Cumulative Proportion  0.80831 0.82686 0.84265 0.85810 0.87068 0.88179 0.89180
    ##                           PC15    PC16    PC17   PC18    PC19    PC20    PC21
    ## Standard deviation     0.68864 0.65816 0.61108 0.5962 0.58119 0.55715 0.54293
    ## Proportion of Variance 0.00988 0.00902 0.00778 0.0074 0.00704 0.00647 0.00614
    ## Cumulative Proportion  0.90168 0.91071 0.91849 0.9259 0.93293 0.93940 0.94554
    ##                          PC22    PC23    PC24    PC25    PC26   PC27    PC28
    ## Standard deviation     0.5230 0.49427 0.49034 0.43322 0.41386 0.4096 0.38437
    ## Proportion of Variance 0.0057 0.00509 0.00501 0.00391 0.00357 0.0035 0.00308
    ## Cumulative Proportion  0.9512 0.95633 0.96133 0.96524 0.96881 0.9723 0.97539
    ##                           PC29    PC30    PC31    PC32    PC33    PC34    PC35
    ## Standard deviation     0.37004 0.35888 0.34487 0.32576 0.31241 0.29935 0.27477
    ## Proportion of Variance 0.00285 0.00268 0.00248 0.00221 0.00203 0.00187 0.00157
    ## Cumulative Proportion  0.97824 0.98092 0.98340 0.98561 0.98764 0.98951 0.99108
    ##                           PC36    PC37    PC38    PC39   PC40    PC41   PC42
    ## Standard deviation     0.26449 0.25470 0.22660 0.22041 0.1958 0.19115 0.1692
    ## Proportion of Variance 0.00146 0.00135 0.00107 0.00101 0.0008 0.00076 0.0006
    ## Cumulative Proportion  0.99254 0.99389 0.99496 0.99597 0.9968 0.99753 0.9981
    ##                           PC43    PC44    PC45    PC46    PC47    PC48
    ## Standard deviation     0.16442 0.15632 0.13058 0.09797 0.08057 0.07131
    ## Proportion of Variance 0.00056 0.00051 0.00036 0.00020 0.00014 0.00011
    ## Cumulative Proportion  0.99869 0.99920 0.99956 0.99976 0.99989 1.00000

## Visualise the PCA

Now the we have the PCA we can plot it. Here we use a range of groupings
(based on demographic data of respondents).

``` r
#Show just dots (individuals)
ggbiplot(gearpca, var.axes=FALSE) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Round1-Gear_analysis_pca_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#Show with gears overlaid
ggbiplot(gearpca) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Round1-Gear_analysis_pca_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
#Look at comparison with sector
ggbiplot(gearpca, ellipse=TRUE, groups=pcadata$sector, var.axes=FALSE) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Round1-Gear_analysis_pca_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
#Look at comparison with ocean region
ggbiplot(gearpca, ellipse=TRUE, groups=pcadata$oceanregion, var.axes=FALSE) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Round1-Gear_analysis_pca_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
#Look at comparison with home continent
ggbiplot(gearpca, ellipse=TRUE, groups=pcadata$homecontinent, var.axes=FALSE) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Round1-Gear_analysis_pca_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

``` r
#Look at comparison with whether their home and work continents are the same
ggbiplot(gearpca, ellipse=TRUE, groups=pcadata$homeworkdiff, var.axes=FALSE) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Round1-Gear_analysis_pca_files/figure-gfm/unnamed-chunk-1-6.png)<!-- -->

## Save plots

Finally, we save the sector plot for use in the manuscript
(supplementary information).

``` r
r1_sector_pca <- ggbiplot(gearpca, ellipse=TRUE, groups=pcadata$sector, var.axes=FALSE) +
geom_point(aes(colour = pcadata$sector)) +
scale_color_brewer(type = "qual", palette = "Dark2") + 
  theme_classic()
r1_sector_pca
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Round1-Gear_analysis_pca_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggsave2(here::here("figures", "r1_sector_pca.jpg"),
  plot = r1_sector_pca,
  device = "jpg",
  width = 20,
  height = 10,
  units = "cm")
```

Survey Rd 2 analysis
================
Arlie McCarthy
2023-10-31

This file contains the scripts for analysing the likert responses to
statements in R2, establishing which questions will be asked again in
R3, generating individual feedback figures for respondents, and
conducting a PCA on responses to statements in R2.

## Import survey q_numbers and responses

``` r
r2_likert_responses <- read_excel(here::here("data/R2_responses.xlsx"), sheet = "responses_wider")
r2_responses_longer <- read_excel(here::here("data/R2_responses.xlsx"), sheet = "responses")
r2_likert_questions <- read_excel(here::here("data/R2_responses.xlsx"), sheet = "questions")
tab <- r2_likert_responses %>% 
  filter(Name != "Lang",
         Name != "Country_from",
         Name != "Country_predominantely_work",
         Name != "Ocean_basin",
         Name != "Years_experience",
         Name != "Fishing_gear_exp",
         Name != "email",
         Name != "sector") %>% 
  mutate(q_number = Name) %>% 
  left_join(r2_likert_questions, by = "q_number") %>% 
  select(-Name) %>% 
  pivot_longer(names_to = "name", values_to = "response", cols = 1:54) %>% 
  select(question, name, response) %>% 
  group_by(question, response) %>% 
  dplyr::summarise(pcent = n()/54) %>% 
  pivot_wider(names_from = response, values_from = pcent) %>% 
  select("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree",  "Strongly agree") %>% 
  replace_na(list("Strongly disagree" = 0,
                  "Somewhat disagree" = 0,
                  "Neither agree nor disagree" = 0,
                  "Somewhat agree" = 0,
                  "Strongly agree" = 0)) %>% 
               left_join(r2_likert_questions)
```

    ## `summarise()` has grouped output by 'question'. You can override using the
    ## `.groups` argument.
    ## Adding missing grouping variables: `question`
    ## Joining with `by = join_by(question)`

``` r
mylevels<-c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree",  "Strongly agree")

round3_question_key <- read_excel(here::here("data/round3_question_key.xlsx"))
round3_question_key$question<-str_wrap(round3_question_key$question, width = 50)
```

# Prepare response data for plotting

``` r
tab$question<-str_wrap(tab$question, width = 50)
#numcenter<-ceiling(numlevels/2)+1
tab <- tab %>% mutate(midvalues = `Neither agree nor disagree` / 2)
tab$all_agree <- tab$`Somewhat agree` + tab$`Strongly agree`
tab$all_disagree <- tab$`Strongly disagree` + tab$`Somewhat disagree`
tab$polarisation <- tab$all_agree - tab$all_disagree # scores closer to 0 will be the most polarised
tab2<- tab %>% 
  mutate(midlow = midvalues,
         midhigh = midvalues) %>% 
  dplyr::select(question, `Strongly disagree`, `Somewhat disagree`, midlow, midhigh, `Somewhat agree`, `Strongly agree`, broad_category:short_question)
tab2 <- tab2  %>% arrange(question)
# Write the summary information into excel workbook
#write.xlsx(x = as.data.frame(tab), file = here::here("Survey_Results_Summary.xlsx"),
#      sheetName = "Round_2", append = FALSE, showNA = FALSE)

numlevels<-length(mylevels)+1
point1<-2
point2<-((numlevels)/2)+1
point3<-point2+1
point4<-numlevels+1
mymin<-(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
mymax<-(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100

temp.rows<-nrow(tab2)
pal<-brewer.pal((numlevels-1),"RdBu")
pal[ceiling(numlevels/2)]<-"#DFDFDF"
legend.pal<-pal
pal<-c(pal[1:(ceiling(numlevels/2)-1)], pal[ceiling(numlevels/2)], 
       pal[ceiling(numlevels/2)], pal[(ceiling(numlevels/2)+1):(numlevels-1)])

tab3<-tab2 %>% 
  pivot_longer(cols = c(2:7)) %>%
  arrange(question)
tab3 <- tab3 %>% 
  mutate(col = case_when(name == "Strongly disagree" ~ "#CA0020",
                         name == "Somewhat disagree" ~ "#F4A582",
                         name == "midlow" ~ "#DFDFDF",
                         name == "midhigh" ~ "#DFDFDF",
                         name == "Somewhat agree" ~ "#92C5DE",
                         name == "Strongly agree" ~ "#0571B0"))
tab3$value<-tab3$value*100
#tab3$question<-factor(tab3$question, levels = tab2$question[order(-(tab2[,2]+tab2[,3]+tab2[,4]))])
tab3 <- tab3 %>% group_by(broad_category, theme, narrow_theme, short_question)

highs_impacts<-tab3 %>% dplyr::filter(broad_category == "Impacts",
                               name == "Somewhat agree" |
                                 name == "Strongly agree" |
                                 name == "midhigh") %>% 
  arrange(theme, narrow_theme, short_question) %>% 
  group_by(narrow_theme)
highs_chars<-tab3 %>% dplyr::filter(broad_category == "Characteristics",
                               name == "Somewhat agree" |
                                 name == "Strongly agree" |
                                 name == "midhigh")  %>% 
  arrange(theme, narrow_theme, short_question) %>% 
  group_by(narrow_theme)
lows_impacts<-tab3 %>% dplyr::filter(broad_category == "Impacts",
                               name == "Somewhat disagree" |
                                 name == "Strongly disagree" |
                                 name == "midlow") %>% 
  arrange(theme, narrow_theme, short_question) %>% 
  group_by(theme, narrow_theme)
lows_chars<-tab3 %>% dplyr::filter(broad_category == "Characteristics",
                               name == "Somewhat disagree" |
                                 name == "Strongly disagree" |
                                 name == "midlow") %>% 
  arrange(theme, narrow_theme, short_question) %>% 
  group_by(narrow_theme)
highs_chars_concept = highs_chars %>% filter(theme == "Concept")
highs_chars_scope = highs_chars %>% filter(theme == "Scope")
lows_chars_concept = lows_chars %>% filter(theme == "Concept")
lows_chars_scope = lows_chars %>% filter(theme == "Scope")
highs_impacts_eco = highs_impacts %>% filter(theme == "Ecological")
highs_impacts_socio = highs_impacts %>% filter(theme == "Socio-economic")
lows_impacts_eco = lows_impacts %>% filter(theme == "Ecological")
lows_impacts_socio = lows_impacts %>% filter(theme == "Socio-economic")
```

# Create figures of responses

Concept figure

``` r
chars_concept_fig <- ggplot() + geom_bar(data=highs_chars_concept, aes(x = short_question, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows_chars_concept, aes(x = short_question, y=-value, fill=factor(col, levels = c("#CA0020", "#F4A582", "#DFDFDF"))), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  facet_grid(narrow_theme ~ ., scales = "free_y", drop = TRUE, space = "free", shrink = TRUE) +
  scale_fill_identity("Percent:", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_cowplot() + 
  coord_flip() +
  labs(y="",x="") +
  theme(axis.text.y = element_text(
                                   size = 8),
        axis.text.x = element_text(
                                   size = 8),
        strip.text.y = element_text(size = 8,
                                    angle = 0)) +
  theme(legend.position = "bottom")
  #scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax))

chars_concept_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/concept-1.png)<!-- -->

``` r
ggsave2(filename = here::here("figures", "characteristics_figure.pdf"),
  plot = chars_concept_fig,
  device = "pdf",
  width = 183,
  units = "mm"
)
```

    ## Saving 183 x 127 mm image

Scope figure

``` r
#And now the plot for the scope data
chars_scope_fig <- ggplot() + geom_bar(data=highs_chars_scope, aes(x = short_question, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows_chars_scope, aes(x = short_question, y=-value, fill=factor(col, levels = c("#CA0020", "#F4A582", "#DFDFDF"))), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  facet_grid(narrow_theme ~ ., scales = "free_y", drop = TRUE, space = "free", shrink = TRUE) +
  scale_fill_identity("Percent:", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_cowplot() + 
  coord_flip() +
  labs(y="",x="") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8,
                                    angle = 0)) +
  theme(legend.position = "none")
chars_scope_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/scope%20figure-1.png)<!-- -->

``` r
legend <- get_legend(chars_concept_fig + theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)))

legend_row <- plot_grid(get_legend(chars_concept_fig + theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8))),
  nrow = 1)
legend
```

    ## TableGrob (5 x 5) "guide-box": 2 grobs
    ##                                     z     cells                  name
    ## 99_72c9102ccad51558720c60f8e252d42d 1 (3-3,3-3)                guides
    ##                                     0 (2-4,2-4) legend.box.background
    ##                                               grob
    ## 99_72c9102ccad51558720c60f8e252d42d gtable[layout]
    ##                                     zeroGrob[NULL]

``` r
chars_fig <- plot_grid(chars_concept_fig + theme(legend.position = "none"), chars_scope_fig, align = "v", ncol = 1, rel_heights = c(1, 1.3), labels = c("Concept-related statements", "Scope-related statements"), label_size = 9, vjust = 1)
chars_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/scope%20figure-2.png)<!-- -->

``` r
chars_fig_both <- plot_grid(chars_fig,
                            legend_row,
                            ncol = 1, nrow = 2,
                            rel_heights = c(1, 0.04))
chars_fig_both
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/scope%20figure-3.png)<!-- -->

``` r
save_plot(here::here("figures", "chars_fig_both.pdf"), chars_fig_both, base_width = 183, base_height = 170, units = "mm")
```

Same again but for the impacts data

``` r
impacts_eco_fig <- ggplot() + geom_bar(data=highs_impacts_eco, aes(x = short_question, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows_impacts_eco, aes(x = short_question, y=-value, fill=factor(col, levels = c("#CA0020", "#F4A582", "#DFDFDF"))), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  facet_grid(narrow_theme ~ ., scales = "free_y", drop = TRUE, space = "free", shrink = TRUE) +
  scale_fill_identity("Percent:", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_cowplot() + 
  coord_flip() +
  labs(y="",x="") +
  theme(axis.text.y = element_text(
                                   size = 8),
        axis.text.x = element_text(
                                   size = 8),
        strip.text.y = element_text(size = 8,
                                    angle = 0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax))

impacts_eco_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/impacts-1.png)<!-- -->

``` r
#And now the plot for the socio-economic data
impacts_socio_fig <- ggplot() + geom_bar(data=highs_impacts_socio, aes(x = short_question, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows_impacts_socio, aes(x = short_question, y=-value, fill=factor(col, levels = c("#CA0020", "#F4A582", "#DFDFDF"))), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  facet_grid(narrow_theme ~ ., scales = "free_y", drop = TRUE, space = "free", shrink = TRUE) +
  scale_fill_identity("Percent:", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_cowplot() + 
  coord_flip() +
  labs(y="",x="") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8,
                                    angle = 0)) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax))
impacts_socio_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/impacts-2.png)<!-- -->

``` r
impacts_legend_row <- plot_grid(get_legend(impacts_eco_fig + theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8))),
  nrow = 1)
impacts_fig <- plot_grid(impacts_eco_fig + theme(legend.position = "none"), impacts_socio_fig, align = "v", ncol = 1, rel_heights = c(1, 0.8), labels = c("Ecological impact statements", "Socio-economic impact statements"), label_size = 9, vjust = 1, hjust = 0)
impacts_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/impacts-3.png)<!-- -->

``` r
impacts_fig_both <- plot_grid(impacts_fig,
                            impacts_legend_row,
                            ncol = 1, nrow = 2,
                            rel_heights = c(1, 0.04))
impacts_fig_both
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/impacts-4.png)<!-- -->

``` r
save_plot(here::here("figures", "impacts_fig_both.pdf"), impacts_fig_both, base_width = 183, base_height = 170, units = "mm")
```

# PCAs on demographic data from responses to likert questions

## Run PCA

``` r
likert_pca <- prcomp(r2_responses_longer[ ,c(13:57)], center=TRUE, scale. = TRUE)
summary(likert_pca)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     3.4524 2.4424 1.73364 1.55222 1.42817 1.32423 1.26834
    ## Proportion of Variance 0.2649 0.1326 0.06679 0.05354 0.04533 0.03897 0.03575
    ## Cumulative Proportion  0.2649 0.3974 0.46422 0.51776 0.56309 0.60206 0.63780
    ##                            PC8     PC9    PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     1.24508 1.21785 1.13756 1.11192 1.03725 0.98061 0.96210
    ## Proportion of Variance 0.03445 0.03296 0.02876 0.02747 0.02391 0.02137 0.02057
    ## Cumulative Proportion  0.67225 0.70521 0.73397 0.76144 0.78535 0.80672 0.82729
    ##                           PC15    PC16    PC17    PC18    PC19    PC20    PC21
    ## Standard deviation     0.87176 0.84439 0.84074 0.77754 0.76817 0.72918 0.68393
    ## Proportion of Variance 0.01689 0.01584 0.01571 0.01343 0.01311 0.01182 0.01039
    ## Cumulative Proportion  0.84418 0.86002 0.87573 0.88917 0.90228 0.91410 0.92449
    ##                           PC22    PC23    PC24    PC25    PC26    PC27    PC28
    ## Standard deviation     0.65963 0.60184 0.59516 0.51584 0.51168 0.49424 0.46290
    ## Proportion of Variance 0.00967 0.00805 0.00787 0.00591 0.00582 0.00543 0.00476
    ## Cumulative Proportion  0.93416 0.94221 0.95008 0.95599 0.96181 0.96724 0.97200
    ##                           PC29    PC30   PC31    PC32    PC33    PC34    PC35
    ## Standard deviation     0.44268 0.41212 0.3912 0.37430 0.33843 0.33031 0.30852
    ## Proportion of Variance 0.00435 0.00377 0.0034 0.00311 0.00255 0.00242 0.00212
    ## Cumulative Proportion  0.97636 0.98013 0.9835 0.98664 0.98919 0.99161 0.99373
    ##                           PC36    PC37    PC38    PC39   PC40    PC41    PC42
    ## Standard deviation     0.25366 0.23438 0.21370 0.19416 0.1644 0.13895 0.11304
    ## Proportion of Variance 0.00143 0.00122 0.00101 0.00084 0.0006 0.00043 0.00028
    ## Cumulative Proportion  0.99516 0.99638 0.99739 0.99823 0.9988 0.99926 0.99955
    ##                           PC43    PC44    PC45
    ## Standard deviation     0.09995 0.08411 0.05798
    ## Proportion of Variance 0.00022 0.00016 0.00007
    ## Cumulative Proportion  0.99977 0.99993 1.00000

``` r
#Show just dots (individuals)
ggbiplot(likert_pca, var.axes=FALSE) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#Show with questions overlaid
ggbiplot(likert_pca) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
#Look at comparison with sector
pca_sector <- ggbiplot(likert_pca, ellipse=TRUE, groups=r2_responses_longer$sector, var.axes=FALSE) + theme_classic() 
pca_sector
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
ggsave2(here::here("figures", "PCA_sector.jpg"),
  plot = pca_sector,
  device = "jpg",
  width = 20,
  height = 10,
  units = "cm")

#Look at comparison with ocean region
ggbiplot(likert_pca, ellipse=TRUE, groups=r2_responses_longer$Ocean_basin, var.axes=FALSE) + theme_classic() + theme(legend.position = "none")
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
#Look at comparison with home continent
ggbiplot(likert_pca, ellipse=TRUE, groups=r2_responses_longer$home_continent, var.axes=FALSE) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

``` r
#Look at comparison with whether their home and work continents are the same
ggbiplot(likert_pca, ellipse=TRUE, groups=r2_responses_longer$homeworkdiff, var.axes=FALSE) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->

``` r
#Look at comparison with number of years experience
ggbiplot(likert_pca, ellipse=TRUE, groups=r2_responses_longer$Years_experience, var.axes=FALSE) + theme_classic() 
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/unnamed-chunk-2-7.png)<!-- -->

\#General information

Round 2 of the survey had 54 responses (51 in English and 3 in Spanish).
Respondents were from 25 countries and 6 continents (Europe 56%, Oceania
15%, South America 9%, Asia 7%, North America 7%, Africa 6%). Experts
worked across 29 countries and 6 continents (Europe 46%, Asia 17%,
Oceania 15%, South America 7%, North America 7%, Africa 7%). Experts had
worked in all listed ocean regions (Indian, North and South Pacific,
North and South Atlantic, Southern Ocean), with 20 of 54 37% of
respondents having worked in multiple regions.

``` r
length(unique(r2_responses_longer$Country_from))
```

    ## [1] 25

``` r
r2_responses_longer %>% group_by(home_continent) %>% dplyr::summarise(continent = n()) %>% 
  mutate(continent_pcent = continent/54 * 100)
```

    ## # A tibble: 6 × 3
    ##   home_continent continent continent_pcent
    ##   <chr>              <int>           <dbl>
    ## 1 Africa                 3            5.56
    ## 2 Asia                   4            7.41
    ## 3 Europe                30           55.6 
    ## 4 North America          4            7.41
    ## 5 Oceania                8           14.8 
    ## 6 South America          5            9.26

``` r
length(unique(r2_responses_longer$Country_predominantely_work))
```

    ## [1] 29

``` r
r2_responses_longer %>% group_by(work_continent) %>% dplyr::summarise(continent = n()) %>% 
  mutate(continent_pcent = continent/54 * 100)
```

    ## # A tibble: 6 × 3
    ##   work_continent continent continent_pcent
    ##   <chr>              <int>           <dbl>
    ## 1 Africa                 4            7.41
    ## 2 Asia                   9           16.7 
    ## 3 Europe                25           46.3 
    ## 4 North America          4            7.41
    ## 5 Oceania                8           14.8 
    ## 6 South America          4            7.41

``` r
20/54
```

    ## [1] 0.3703704

## Creating demographic maps

Import the country baselayer for mapping.

``` r
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
```

## Check that the countries given in the survey data align with the countries in the mapping data.

``` r
country_information <- r2_responses_longer %>% 
  mutate(country_worked = Country_predominantely_work,
         country_from = Country_from,
         country_from_in_map = case_when(country_from %in% world$name_long ~ TRUE, 
                                         TRUE ~ FALSE),
         country_worked_in_map = case_when(country_worked %in% world$name_long ~ TRUE, 
                                         TRUE ~ FALSE)
  )
country_information %>% 
  filter(country_from_in_map == FALSE) %>% 
  group_by(country_from) %>% 
  slice(1)
```

    ## # A tibble: 4 × 61
    ## # Groups:   country_from [4]
    ##   Name   Lang  Country_from home_continent Country_predominante…¹ work_continent
    ##   <chr>  <chr> <chr>        <chr>          <chr>                  <chr>         
    ## 1 Mark … EN    UK           Europe         <NA>                   Europe        
    ## 2 Alex … EN    United King… Europe         Timor-Leste            Asia          
    ## 3 Peter… EN    United Stat… North America  United States of Amer… North America 
    ## 4 Moh A… EN    <NA>         Asia           Indonesia              Asia          
    ## # ℹ abbreviated name: ¹​Country_predominantely_work
    ## # ℹ 55 more variables: homeworkdiff <chr>, Ocean_basin <chr>,
    ## #   Years_experience <dbl>, Fishing_gear_exp <chr>, email <chr>, sector <chr>,
    ## #   `2_1` <dbl>, `2_2` <dbl>, `2_3` <dbl>, `2_4` <dbl>, `2_5` <dbl>,
    ## #   `2_6` <dbl>, `2_7` <dbl>, `2_8` <dbl>, `2_9` <dbl>, `2_10` <dbl>,
    ## #   `2_11` <dbl>, `2_12` <dbl>, `2_13` <dbl>, `3_1` <dbl>, `3_2` <dbl>,
    ## #   `3_3` <dbl>, `3_4` <dbl>, `3_5` <dbl>, `3_6` <dbl>, `3_7` <dbl>, …

``` r
country_information %>% 
  filter(country_worked_in_map == FALSE) %>% 
  group_by(country_worked) %>% 
  slice(1)
```

    ## # A tibble: 4 × 61
    ## # Groups:   country_worked [4]
    ##   Name   Lang  Country_from home_continent Country_predominante…¹ work_continent
    ##   <chr>  <chr> <chr>        <chr>          <chr>                  <chr>         
    ## 1 Jacqu… EN    South Africa Africa         Hong Kong (S.A.R.)     Asia          
    ## 2 Andre… EN    United King… Europe         United Kingdom of Gre… Europe        
    ## 3 Hilar… EN    Spain        Europe         United States of Amer… North America 
    ## 4 Gille… EN    Luxembourg   Europe         <NA>                   Europe        
    ## # ℹ abbreviated name: ¹​Country_predominantely_work
    ## # ℹ 55 more variables: homeworkdiff <chr>, Ocean_basin <chr>,
    ## #   Years_experience <dbl>, Fishing_gear_exp <chr>, email <chr>, sector <chr>,
    ## #   `2_1` <dbl>, `2_2` <dbl>, `2_3` <dbl>, `2_4` <dbl>, `2_5` <dbl>,
    ## #   `2_6` <dbl>, `2_7` <dbl>, `2_8` <dbl>, `2_9` <dbl>, `2_10` <dbl>,
    ## #   `2_11` <dbl>, `2_12` <dbl>, `2_13` <dbl>, `3_1` <dbl>, `3_2` <dbl>,
    ## #   `3_3` <dbl>, `3_4` <dbl>, `3_5` <dbl>, `3_6` <dbl>, `3_7` <dbl>, …

From that we see that the following countries are given as locations but
not listed in the map data: - United Kingdom of Great Britain and
Northern Ireland  
- UK - United States of America  
- Hong Kong (S.A.R.)

The equivalent locations in the map data are: - United Kingdom  
- United States  
- Hong Kong

Therefore need to change the names for plotting so that they align with
the map data.

``` r
country_information <- country_information %>% 
  mutate(country_from = case_when(country_from == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                                  country_from == "UK" ~ "United Kingdom",
                                  country_from == "United States of America" ~ "United States",
                                  country_from == "Hong Kong (S.A.R.)" ~ "Hong Kong",
                                         TRUE ~ country_from),
         country_worked = case_when(country_worked == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                                    country_worked == "UK" ~ "United Kingdom",
                                    country_worked == "United States of America" ~ "United States",
                                    country_worked == "Hong Kong (S.A.R.)" ~ "Hong Kong",
                                         TRUE ~ country_worked)
  )
```

## Reshape and joing country information data to mapping

``` r
country_from_summarised <- country_information %>% 
  group_by(country_from) %>% 
  dplyr::summarise(from = n()) %>% 
  dplyr::rename(name_long = country_from)
country_worked_summarised <- country_information %>% 
  group_by(country_worked) %>% 
  dplyr::summarise(worked = n()) %>% 
  dplyr::rename(name_long = country_worked)
survey_data_summarised <- country_from_summarised %>% 
  left_join(country_worked_summarised)
```

    ## Joining with `by = join_by(name_long)`

``` r
world <- world %>% 
  left_join(survey_data_summarised)
```

    ## Joining with `by = join_by(name_long)`

## Making the plots

``` r
plot_countries_from <- ggplot() +
  geom_sf(data = world,
          aes(fill = from,
              colour = from)
          ) +
  scale_fill_viridis_b(name = "", na.value="snow2", option = "C", end = 0.9) +
  scale_colour_viridis_b(name = "", na.value="snow3", option = "C", end = 0.9) +
  labs(title="Number of respondents from each country") +
  theme_minimal()

plot_countries_from
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/making%20demographic%20maps-1.png)<!-- -->

``` r
ggsave(here::here("figures", "R2_countries_from_map.png"),
  plot = plot_countries_from,
  device = "png",
  width = 183,
  units = "mm"
)
```

    ## Saving 183 x 127 mm image

``` r
plot_countries_worked <- ggplot() +
  geom_sf(data = world,
          aes(fill = worked,
              colour = worked)
          ) +
  scale_fill_viridis_b(name = "", na.value="snow2", option = "C", end = 0.9) +
  scale_colour_viridis_b(name = "", na.value="snow3", option = "C", end = 0.9) +
  labs(title="Number of respondents working in each country") +
  theme_minimal()

plot_countries_worked
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/making%20demographic%20maps-2.png)<!-- -->

``` r
plot_both <- plot_grid(plot_countries_from, plot_countries_worked, ncol = 1)
plot_both
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/making%20demographic%20maps-3.png)<!-- -->

``` r
save_plot(here::here("figures", "R2survey_demographics_map.pdf"), plot_both, base_width = 183, base_height = 200, units = "mm")
```

# Polarisation analysis

need to make another version of tab, this time with the raw values
rather than the percentages.

``` r
polar <- r2_likert_responses %>% 
  filter(Name != "Lang",
         Name != "Country_from",
         Name != "Country_predominantely_work",
         Name != "Ocean_basin",
         Name != "Years_experience",
         Name != "Fishing_gear_exp",
         Name != "email",
         Name != "sector") %>% 
  mutate(q_number = Name) %>% 
  left_join(r2_likert_questions, by = "q_number") %>% 
  select(-Name, -q_number) %>% 
  pivot_longer(names_to = "name", values_to = "response", cols = 1:54) %>% 
  select(question, name, response) %>% 
  group_by(question, response) %>% 
  dplyr::summarise(count = n()) %>% 
  pivot_wider(names_from = response, values_from = count) %>% 
  select("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree",  "Strongly agree") %>% 
  replace_na(list("Strongly disagree" = 0,
                  "Somewhat disagree" = 0,
                  "Neither agree nor disagree" = 0,
                  "Somewhat agree" = 0,
                  "Strongly agree" = 0)) %>% 
               left_join(r2_likert_questions)
```

    ## `summarise()` has grouped output by 'question'. You can override using the
    ## `.groups` argument.
    ## Adding missing grouping variables: `question`
    ## Joining with `by = join_by(question)`

Now calculate the scores for each row/question

``` r
agreement(as.vector(polar[1,2:6], mode = "numeric"))
```

    ## [1] 0.3209877

``` r
agreement(as.vector(polar[2,2:6], mode = "numeric"))
```

    ## [1] 0.08230453

``` r
agreement(as.vector(polar[3,2:6], mode = "numeric"))
```

    ## [1] 0.03703704

``` r
agreement(as.vector(polar[4,2:6], mode = "numeric"))
```

    ## [1] 0.4290123

``` r
agreement(as.vector(polar[5,2:6], mode = "numeric"))
```

    ## [1] 0.09876543

``` r
agreement(as.vector(polar[6,2:6], mode = "numeric"))
```

    ## [1] 0.4166667

``` r
agreement(as.vector(polar[7,2:6], mode = "numeric"))
```

    ## [1] 0.462963

``` r
agreement(as.vector(polar[8,2:6], mode = "numeric"))
```

    ## [1] 0.08230453

``` r
agreement(as.vector(polar[9,2:6], mode = "numeric"))
```

    ## [1] 0.4320988

``` r
agreement(as.vector(polar[10,2:6], mode = "numeric"))
```

    ## [1] 0.4351852

``` r
agreement(as.vector(polar[11,2:6], mode = "numeric"))
```

    ## [1] 0.2253086

``` r
agreement(as.vector(polar[12,2:6], mode = "numeric"))
```

    ## [1] 0.5524691

``` r
agreement(as.vector(polar[13,2:6], mode = "numeric"))
```

    ## [1] 0.1851852

``` r
agreement(as.vector(polar[14,2:6], mode = "numeric"))
```

    ## [1] 0.1080247

``` r
agreement(as.vector(polar[15,2:6], mode = "numeric"))
```

    ## [1] 0.5

``` r
agreement(as.vector(polar[16,2:6], mode = "numeric"))
```

    ## [1] 0.6574074

``` r
agreement(as.vector(polar[17,2:6], mode = "numeric"))
```

    ## [1] 0.6666667

``` r
agreement(as.vector(polar[18,2:6], mode = "numeric"))
```

    ## [1] 0.5277778

``` r
agreement(as.vector(polar[19,2:6], mode = "numeric"))
```

    ## [1] 0.5030864

``` r
agreement(as.vector(polar[20,2:6], mode = "numeric"))
```

    ## [1] 0.5462963

``` r
agreement(as.vector(polar[21,2:6], mode = "numeric"))
```

    ## [1] 0.3950617

``` r
agreement(as.vector(polar[22,2:6], mode = "numeric"))
```

    ## [1] 0.4104938

``` r
agreement(as.vector(polar[23,2:6], mode = "numeric"))
```

    ## [1] 0.5493827

``` r
agreement(as.vector(polar[24,2:6], mode = "numeric"))
```

    ## [1] 0.654321

``` r
agreement(as.vector(polar[25,2:6], mode = "numeric"))
```

    ## [1] 0.3590535

``` r
agreement(as.vector(polar[26,2:6], mode = "numeric"))
```

    ## [1] 0.0308642

``` r
agreement(as.vector(polar[27,2:6], mode = "numeric"))
```

    ## [1] 0.1049383

``` r
agreement(as.vector(polar[28,2:6], mode = "numeric"))
```

    ## [1] 0.6296296

``` r
agreement(as.vector(polar[29,2:6], mode = "numeric"))
```

    ## [1] 0.3641975

``` r
agreement(as.vector(polar[30,2:6], mode = "numeric"))
```

    ## [1] 0.6666667

``` r
agreement(as.vector(polar[31,2:6], mode = "numeric"))
```

    ## [1] 0.9166667

``` r
agreement(as.vector(polar[32,2:6], mode = "numeric"))
```

    ## [1] 0.7654321

``` r
agreement(as.vector(polar[33,2:6], mode = "numeric"))
```

    ## [1] 0.5

``` r
agreement(as.vector(polar[34,2:6], mode = "numeric"))
```

    ## [1] 0.1512346

``` r
agreement(as.vector(polar[35,2:6], mode = "numeric"))
```

    ## [1] 0.132716

``` r
agreement(as.vector(polar[36,2:6], mode = "numeric"))
```

    ## [1] 0.1728395

``` r
agreement(as.vector(polar[37,2:6], mode = "numeric"))
```

    ## [1] 0.08024691

``` r
agreement(as.vector(polar[38,2:6], mode = "numeric"))
```

    ## [1] 0.2283951

``` r
agreement(as.vector(polar[39,2:6], mode = "numeric"))
```

    ## [1] 0.2736626

``` r
agreement(as.vector(polar[40,2:6], mode = "numeric"))
```

    ## [1] 0.6512346

``` r
agreement(as.vector(polar[41,2:6], mode = "numeric"))
```

    ## [1] 0.5648148

``` r
agreement(as.vector(polar[42,2:6], mode = "numeric"))
```

    ## [1] 0.2242798

``` r
agreement(as.vector(polar[43,2:6], mode = "numeric"))
```

    ## [1] 0.2633745

``` r
agreement(as.vector(polar[44,2:6], mode = "numeric"))
```

    ## [1] 0.7777778

``` r
agreement(as.vector(polar[45,2:6], mode = "numeric"))
```

    ## [1] 0.4351852

``` r
p1 <- polarization(as.vector(polar[1,2:6], mode = "numeric"))
```

# IDENTIFYING QUESTIONS TO USE IN ROUND 3

``` r
#How many questions would we ask again with a 70% threshold?
round3_questions_70 <- tab %>%
  filter(all_agree < 0.7) %>% 
  filter(all_disagree < 0.7)
nrow(round3_questions_70)
```

    ## [1] 25

``` r
#Answer: 25

round3_questions_70 <- round3_questions_70 %>% 
  left_join(round3_question_key, by = c("question", "q_number")) %>% 
  mutate(question_with_number = paste(q_number_R3, question))  %>% 
  mutate(question_with_number = sub("_", ".", question_with_number)) %>% 
  mutate(question_with_number = factor(question_with_number, levels= c(
    "5.4 Only some practices that use 'legitimate' fishing\ngears can be adapted or mitigated to reduce their\nimpact to an appropriate level",
    "5.3 All practices that use 'legitimate' fishing gears\ncan be adapted or mitigated to reduce their impact\nto an appropriate level",
    "5.2 Poor or low capacity management - rather than\nspecific fishing practices - determines how\ndestructive a fishing activity is",
    "5.1 All fishing practices can be destructive (if not\ncarried out appropriately or maintained at an\nappropriate level)",
    "4.4 Shares some characteristics with the term\n'illegal, unreported and unregulated (IUU)\nfishing'",
    "4.3 Shares some characteristics with the term\n'overfishing'",
    "4.2 Is the same (conceptually) as the term 'fishing\nthat causes significant adverse impact'",
    "4.1 Is the same (conceptually) as the term 'illegal,\nunreported and unregulated (IUU) fishing'",
    "3.10 Fishing activity (or any other anthropogenic\nactivity) that infringes the legitimate rights of\nother fishers is destructive",
    "3.9 Proportional loss of a target species from fishing\nthat exceeds its biologically safe limits is\ndestructive",
    "3.8 Any mortality or injury to a non-target species\nfrom fishing (or any other anthropogenic activity)\nis destructive",
    "3.7 Any damage to a seabed habitat from fishing (or\nany other anthropogenic activity) is destructive",
    "3.6 Relates to the economic impact of financial loss\nto national economies",
    "3.5 Relates to the economic impact of financial loss\nto fishers/fishing enterprises",
    "3.4 Relates to the social impact of human rights\nviolation",
    "3.3 Relates to the social impact of conflict between\nsectors",
    "3.2 Relates to the social impact of traditional\nculture loss/degradation",
    "3.1 Relates to the social impact of traditional\nlivelihood loss/degradation",
    "2.7 Cannot be expressed universally (i.e. is always\ncontext-dependent)",
    "2.6 Describes changes/impacts that are avoidable",
    "2.5 Describes changes/impacts that are reversible over\nany time-scale",
    "2.4 Describes changes/impacts that are irreversible\nover any time scale",
    "2.3 Can describe an economic concept",
    "2.2 Can describe a social concept",
    "2.1 Has changed in its meaning over time"
)))

#What about a 75% threshold?
round3_questions_75 <- tab %>%
  filter(all_agree < 0.75) %>% 
  filter(all_disagree < 0.75)
nrow(round3_questions_75)
```

    ## [1] 27

``` r
#Answer: 27
```

# MODIFIED FIGURE TO SHARE DURING ROUND 3

``` r
highs_R3 <-tab3 %>% 
  ungroup() %>% 
  dplyr::filter(tab3$question %in% round3_questions_70$question) %>% 
  dplyr::filter(name == "Somewhat agree" |
                                 name == "Strongly agree" |
                                 name == "midhigh") %>% 
  left_join(round3_questions_70, by = "question") %>% 
  arrange(question_with_number)   %>% 
  mutate(question_with_number = factor(question_with_number, levels= c(
        "5.4 Only some practices that use 'legitimate' fishing\ngears can be adapted or mitigated to reduce their\nimpact to an appropriate level",
    "5.3 All practices that use 'legitimate' fishing gears\ncan be adapted or mitigated to reduce their impact\nto an appropriate level",
    "5.2 Poor or low capacity management - rather than\nspecific fishing practices - determines how\ndestructive a fishing activity is",
    "5.1 All fishing practices can be destructive (if not\ncarried out appropriately or maintained at an\nappropriate level)",
    "4.4 Shares some characteristics with the term\n'illegal, unreported and unregulated (IUU)\nfishing'",
    "4.3 Shares some characteristics with the term\n'overfishing'",
    "4.2 Is the same (conceptually) as the term 'fishing\nthat causes significant adverse impact'",
    "4.1 Is the same (conceptually) as the term 'illegal,\nunreported and unregulated (IUU) fishing'",
    "3.10 Fishing activity (or any other anthropogenic\nactivity) that infringes the legitimate rights of\nother fishers is destructive",
    "3.9 Proportional loss of a target species from fishing\nthat exceeds its biologically safe limits is\ndestructive",
    "3.8 Any mortality or injury to a non-target species\nfrom fishing (or any other anthropogenic activity)\nis destructive",
    "3.7 Any damage to a seabed habitat from fishing (or\nany other anthropogenic activity) is destructive",
    "3.6 Relates to the economic impact of financial loss\nto national economies",
    "3.5 Relates to the economic impact of financial loss\nto fishers/fishing enterprises",
    "3.4 Relates to the social impact of human rights\nviolation",
    "3.3 Relates to the social impact of conflict between\nsectors",
    "3.2 Relates to the social impact of traditional\nculture loss/degradation",
    "3.1 Relates to the social impact of traditional\nlivelihood loss/degradation",
    "2.7 Cannot be expressed universally (i.e. is always\ncontext-dependent)",
    "2.6 Describes changes/impacts that are avoidable",
    "2.5 Describes changes/impacts that are reversible over\nany time-scale",
    "2.4 Describes changes/impacts that are irreversible\nover any time scale",
    "2.3 Can describe an economic concept",
    "2.2 Can describe a social concept",
    "2.1 Has changed in its meaning over time"
)))

lows_R3 <- tab3 %>% 
  ungroup() %>%
  dplyr::filter(tab3$question %in% round3_questions_70$question) %>% 
  dplyr::filter(name == "Somewhat disagree" |
                                 name == "Strongly disagree" |
                                 name == "midlow") %>% 
  left_join(round3_questions_70, by = "question") %>% 
  arrange(question_with_number)   %>% 
  mutate(question_with_number = factor(question_with_number, levels= c(
        "5.4 Only some practices that use 'legitimate' fishing\ngears can be adapted or mitigated to reduce their\nimpact to an appropriate level",
    "5.3 All practices that use 'legitimate' fishing gears\ncan be adapted or mitigated to reduce their impact\nto an appropriate level",
    "5.2 Poor or low capacity management - rather than\nspecific fishing practices - determines how\ndestructive a fishing activity is",
    "5.1 All fishing practices can be destructive (if not\ncarried out appropriately or maintained at an\nappropriate level)",
    "4.4 Shares some characteristics with the term\n'illegal, unreported and unregulated (IUU)\nfishing'",
    "4.3 Shares some characteristics with the term\n'overfishing'",
    "4.2 Is the same (conceptually) as the term 'fishing\nthat causes significant adverse impact'",
    "4.1 Is the same (conceptually) as the term 'illegal,\nunreported and unregulated (IUU) fishing'",
    "3.10 Fishing activity (or any other anthropogenic\nactivity) that infringes the legitimate rights of\nother fishers is destructive",
    "3.9 Proportional loss of a target species from fishing\nthat exceeds its biologically safe limits is\ndestructive",
    "3.8 Any mortality or injury to a non-target species\nfrom fishing (or any other anthropogenic activity)\nis destructive",
    "3.7 Any damage to a seabed habitat from fishing (or\nany other anthropogenic activity) is destructive",
    "3.6 Relates to the economic impact of financial loss\nto national economies",
    "3.5 Relates to the economic impact of financial loss\nto fishers/fishing enterprises",
    "3.4 Relates to the social impact of human rights\nviolation",
    "3.3 Relates to the social impact of conflict between\nsectors",
    "3.2 Relates to the social impact of traditional\nculture loss/degradation",
    "3.1 Relates to the social impact of traditional\nlivelihood loss/degradation",
    "2.7 Cannot be expressed universally (i.e. is always\ncontext-dependent)",
    "2.6 Describes changes/impacts that are avoidable",
    "2.5 Describes changes/impacts that are reversible over\nany time-scale",
    "2.4 Describes changes/impacts that are irreversible\nover any time scale",
    "2.3 Can describe an economic concept",
    "2.2 Can describe a social concept",
    "2.1 Has changed in its meaning over time"
)))
```

Preparing the data for creating summary results to distribute in Round 3

``` r
all_answers <-  r2_likert_responses %>%
  dplyr::rename(q_number = Name) %>% 
  right_join(round3_questions_70, by = "q_number")
```

# Figure of overall responses

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd2_likert_analysis_files/figure-gfm/pressure-1.png)<!-- -->

# Individual feedback figures

Now, create a loop or similar to create a figure for each individual.
This section isn’t run here because it generates individual figures, but
retained here to show what was run to generate the indivdual feedback
figures.

    #create list of names
    respondents <- as.list(names(all_answers[,2:55]))
    respondents <- sub(" ", "_", respondents)
    names(all_answers) <- sub(" ", "_", names(all_answers))
    all_respondent_figures <- list()
    for( i in 1:length(respondents)) {
      respondent_name <- respondents[[i]]
      person <- all_answers %>% 
        select_if(colnames(.) %in% respondent_name | 
                 colnames(.) == "question_with_number")
      colnames(person)[1] <- "person_name"
      person1 <- person$person_name
      names(person1) <- person$question_with_number
      
      individual_fig_Rd3 <- ggplot() + geom_bar(data=highs_R3, 
                                                aes(x = question_with_number, 
                                                    y=value, fill=col),
                                                position="stack", stat="identity") +
      geom_bar(data=lows_R3, 
               aes(x = question_with_number, y=-value, 
                   fill=factor(col, levels = c("#CA0020", "#F4A582", "#DFDFDF"))), 
               position="stack", stat="identity") +
      geom_hline(yintercept = 0, color =c("white")) +
      scale_fill_identity("Response", 
                          labels = mylevels, breaks=legend.pal, guide="legend") +
      theme_fivethirtyeight() + 
      coord_flip() +
      labs(y="Question",
           x="",
           y.sec = "Your response",
           title = "Questions from Round 2 without consensus",
           subtitle = "                                                      Collated responses                                                 Your response",
           caption = "Figure shows all responses for the 25 statements that did not reach a threshold of 70% consensus, shown as a percentage of all responses. Positive values\nindicate agreement with the statement, negative values indicate disagreement. Your response to the statement is shown to the right of the figure.") +
      theme(plot.title = element_text(size=14, hjust=0.5)) +
      theme(axis.text.y = element_text(hjust=0)) +
      theme(legend.position = "bottom") +
      scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax)) +
      guides(y.sec = guide_axis_manual(
        title = "Your response",
        breaks = names(person1),
        labels = person1))
      
      all_respondent_figures[[i]] <- individual_fig_Rd3
      individual_fig_name <- paste("Individual_fig_Rd3_", respondents[[i]], ".pdf", sep = "")
      
      ggsave2(here::here("Individual_figures_Rd_3", individual_fig_name),
      plot = individual_fig_Rd3,
      device = "pdf",
      width = 30,
      height = 35,
      units = "cm")
    }

# Save the csv file of questions for using in Round 3

``` r
write_csv2(round3_questions_70, file = here::here("data", "Round3_Questions.csv"), col_names = TRUE, escape = "double", quote = "all")
```

Survey_R2andR3_likert_analysis
================
Arlie McCarthy
2023-10-31

This file generates the figures of responses to statements in R2 and R3.

# Set-up

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(here)
```

    ## here() starts at /Users/armcca001/Documents/Dokumente - ohif01m019/Projects/Destructive Fishing/destructive_fishing_defintion_delphi

``` r
library(readxl)
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)
library(ggbiplot)
```

    ## Loading required package: plyr
    ## ------------------------------------------------------------------------------
    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)
    ## ------------------------------------------------------------------------------
    ## 
    ## Attaching package: 'plyr'
    ## 
    ## The following object is masked from 'package:here':
    ## 
    ##     here
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact
    ## 
    ## Loading required package: scales
    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor
    ## 
    ## Loading required package: grid

``` r
library(sf)
```

    ## Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

``` r
library(rnaturalearth)
```

    ## The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
    ## which was just loaded, will retire in October 2023.
    ## Please refer to R-spatial evolution reports for details, especially
    ## https://r-spatial.org/r/2023/05/15/evolution4.html.
    ## It may be desirable to make the sf package available;
    ## package maintainers should consider adding sf to Suggests:.
    ## The sp package is now running under evolution status 2
    ##      (status 2 uses the sf package in place of rgdal)
    ## Support for Spatial objects (`sp`) will be deprecated in {rnaturalearth} and will be removed in a future release of the package. Please use `sf` objects with {rnaturalearth}. For example: `ne_download(returnclass = 'sf')`

``` r
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'
    ## 
    ## The following object is masked from 'package:ggthemes':
    ## 
    ##     theme_map
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
library(agrmt)
```

    ## 
    ## Attaching package: 'agrmt'
    ## 
    ## The following object is masked from 'package:scales':
    ## 
    ##     censor
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

``` r
library(ggh4x)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

# Importing Data

The first step is to import data for both round 2 and round 3, then join
them into a file that allows them to be plotted together

``` r
r2_likert_responses <- read_excel(here::here("data/R2_responses.xlsx"), sheet = "responses_wider")
r2_responses_longer <- read_excel(here::here("data/R2_responses.xlsx"), sheet = "responses")
r2_likert_questions <- read_excel(here::here("data/R2_responses.xlsx"), sheet = "questions")
tabr2 <- r2_likert_responses %>% 
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
mylevelsr2<-c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree",  "Strongly agree")

round3_question_key <- read_excel(here::here("data/round3_question_key.xlsx"))
round3_question_key$question<-str_wrap(round3_question_key$question, width = 50)
```

``` r
r3_likert_responses <- read_excel(here::here("data/Rd3_Results_Final.xlsx"), sheet = "responses_wider")
r3_responses_longer <- read_excel(here::here("data/Rd3_Results_Final.xlsx"), sheet = "responses")
```

    ## New names:
    ## • `3.1` -> `3.1...19`
    ## • `3.1` -> `3.1...21`
    ## • `3.1` -> `3.1...37`

``` r
r3_likert_questions <- read_excel(here::here("data/Rd3_Results_Final.xlsx"), sheet = "questions")
r3_likert_questions <- r3_likert_questions %>% 
  mutate(question_with_number = paste(q_number_rd3, question))

tabr3 <- r3_likert_responses %>% 
  filter(Name != "Lang",
         Name != "email",
         Name != "sector") %>% 
  mutate(q_number_rd3 = Name) %>% 
  left_join(r3_likert_questions, by = "q_number_rd3") %>% 
  select(-Name) %>% 
  pivot_longer(names_to = "name", values_to = "response", cols = 1:39) %>% 
  select(question_with_number, name, response) %>% 
  group_by(question_with_number, response) %>% 
  dplyr::summarise(pcent = n()/39) %>% 
  pivot_wider(names_from = response, values_from = pcent) %>% 
  select("Strongly disagree", "Somewhat disagree", "Prefer not to say", "Somewhat agree",  "Strongly agree") %>% 
  replace_na(list("Strongly disagree" = 0,
                  "Somewhat disagree" = 0,
                  "Prefer not to say" = 0,
                  "Somewhat agree" = 0,
                  "Strongly agree" = 0)) %>% 
               left_join(r3_likert_questions)
```

    ## `summarise()` has grouped output by 'question_with_number'. You can override
    ## using the `.groups` argument.
    ## Adding missing grouping variables: `question_with_number`
    ## Joining with `by = join_by(question_with_number)`

``` r
mylevelsr3<-c("Strongly agree","Somewhat agree", "Somewhat disagree", "Strongly disagree", "Prefer not to say")
mybreaks <- c("#0571B0", "#92C5DE", "#F4A582", "#CA0020", "#ffffff")

tabr3$agree = tabr3$`Strongly agree` + tabr3$`Somewhat agree`
tabr3$disagree = tabr3$`Strongly disagree` + tabr3$`Somewhat disagree`
tabr3$sum_pcent = tabr3$`Strongly agree` + tabr3$`Somewhat agree` + tabr3$`Strongly disagree` + tabr3$`Somewhat disagree`
#tabr3$question = paste( "* ", tabr3$question)
# this line removed to remove the asterisk in the final figures
```

Now join the two dataframes to replace the questions asked in R2 with
those asked in R3 BUT also add a variable that indicates which round the
answer is from so that it can be displayed in the figure, too.

For these figures a version with an asterisk to indicate which questions
were asked in round 3, and a version to indicate which questions were
only asked in round 2. The version with the asterisk is saved with \* at
the end of the filename and is included in the supplementary informtion.
The version without the asterisk is used in the main manuscript. To
avoid duplicating almost identical sections of code, the version without
the asterisk is shown below.

``` r
`%!in%` = Negate(`%in%`)
tabr2$round = 2
tabr3$round = 3
tabr3.v2 <- tabr3 %>% 
  ungroup() %>% 
  select("question",
         "Strongly disagree",
         "Somewhat disagree",
         "Prefer not to say", 
         "Somewhat agree", 
         "Strongly agree",
         "q_number",
         "q_group",
         "q_start_phrase",
         "broad_category",
         "theme",
         "narrow_theme",
         "short_question",
         "round"
         )

tab <- tabr2 %>% 
  filter(q_number %!in% tabr3.v2$q_number) %>% 
  full_join(tabr3.v2)
```

    ## Joining with `by = join_by(question, `Strongly disagree`, `Somewhat disagree`,
    ## `Somewhat agree`, `Strongly agree`, q_number, q_group, q_start_phrase,
    ## broad_category, theme, narrow_theme, short_question, round)`

``` r
tab$question <- str_wrap(tab$question, width = 55)
tab$narrow_theme <- str_wrap(tab$narrow_theme, width = 12)

tab2<- tab %>% 
  dplyr::select(question,
                `Strongly disagree`,
                `Somewhat disagree`,
                `Somewhat agree`,
                `Strongly agree`,
                -`Prefer not to say`,
                -`Neither agree nor disagree`,
                q_number:round) %>% 
  pivot_longer(cols = c(2:5))
tab3 <- tab2 %>% 
  mutate(col = case_when(name == "Strongly disagree" ~ "#CA0020",
                         name == "Somewhat disagree" ~ "#F4A582",
                         name == "Somewhat agree" ~ "#92C5DE",
                         name == "Strongly agree" ~ "#0571B0"))
tab3$value<-tab3$value*100
```

Create the high and low values to make the plot

``` r
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

# Figures grouped by broad category

``` r
chars_concept_fig <- ggplot() + geom_bar(data=highs_chars_concept, aes(x = reorder(question, value), y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows_chars_concept, aes(x = question, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  geom_hline(yintercept = -70, color =c("black")) +
  geom_hline(yintercept = 70, color =c("black")) +
  scale_fill_identity("Response types:", labels = mylevelsr3, breaks = mybreaks, guide="legend") +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 10)) + 
  theme_cowplot() + 
  coord_flip() +
  labs(y="",x="") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8,
                                   hjust = 1,
                                   angle = 45),
        strip.text.y = element_text(size = 8,
                                    angle = 0)) +
  theme(legend.position = c(0.95, -0.1),
        legend.direction = "horizontal",
        legend.justification="right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))

chars_concept_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_R2andR3_likert_figures_files/figure-gfm/concept%20figure-1.png)<!-- -->

``` r
ggsave2(filename = here::here("figures", "concept_figure.png"),
  plot = chars_concept_fig,
  device = "png",
  width = 183,
  height = 123,
  units = "mm"
)
```

\###And now the plot for the scope data

``` r
chars_scope_fig <- ggplot() + geom_bar(data=highs_chars_scope,  aes(x = reorder(question, value), y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows_chars_scope, aes(x = question, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  geom_hline(yintercept = -70, color =c("black")) +
  geom_hline(yintercept = 70, color =c("black")) +
#  facet_grid(narrow_theme ~ ., scales = "free_y", drop = TRUE, space = "free", shrink = TRUE) +
  scale_fill_identity("Response types:", labels = mylevelsr3, breaks = mybreaks, guide="legend") +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 10)) + 
  theme_cowplot() + 
  coord_flip() +
  labs(y="",x="") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8,
                                   hjust = 1,
                                   angle = 45),
        strip.text.y = element_text(size = 8,
                                    angle = 0)) +
  theme(legend.position = c(0.95, -0.08),
        legend.direction = "horizontal",
        legend.justification="right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
chars_scope_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_R2andR3_likert_figures_files/figure-gfm/scope%20figure-1.png)<!-- -->

``` r
ggsave2(filename = here::here("figures", "scope_figure.png"),
  plot = chars_scope_fig,
  device = "png",
  width = 183,
  height = 163,
  units = "mm"
)
```

Same again but for the impacts data

``` r
impacts_eco_fig <- ggplot() + geom_bar(data=highs_impacts_eco, aes(x = reorder(question, value), y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows_impacts_eco, aes(x = question, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  geom_hline(yintercept = -70, color =c("black")) +
  geom_hline(yintercept = 70, color =c("black")) +
#  facet_grid(narrow_theme ~ ., scales = "free_y", drop = TRUE, space = "free", shrink = TRUE) +
  scale_fill_identity("Response types:", labels = mylevelsr3, breaks = mybreaks, guide="legend") +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 10)) + 
  theme_cowplot() + 
  coord_flip() +
  labs(y="",x="") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8,
                                   hjust = 1,
                                   angle = 45),
        strip.text.y = element_text(size = 8,
                                    angle = 0)) +
  theme(legend.position = c(0.95, -0.15),
        legend.direction = "horizontal",
        legend.justification="right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
#  theme(legend.position = "none")
impacts_eco_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_R2andR3_likert_figures_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggsave2(filename = here::here("figures", "eco_impacts_figure.png"),
  plot = impacts_eco_fig,
  device = "png",
  width = 183,
  height = 103,
  units = "mm"
)
```

``` r
#And now the plot for the socio-economic impacts data
impacts_socio_fig <- ggplot() + geom_bar(data=highs_impacts_socio, aes(x = reorder(question, value), y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows_impacts_socio, aes(x = question, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  geom_hline(yintercept = -70, color =c("black")) +
  geom_hline(yintercept = 70, color =c("black")) +
#  facet_grid(narrow_theme ~ ., scales = "free_y", drop = TRUE, space = "free", shrink = TRUE) +
  scale_fill_identity("Response types:", labels = mylevelsr3, breaks = mybreaks, guide="legend") +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 10)) + 
  theme_cowplot() + 
  coord_flip() +
  labs(y="",x="") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8,
                                   hjust = 1,
                                   angle = 45),
        strip.text.y = element_text(size = 8,
                                    angle = 0)) +
  theme(legend.position = c(0.95, -0.15),
        legend.direction = "horizontal",
        legend.justification="right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
impacts_socio_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_R2andR3_likert_figures_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Save the plot

``` r
ggsave2(filename = here::here("figures", "socio_impacts_figure.png"),
  plot = impacts_socio_fig,
  device = "png",
  width = 183,
  height = 83,
  units = "mm"
)
```

The impacts figures are both relatively small so can be combined into
one figure

``` r
impacts_legend_row <- plot_grid(get_legend(impacts_eco_fig + theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8))),
  nrow = 1)
impacts_fig <- plot_grid(impacts_eco_fig + theme(legend.position = "none"), impacts_socio_fig, align = "v", ncol = 1, rel_heights = c(1, 0.8), labels = c("a) Ecological impact statements", "b) Socio-economic impact statements"), label_size = 9, vjust = 1, hjust = 0)
impacts_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_R2andR3_likert_figures_files/figure-gfm/combine%20impacts%20figures-1.png)<!-- -->

``` r
impacts_fig_both <- plot_grid(impacts_fig,
                            impacts_legend_row,
                            ncol = 1, nrow = 2,
                            rel_heights = c(1, 0.04))
impacts_fig_both
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_R2andR3_likert_figures_files/figure-gfm/combine%20impacts%20figures-2.png)<!-- -->

``` r
save_plot(here::here("figures", "impacts_fig_both.png"), impacts_fig_both, base_width = 183, base_height = 168, units = "mm")
```

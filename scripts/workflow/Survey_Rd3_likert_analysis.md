Survey_Rd3_analysis
================
Arlie McCarthy
2023-10-31

# Introduction and rationale for this phase of analysis

The Delphi process is a method to explore consensus (and dissensus)
around issues and repeated surveys, between which participants see where
their answers lie in relation to the rest of the group, can help
participants reach consensus on certain questions.

This script analyses the data from the 3rd round of surveys on the term
‘Destructive Fishing’. In this round we asked participants again to
answer all questions from round 2 that did not reach our consensus
threshold of 70%. However, in this round we removed the option “Neither
agree nor disagree” and replaced it with “Prefer not to say”.
Participants to answered “Prefer not to say” were asked to explain why
they did not wish to answer the question.

This script, therefore, identifies whether consensus has been reached on
any of the 25 statements asked again in this survey, creates individual
figures for feedback for each respondent. It also identifies which
respondents answered differently from the group on questions where
consensus has been reached. These participants will be contacted during
follow-up to explain why they take that point of view. This is a
critical part of the process, since differing views (even in a minority)
can be extremely important parts of the discussion, especially where
there is potential for bias in the group of participants.

We hope that by identifying ares of consensus and dissensus around the
term “destructive fishing” we can inform discussions in policy fora and
any future definition-setting exercises.

## Import survey questions and responses

``` r
r3_likert_responses <- read_excel(here::here("data", "Rd3_Results_Final.xlsx"), sheet = "responses_wider")
r3_responses_longer <- read_excel(here::here("data", "Rd3_Results_Final.xlsx"), sheet = "responses")
```

    ## New names:
    ## • `3.1` -> `3.1...19`
    ## • `3.1` -> `3.1...21`
    ## • `3.1` -> `3.1...37`

``` r
r3_likert_questions <- read_excel(here::here("data", "Rd3_Results_Final.xlsx"), sheet = "questions")
r2_responses_longer <- read_excel(here::here("data", "R2_responses.xlsx"), sheet = "responses")
r3_likert_questions <- r3_likert_questions %>% 
  mutate(question_with_number = paste(q_number_rd3, question))

tab <- r3_likert_responses %>% 
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
mylevels<-c("Strongly agree","Somewhat agree", "Somewhat disagree", "Strongly disagree", "Prefer not to say")
mybreaks <- c("#0571B0", "#92C5DE", "#F4A582", "#CA0020", "#ffffff")

tab$agree = tab$`Strongly agree` + tab$`Somewhat agree`
tab$disagree = tab$`Strongly disagree` + tab$`Somewhat disagree`
tab$sum_pcent = tab$`Strongly agree` + tab$`Somewhat agree` + tab$`Strongly disagree` + tab$`Somewhat disagree`
```

### General Information

What do we know about the people who responded to round 3?

Round 3 had 42 responses (39 in English and 2 in Spanish). Most
respondents were from Civil Society (17 total, 15 from environmental
NGOs, 1 from other NGO, 1 from small-scale fisheries/Rights holder
institution), 12 were from the fishing industry (5 from commercial
fishing, 7 from other fishing industry), 6 respondents worked in
governmental fisheries management, 5 in academia, and 2 for an
intergovernmental body (see table below).

``` r
r3_responses_longer %>% 
  group_by(sector) %>% 
  dplyr::summarise(sector_n = n())
```

    ## # A tibble: 8 × 2
    ##   sector                                                          sector_n
    ##   <chr>                                                              <int>
    ## 1 Academia                                                               5
    ## 2 Civil society (Small-scale fisheries/Rights holder institution)        1
    ## 3 Civil society (environmental NGO)                                     15
    ## 4 Civil society (other NGO)                                              1
    ## 5 Government (fisheries management)                                      6
    ## 6 Industry (commercial fishing)                                          5
    ## 7 Industry (other)                                                       7
    ## 8 Intergovernmental body                                                 2

Round 2 of the survey had 54 responses (51 in English and 3 in Spanish).
Respondents were from 25 countries and 6 continents (Europe 56%, Oceania
15%, South America 9%, Asia 7%, North America 7%, Africa 6%). Experts
worked across 29 countries and 6 continents (Europe 46%, Asia 17%,
Oceania 15%, South America 7%, North America 7%, Africa 7%). Experts had
worked in all listed ocean regions (Indian, North and South Pacific,
North and South Atlantic, Southern Ocean), with 20 of 54 37% of
respondents having worked in multiple regions.

To do glean any other demographic information from round 3 we need to
match the names with the demographic information collected in Round 2.

``` r
dem_info <- r2_responses_longer[,1:12]
dem_info_r3 <- r3_responses_longer[,1:4] %>% 
  full_join(dem_info) %>% 
  arrange(Name)
```

    ## Joining with `by = join_by(Lang, Name, email, sector)`

Now that we have the percentages we can check which questions have
reached consensus. sum_pcent is the percentage of respondents who
answered the question, i.e. anyone who answered “Prefer not to say” has
been removed so that we can show the different response rates between
questions.

``` r
tab$question_with_number<-str_wrap(tab$question_with_number, width = 50) #this is here becuase the tab_nonconsensus dataframe is used later for filtering and needs to have the same question_with_number as later versions
tab_consensus <- tab %>% 
  filter(agree > 0.7 |
                 agree < 0.3)
tab_nonconsensus <- tab %>% 
  anti_join(tab_consensus)
```

    ## Joining with `by = join_by(question_with_number, `Strongly disagree`, `Somewhat
    ## disagree`, `Prefer not to say`, `Somewhat agree`, `Strongly agree`, q_number,
    ## q_number_rd3, q_group, q_start_phrase, question, broad_category, theme,
    ## narrow_theme, short_question, agree, disagree, sum_pcent)`

11 of the questions asked in Rd 3 have reached consensus threshold
(70%). Therefore, will need to follow up with the respondents who
disareed with the consensus for about . Leave the rest as topics for
which no consensus could be reached in this process.

## Figures

### Prepare data for plotting

Not exactly the same as for the round 2 data because we had slightly
different response options. Need to remove the midvalues as we now only
have agree/disagree options. However, it would be helpful to include the
number of respondents for each because we had the option of “Prefer not
to say”.

``` r
tab$polarisation <- tab$agree - tab$disagree # scores closer to 0 will be the most polarised
tab2<- tab %>% 
  dplyr::select(question_with_number, `Strongly disagree`, `Somewhat disagree`, `Somewhat agree`, `Strongly agree`, broad_category:short_question)
tab2 <- tab2  %>% 
  arrange(question_with_number) 
#Save into an excel sheet for others to see
#write.xlsx(x = as.data.frame(tab), file = here::here("Survey_Results_Summary.xlsx"),
      #sheetName = "Round_3", append = TRUE, showNA = FALSE)

tab3 <- tab2 %>% 
  pivot_longer(cols = c(2:5)) %>%
  arrange(desc(question_with_number))
tab3 <- tab3 %>% 
  mutate(col = case_when(name == "Strongly disagree" ~ "#CA0020",
                         name == "Somewhat disagree" ~ "#F4A582",
                         name == "Somewhat agree" ~ "#92C5DE",
                         name == "Strongly agree" ~ "#0571B0"))
tab3$value<-tab3$value*100
#tab3$question<-factor(tab3$question, levels = #tab2$question[order(-(tab2[,2]+tab2[,3]+tab2[,4]))])
#tab3 <- tab3 %>% group_by(broad_category, theme, narrow_theme, short_question)

highs<-tab3 %>% dplyr::filter(name == "Somewhat agree" |
                                 name == "Strongly agree") %>% 
  mutate(question_with_number = factor(question_with_number, levels= c(
        "5.4 Only some practices that use 'legitimate'\nfishing gears can be adapted or mitigated to\nreduce their impact to an appropriate level",
    "5.3 All practices that use 'legitimate' fishing\ngears can be adapted or mitigated to reduce their\nimpact to an appropriate level",
    "5.2 Poor or low capacity management - rather\nthan specific fishing practices - determines how\ndestructive a fishing activity is",
    "5.1 All fishing practices can be destructive (if\nnot carried out appropriately or maintained at an\nappropriate level)",
    "4.4 Shares some characteristics with the term\n'illegal, unreported and unregulated (IUU)\nfishing'",
    "4.3 Shares some characteristics with the term\n'overfishing'",
    "4.2 Is the same (conceptually) as the term\n'fishing that causes significant adverse impact'",
    "4.1 Is the same (conceptually) as the term\n'illegal, unreported and unregulated (IUU)\nfishing'",
    "3.10 Fishing activity (or any other anthropogenic\nactivity) that infringes the legitimate rights of\nother fishers is destructive",
    "3.9 Proportional loss of a target species from\nfishing that exceeds its biologically safe limits\nis destructive",
    "3.8 Any mortality or injury to a non-target\nspecies from fishing (or any other anthropogenic\nactivity) is destructive",
    "3.7 Any damage to a seabed habitat from fishing\n(or any other anthropogenic activity) is\ndestructive",
    "3.6 Relates to the economic impact of financial\nloss to national economies",
    "3.5 Relates to the economic impact of financial\nloss to fishers/fishing enterprises",
    "3.4 Relates to the social impact of human rights\nviolation",
    "3.3 Relates to the social impact of conflict\nbetween sectors",
    "3.2 Relates to the social impact of traditional\nculture loss/degradation",
    "3.1 Relates to the social impact of traditional\nlivelihood loss/degradation",
    "2.7 Cannot be expressed universally (i.e. is\nalways context-dependent)",
    "2.6 Describes changes/impacts that are avoidable",
    "2.5 Describes changes/impacts that are reversible\nover any time-scale",
    "2.4 Describes changes/impacts that are\nirreversible over any time scale",
    "2.3 Can describe an economic concept",
    "2.2 Can describe a social concept",
    "2.1 Has changed in its meaning over time"
)))
lows <-tab3 %>% dplyr::filter(name == "Somewhat disagree" |
                                 name == "Strongly disagree") %>% 
  mutate(question_with_number = factor(question_with_number, levels= c(
        "5.4 Only some practices that use 'legitimate'\nfishing gears can be adapted or mitigated to\nreduce their impact to an appropriate level",
    "5.3 All practices that use 'legitimate' fishing\ngears can be adapted or mitigated to reduce their\nimpact to an appropriate level",
    "5.2 Poor or low capacity management - rather\nthan specific fishing practices - determines how\ndestructive a fishing activity is",
    "5.1 All fishing practices can be destructive (if\nnot carried out appropriately or maintained at an\nappropriate level)",
    "4.4 Shares some characteristics with the term\n'illegal, unreported and unregulated (IUU)\nfishing'",
    "4.3 Shares some characteristics with the term\n'overfishing'",
    "4.2 Is the same (conceptually) as the term\n'fishing that causes significant adverse impact'",
    "4.1 Is the same (conceptually) as the term\n'illegal, unreported and unregulated (IUU)\nfishing'",
    "3.10 Fishing activity (or any other anthropogenic\nactivity) that infringes the legitimate rights of\nother fishers is destructive",
    "3.9 Proportional loss of a target species from\nfishing that exceeds its biologically safe limits\nis destructive",
    "3.8 Any mortality or injury to a non-target\nspecies from fishing (or any other anthropogenic\nactivity) is destructive",
    "3.7 Any damage to a seabed habitat from fishing\n(or any other anthropogenic activity) is\ndestructive",
    "3.6 Relates to the economic impact of financial\nloss to national economies",
    "3.5 Relates to the economic impact of financial\nloss to fishers/fishing enterprises",
    "3.4 Relates to the social impact of human rights\nviolation",
    "3.3 Relates to the social impact of conflict\nbetween sectors",
    "3.2 Relates to the social impact of traditional\nculture loss/degradation",
    "3.1 Relates to the social impact of traditional\nlivelihood loss/degradation",
    "2.7 Cannot be expressed universally (i.e. is\nalways context-dependent)",
    "2.6 Describes changes/impacts that are avoidable",
    "2.5 Describes changes/impacts that are reversible\nover any time-scale",
    "2.4 Describes changes/impacts that are\nirreversible over any time scale",
    "2.3 Can describe an economic concept",
    "2.2 Can describe a social concept",
    "2.1 Has changed in its meaning over time"
)))
```

### Figure of overall responses

``` r
Rd_3_results_fig <- ggplot() + geom_bar(data=highs, aes(x = question_with_number, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = question_with_number, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  geom_hline(yintercept = -70, color =c("black")) +
  geom_hline(yintercept = 70, color =c("black")) +
  scale_fill_identity("Response types", labels = mylevels, breaks = mybreaks, guide="legend") +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 10)) +
  theme_fivethirtyeight() + 
  coord_flip() +
  labs(y="Percentage of responses that agree or disagree with the statement",
       x="",
       title = "Responses to statements asked in Round 3",
       caption = "Figure shows all responses for the 25 statements asked again in round 3, because they did not reach a threshold of 70% consensus in round 2.\nResponses are shown as a percentage of all responses. Positive values indicate agreement with the statement, negative values indicate disagreement.") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = c(0.15, 0.93),
        legend.direction = "vertical")

Rd_3_results_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd3_likert_analysis_files/figure-gfm/rd3_response_figure-1.png)<!-- -->

``` r
ggsave2(here::here("figures", "Rd_3_results_figure.pdf"),
  plot = Rd_3_results_fig,
  device = "pdf",
  width = 30,
  height = 35,
  units = "cm")
ggsave2(here::here("figures", "Rd_3_results_figure.jpg"),
  plot = Rd_3_results_fig,
  device = "jpg",
  width = 30,
  height = 35,
  units = "cm")
```

### Figure of Questions that did not reach consensus

First, we need to filter the incoming data to ensure that only the
non-consensus questions remain.

``` r
highs_noncon <- highs %>% 
  filter(question_with_number %in% tab_nonconsensus$question_with_number)
lows_noncon <- lows %>% 
  filter(question_with_number %in% tab_nonconsensus$question_with_number)
```

Then we generate the figure, as above, but using the filtered data to
show only questions that did not reach consensus.

``` r
Rd_3_results_noncon_fig <- ggplot() + geom_bar(data=highs_noncon, aes(x = question_with_number, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows_noncon, aes(x = question_with_number, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  geom_hline(yintercept = -70, color =c("black")) +
  geom_hline(yintercept = 70, color =c("black")) +
  scale_fill_identity("Response types", labels = mylevels, breaks = mybreaks, guide="legend") +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 10)) +
  theme_fivethirtyeight() + 
  coord_flip() +
  labs(y="Percentage of responses that agree or disagree with the statement",
       x="",
       title = "Questions asked in Round 3 that did not reach consensus",
       caption = "Figure shows all responses for the 14 statements that did not reach a threshold of 70% consensus in round 3, shown as a percentag\nof all responses. Positive values indicate agreement with the statement, negative values indicate disagreement.") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = c(0.15, 0.93),
        legend.direction = "vertical")

Rd_3_results_noncon_fig
```

![](/Users/armcca001/Documents/Dokumente%20-%20ohif01m019/Projects/Destructive%20Fishing/destructive_fishing_defintion_delphi/scripts/workflow/Survey_Rd3_likert_analysis_files/figure-gfm/rd3_response_figure%20non-consensus-1.png)<!-- -->

``` r
ggsave2(here::here("figures", "Rd_3_results_nonconsensus_figure.pdf"),
  plot = Rd_3_results_noncon_fig,
  device = "pdf",
  width = 30,
  height = 20,
  units = "cm")
ggsave2(here::here("figures", "Rd_3_results_nonconsensus_figure.jpg"),
  plot = Rd_3_results_noncon_fig,
  device = "jpg",
  width = 30,
  height = 20,
  units = "cm")
```

## Identifying respondents for follow-up

As part of the Delphi method we need to ask respondents who had strong
opinions on the questions that did not reach consensus, why the answered
the way they did. Adding qualitative information to the responses will
help identify areas that might need further discussion in any
definition-setting process.

``` r
`%notin%` <- Negate(`%in%`)
follow_up_responses <- r3_likert_responses %>% 
  filter(Name %notin% tab_consensus$q_number_rd3)
```

Now that I have a dataframe with just the respondents with at least 1
strong response to a non-consensus question, I want to create a little
table for each person that shows only the questions they had strong
responses to.

``` r
col_nos <- list(2:41)
resp_names <- colnames(follow_up_responses[,2:41])
follow_up_response_tables <- list()
for( i in 1:length(resp_names)) {
  individual_response <- follow_up_responses %>% 
    select(1, resp_names[[i]]) %>% 
    filter(.data[[resp_names[[i]]]] != "Somewhat agree" &
           .data[[resp_names[[i]]]] != "Somewhat disagree")
  follow_up_response_tables[[i]] <- knitr::kable(individual_response)
}
```

Now that we have them, we need to display them all so that we can
contact repondents for follow-up. This code is not run here because it
connects individuals to their responses.

    follow_up_response_tables[[1]]
    follow_up_response_tables[[2]]
    follow_up_response_tables[[3]]
    follow_up_response_tables[[4]]
    follow_up_response_tables[[5]]
    follow_up_response_tables[[6]]
    follow_up_response_tables[[7]]
    follow_up_response_tables[[8]]
    follow_up_response_tables[[9]]
    follow_up_response_tables[[10]]
    follow_up_response_tables[[11]]
    follow_up_response_tables[[12]]
    follow_up_response_tables[[13]]
    follow_up_response_tables[[14]]
    follow_up_response_tables[[15]]
    follow_up_response_tables[[16]]
    follow_up_response_tables[[17]]
    follow_up_response_tables[[18]]
    follow_up_response_tables[[19]]
    follow_up_response_tables[[20]]
    follow_up_response_tables[[21]]
    follow_up_response_tables[[22]]
    follow_up_response_tables[[23]]
    follow_up_response_tables[[24]]
    follow_up_response_tables[[25]]
    follow_up_response_tables[[26]]
    follow_up_response_tables[[27]]
    follow_up_response_tables[[28]]
    follow_up_response_tables[[29]]
    follow_up_response_tables[[30]]
    follow_up_response_tables[[31]]
    follow_up_response_tables[[32]]
    follow_up_response_tables[[33]]
    follow_up_response_tables[[34]]
    follow_up_response_tables[[35]]
    follow_up_response_tables[[36]]
    follow_up_response_tables[[37]]
    follow_up_response_tables[[38]]
    follow_up_response_tables[[39]]
    follow_up_response_tables[[40]]

library(tidyverse)
library(readr)
library(lubridate)

agemerge4 <- read_csv("inventory.csv")
str(agemerge4)

agemerge4$`Date Planted`<-ymd(agemerge4$`Date Planted`)
unique(agemerge4$SPECIES)

updatedTagID300365 <- agemerge4 %>%
  filter(`Collection Site` == "CP"| `Collection Site` == "OC" |
           `Collection Site` == "PB" | `Collection Site` == "STEER") %>%
  mutate(CurrentDate = ymd("2027-01-25")) %>%
  mutate(TimeInNursery =(CurrentDate-`Date Planted`)) %>%
  filter(TimeInNursery > 300 & TimeInNursery <= 365) %>%
  select(`Collection Site`,`Tag ID`, `Tank Number`, QUADRANT)

length(which(updatedTagID300365$`Collection Site` == "PB")) #167
length(which(updatedTagID300365$`Collection Site` == "OC")) #36
length(which(updatedTagID300365$`Collection Site` == "STEER")) #72

write.csv(updatedTagID300365 , "Big3Sites_TagIDs.csv", row.names = FALSE)

View(updatedTagID300365)

oneweekmargin <- agemerge4 %>%
filter(`Collection Site` == "CP"| `Collection Site` == "OC" |
        `Collection Site` == "PB" | `Collection Site` == "STEER") %>%
  mutate(CurrentDate = ymd("2027-01-18")) %>%
  mutate(TimeInNursery =(CurrentDate-`Date Planted`)) %>%
  mutate(AgeCat = case_when(
    TimeInNursery >= 83 & TimeInNursery <= 97  ~ "3 Months",
    TimeInNursery >= 173 & TimeInNursery <= 207 ~ "6 Months",
    TimeInNursery >= 263 & TimeInNursery <= 307 ~ "9 Months",
    TimeInNursery >= 353 & TimeInNursery <= 367 ~ "12 Months",
    TimeInNursery >= 443 & TimeInNursery <= 457 ~ "15 Months",
    TimeInNursery >= 533 & TimeInNursery <= 547 ~ "18 Months",
    TimeInNursery >= 623 & TimeInNursery <= 637 ~ "21 Months",
    TimeInNursery >= 713 & TimeInNursery <= 727 ~ "24 Months",
    TRUE ~ NA_character_)) %>%
  group_by(AgeCat, `Collection Site`) %>%
  select(`Tag ID`)
  summarise(n=n())
View(oneweekmargin)


oneweekmargin$AgeCat <- factor(oneweekmargin$AgeCat, levels = c("3 Months", "6 Months", "9 Months", "12 Months",
                                                                "15 Months", "18 Months", "21 Months", "24 Months"))

oneweekmargin_wide <- oneweekmargin %>%
  pivot_wider(names_from = `Collection Site`, values_from = n)
View(oneweekmargin_wide)

twoweekmargin <- agemerge4 %>%
  filter(`Collection Site` == "CP"| `Collection Site` == "OC" |
           `Collection Site` == "PB" | `Collection Site` == "STEER") %>%
  mutate(CurrentDate = ymd("2027-01-18")) %>%
  mutate(TimeInNursery =(CurrentDate-`Date Planted`)) %>%
  mutate(AgeCat = case_when(
    TimeInNursery >= 76 & TimeInNursery <= 104  ~ "3 Months",
    TimeInNursery >= 166 & TimeInNursery <= 214 ~ "6 Months",
    TimeInNursery >= 256 & TimeInNursery <= 314 ~ "9 Months",
    TimeInNursery >= 346 & TimeInNursery <= 374 ~ "12 Months",
    TimeInNursery >= 436 & TimeInNursery <= 464 ~ "15 Months",
    TimeInNursery >= 526 & TimeInNursery <= 554 ~ "18 Months",
    TimeInNursery >= 616 & TimeInNursery <= 644 ~ "21 Months",
    TimeInNursery >= 706 & TimeInNursery <= 734 ~ "24 Months",
    TRUE ~ NA_character_)) %>%
  group_by(`Collection Site`, AgeCat) %>%
  summarise(n=n())
View(twoweekmargin)

twoweekmargin$AgeCat <- factor(twoweekmargin$AgeCat, levels = c("3 Months", "6 Months", "9 Months", "12 Months",
                                                                "15 Months", "18 Months", "21 Months", "24 Months"))

twoweekmargin_wide <- twoweekmargin %>%
  pivot_wider(names_from = `Collection Site`, values_from = n)
View(twoweekmargin_wide)

threeweekmargin <- agemerge4 %>%
  filter(`Collection Site` == "CP"| `Collection Site` == "OC" |
           `Collection Site` == "PB" | `Collection Site` == "STEER") %>%
  mutate(CurrentDate = ymd("2027-01-18")) %>%
  mutate(TimeInNursery =(CurrentDate-`Date Planted`)) %>%
  mutate(AgeCat = case_when(
    TimeInNursery >= 69 & TimeInNursery <= 111  ~ "3 Months",
    TimeInNursery >= 159 & TimeInNursery <= 221 ~ "6 Months",
    TimeInNursery >= 249 & TimeInNursery <= 321 ~ "9 Months",
    TimeInNursery >= 339 & TimeInNursery <= 381 ~ "12 Months",
    TimeInNursery >= 429 & TimeInNursery <= 471 ~ "15 Months",
    TimeInNursery >= 519 & TimeInNursery <= 561 ~ "18 Months",
    TimeInNursery >= 609 & TimeInNursery <= 651 ~ "21 Months",
    TimeInNursery >= 699 & TimeInNursery <= 741 ~ "24 Months",
    TRUE ~ NA_character_)) %>%
  group_by(`Collection Site`, AgeCat) %>%
  summarise(n=n())
View(threeweekmargin)

threeweekmargin$AgeCat <- factor(threeweekmargin$AgeCat, levels = c("3 Months", "6 Months", "9 Months", "12 Months",
                                                                "15 Months", "18 Months", "21 Months", "24 Months"))

threeweekmargin_wide <- threeweekmargin %>%
  pivot_wider(names_from = `Collection Site`, values_from = n)
View(threeweekmargin_wide)

monthmargin <- agemerge4 %>%
  filter(`Collection Site` == "CP"| `Collection Site` == "OC" |
           `Collection Site` == "PB" | `Collection Site` == "STEER") %>%
  mutate(CurrentDate = ymd("2027-01-18")) %>%
  mutate(TimeInNursery =(CurrentDate-`Date Planted`)) %>%
  mutate(AgeCat = case_when(
    TimeInNursery >= 62 & TimeInNursery <= 118  ~ "3 Months",
    TimeInNursery >= 152 & TimeInNursery <= 228 ~ "6 Months",
    TimeInNursery >= 242 & TimeInNursery <= 328 ~ "9 Months",
    TimeInNursery >= 332 & TimeInNursery <= 388 ~ "12 Months",
    TimeInNursery >= 422 & TimeInNursery <= 478 ~ "15 Months",
    TimeInNursery >= 512 & TimeInNursery <= 568 ~ "18 Months",
    TimeInNursery >= 602 & TimeInNursery <= 658 ~ "21 Months",
    TimeInNursery >= 692 & TimeInNursery <= 748 ~ "24 Months",
    TRUE ~ NA_character_)) %>%
  group_by(`Collection Site`, AgeCat) %>% 
  filter(AgeCat == "9 Months" | AgeCat == "12 Months") %>%
  select(`Tag ID`, `Tank Number`, `QUADRANT`) #%>%
  summarise(n=n())
View(monthmargin)

write.csv(monthmargin, "Big4tagIDs.csv", row.names = FALSE)

monthmargin$AgeCat <- factor(monthmargin$AgeCat, levels = c("3 Months", "6 Months", "9 Months", "12 Months",
                                                                    "15 Months", "18 Months", "21 Months", "24 Months"))

monthmargin_wide <- monthmargin %>%
  pivot_wider(names_from = `Collection Site`, values_from = n)
View(monthmargin_wide)




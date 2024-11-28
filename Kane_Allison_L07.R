# L07 - strings ----
# Stat 301-1

## load packages ----
library(tidyverse)
library(stringi)
library(htmlwidgets)
library(babynames)

## Exercises ----

### Ex 1 ----

# Create strings that contain the following values:

# 1. `He said "That's amazing!"`

string1 <- "He said 'That's amazing!'"

str_view(string1)

# 2. `\a\b\c\d`

string2 <- "\\a\\b\\c\\d"
 
str_view(string2)

# 3. `\\\\\\`
string3 <- "\\\\\\\\\\\\"

str_view(string3)

### Exercise 2---

# Compare and contrast the results of `paste0()` with `str_c()` for the following inputs:

# str_c output
str_c("hi ", NA)
str_c(letters[1:2], letters[1:3])

# playing around with str_c

str_c(letters[1:2], letters[1:2])
str_c(letters[1:3], letters[1:3])

# paste0 output
paste0("hi ", NA)
paste0(letters[1:2], letters[1:3])


### Exercise 3----

# What's the difference between `paste()` and `paste0()`?

# paste0() concatenates between inputs directly with no separator included
# paste() concatenates between inputs but includes a separator between each
# if there is no specified sep argument, the default is a space between each input.

# How can you recreate the equivalent of `paste()` with `str_c()`?

paste("hi", "there")
str_c("hi", "there", sep = " ")

# str_c doesn't require a separator but paste() does
# can recreate by adding a separator argument?


### Exercise 4----

# Write a sentence that contains a Greek letter.

# greek letter mu encoded by \u00b5

str_view('1000 \u00b5L is equivalent to 1 mL')


### Exercise 5----

# Use `str_length()` and `str_sub()` to extract the middle letter from each baby name, 
# use data `babynames::babynames`. 

babynames |> 
  mutate(
    name_length = str_length(name), 
    name_length = ifelse(name_length %% 2 == 0, name_length + 1, name_length), # adjust for even number lengths
    middle_length = name_length / 2 +.5,
    middle_letter = str_sub(name, start = middle_length, end = middle_length)
  )

# What will you do if the string has an even number of characters?

# If the string has an even number of characters, I will add oone to the name length.
# When determining the middle letter, this will select the middle letter closer to the end of the name.


### Exercise 6----

# Using `babynames::babynames`, what baby name has the most vowels? 
# What name has the highest proportion of vowels?

# most vowels
most_vowels <- babynames |> 
  mutate(
    vowel_count = str_count(name, "[aeiouAEIOU]"),
  ) |> 
  arrange(desc(vowel_count))

# Mariadelrosario and Mariaguadalupe

# highest proportion
babynames |> 
  mutate(
    name_length = str_length(name), 
    vowel_count = str_count(name, "[aeiouAEIOU]"),
    vowel_prop = vowel_count / name_length
  ) |> 
  arrange(desc(vowel_prop)) |> 
  distinct(name)

# Eua, Ea, Ai, Ii, Aoi, and Aia

### Exercise 7----

# Implement a simple version of `str_to_lower()` using `str_replace_all()`.

simple_str_to_lower <- function(string_input) {
  str_replace_all(string_input, c("A" = "a", "B" = "b", "C" = "c", "D" = "d", 
                                  "E" = "e", "F" = "f", "G" = "g", "H" = "h", 
                                  "I" = "i", "J" = "j", "K" = "k", "L" = "l",
                                  "M" = "m", "N" = "n", "O" = "o", "P" = "p", 
                                  "Q" = "q", "R" = "r", "S" = "s", "T" = "t",
                                  "U" = "u", "V" = "v", "W" = "w", "X" = "x",
                                  "Y" = "y", "Z" = "z"))
}

simple_str_to_lower("HI! HOW ARE YOU TODAY? THIS WILL BE LOWERCASE")

### Exercise 8----
# Create a regular expression that will match telephone numbers as commonly written in your country. 
# Demonstrate a case when it matches and one where it doesn't match (an improper phone number).

# in the form XXX-XXX-XXXX

expression <- "^\\d{3}-\\d{3}-\\d{4}$"

phone_hyphenated_valid <- "884-727-9012"

str_detect(phone_hyphenated_valid, expression)

phone_hyphenated_invalid <- "909-12-9791"

str_detect(phone_hyphenated_invalid, expression)


### Exercise 9----

# How would you match the literal string `"'\`? How about `"$^$"`?

# `"'\`? 

string_input_a <- c("\"'\\", "\"", "'", "\\\\")

# recognizing the "
str_detect(string_input_a, pattern = "\"")

# recognizing the '
str_detect(string_input_a, pattern = "'")

# recognizing last two slashes \\
str_detect(string_input_a, pattern = "\\\\")

# putting it together
str_detect(string_input_a, pattern = "\"'\\\\")


# `"$^$"``

string_input_b <- c("\"$^$\"", "hello", "^", "$", "\"")

# recognizing the \"
str_detect(string_input_b, pattern = "\"")

# recognizing the $
str_detect(string_input_b, pattern = "\\$")

# recognizing the ^
str_detect(string_input_b, pattern = "\\^")

# put it together 
str_detect(string_input_b, pattern = "\"\\$\\^\\$\"")


### Exercise 10----

# Explain why each of these patterns don't match a `\`: 

# to write `\` as a string:

str_view("\\")

string_ex_10 <- "\\"

# a. `"\"`

# This escapes without including anything after it

# b. `"\\"`

# This escapes and includes a first slash. However, in order to match `\` as a string,
#  i.e. "\\" two backslashes are required.

# c. `"\\\"`.

# This escapes and includes a first slash. The third backslash escapes but does not 
# include the final backslash to match the expression. 


### Exercise 11----

# Given the corpus of common words in `stringr::words`, create regular expressions that find all words that:

words_tibble <- tibble(words)

# a.  Start with "y".

words_tibble |>
  filter(str_starts(words, "^y"))

# b.  Don't start with "y".

words_tibble |>
  filter(str_starts(words, "[^y]"))

# c.  End with "x".

words_tibble |>
  filter(str_ends(words, "x"))

words_tibble |>
  filter(str_detect(words, "x$"))

# d.  Are exactly three letters long. (Don't cheat by using `str_length()`!)

# words_tibble |> 
#   filter(str_length(words) == 3)

words_tibble |>
  filter(str_detect(words, "^...$"))

# e.  Have seven letters or more.

# words_tibble |> 
#   filter(str_detect(words, "^......."))

words_tibble |>
  filter(str_detect(words, "^.{7}"))

# f.  Contain a vowel-consonant pair.

words_tibble |>
  filter(str_detect(words, "[aeiou][^aeiou]"))

# g.  Contain at least two vowel-consonant pairs in a row.

words_tibble |>
  filter(str_detect(words, "([aeiou][^aeiou]){2}"))

# h.  Only consist of repeated vowel-consonant pairs.

words_tibble |>
  filter(str_detect(words, "^([aeiou][^aeiou]){1,}$"))

### Exercise 12----

# Describe in words what these regular expressions match: 
#(read carefully to see if each entry is a regular expression or a string that defines a regular expression.)

# a.  `^.*$`
# This regular expression matches any string, as it includes ^ and $ on start and ends respectively. 

# b.  `"\\{.+\\}"`
# This string that defines a regular expression matches for a curly bracket, 
# then any amount of characters, and then another curly bracket

# c.  `\d{4}-\d{2}-\d{2}`

# This regular expression matches for any 4 digits (ranging from 0-9), a dash, 
# any two digits (ranging from 0-9), a dash, and any two digits (ranging from 0-9).

# d.  `"\\\\{4}"`

# This string defines a regular expression that matches for a back slash occurring 4 times.

# e.  `\..\..\..`

# This regular expression matches for three dots separated by any characters, for
# example: hi.how.are.you

# f.  `(.)\1\1`

# This regular expression is used to match any three characters in a row, for example:
# aaa 

# g.  `"(..)\\1"`

# This string defines a regular expression that is used to match a double repeat of two characters.
# for example, it would match the anan in banana


### Exercise 13----

# For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple `str_detect()` calls.

# a. Find all `words` that start or end with `x`.

words_tibble |>
  filter(str_detect(words, "x$"))

words_tibble |>
  filter(str_detect(words, "^x"))

words_tibble |>
  filter(str_detect(words, "x$") | str_detect(words, "^x"))

words_tibble |>
  filter(str_detect(words, "^x|x$"))

# b. Find all `words` that start with a vowel and end with a consonant.
words_tibble |>
  filter(str_detect(words, "^[aeiou]"))

words_tibble |>
  filter(str_detect(words, "[^aeiou]$"))

words_tibble |>
  filter(str_detect(words, "^[aeiou]") & str_detect(words, "[^aeiou]$"))

words_tibble |>
  filter(str_detect(words, "^[aeiou].{1,}[^aeiou]$"))

# c. Are there any `words` that contain at least one of each different vowel?

words_tibble |> 
  filter(str_detect(words, "a") &
           str_detect(words, "e") &
           str_detect(words, "i") &
           str_detect(words, "o") &
           str_detect(words, "u"))

### Exercise 14----
# Construct patterns to find evidence for and against the rule "i before e except after c"?

words_cei <- words_tibble |> 
  filter(str_detect(words, "c") & str_detect(words, "e") & str_detect(words, "i"))

# for 
words_cei |> 
  filter(str_detect(words, "^.*c.*ei.*$"))

words_cei |> 
  filter(!str_detect(words, "^.*c.*ie.*$"))

# against
words_cei |> 
  filter(str_detect(words, "^.*cie.*$"))


### Case Study

# Use the `tinder_profile` file in the `data` subdirectory to complete the tasks 

# - Obtain the count of users that display their `school` on their profile
# - Obtain the count of users that display their `job` on their profile

tinder_profile <- read_csv("data/tinder_profile.csv")

tinder_profile <- tinder_profile |> 
  mutate(
    schools = ifelse(schools == "[]" | is.na(schools) == TRUE, FALSE, TRUE),
    jobs = ifelse(jobs == "[]" | is.na(jobs) == TRUE, FALSE, TRUE)
  )

# total profiles
tinder_profile |> 
  summarize(
    count = n()
  )

# find school counts
tinder_profile |> 
  filter(schools == TRUE) |> 
  summarize(
    school_count = n()
  )

# find job counts
tinder_profile |> 
  filter(jobs == TRUE) |> 
  summarize(
    job_count = n()
  )
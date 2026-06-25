# Day 10 - Programming basics

# packages ----
library(tidyverse)

# Functions ----

# Why do we need functions? 

fish_tl <- data.frame(
  cod = rnorm(n = 10, mean = 70, sd = 5),
  herring = rnorm(n = 10, mean = 15, sd = 2.5),
  plaice = rnorm(n = 10, mean = 22, sd = 2),
  flounder = rnorm(n = 10, mean = 18, sd = 3)
)
View(fish_tl)

# Where is the mistake and what does this code do?
fish_tl <- fish_tl %>%
  mutate(cod_new = (cod - min(cod, na.rm = TRUE)) / 
                      (max(cod, na.rm = TRUE) - min(cod, na.rm = TRUE)),
         herring_new = (herring - min(herring, na.rm = TRUE)) / 
                          (max(herring, na.rm = TRUE) - min(herring, na.rm = TRUE)),
         plaice_new = (plaice - min(plaice, na.rm = TRUE)) / 
                         (max(plaice, na.rm = TRUE) - min(cod, na.rm = TRUE)),
         flounder_new = (flounder - min(flounder, na.rm = TRUE)) / 
                           (max(flounder, na.rm = TRUE) - min(flounder, na.rm = TRUE))
  )

View(fish_tl)

# Step 1: Identify the parts that are soley repeated and the ones that change: 

# What changes? 

# What stays the same? 

# Step 2: Integrate ths into a basic function:
# Simply copy and paste one of the code blocks as body and change the part that
# varies to the arguments:
# Give your function a meaningful name

norm_species_length <- function(species){
  (species - min(species, na.rm = TRUE)) / 
    (max(species, na.rm = TRUE) - min(species, na.rm = TRUE))
}

norm_species_length(species = c(fish_tl$cod, fish_tl$herring, 
                                fish_tl$plaice, fish_tl$flounder))


# Step 3: Update your code
fish_tl <- fish_tl %>%
  mutate(cod_new = norm_species_length(species = cod),
         herring_new = norm_species_length(species = herring),
         plaice_new = norm_species_length(species = plaice),
         flounder_new = norm_species_length(species = flounder)
  )


## Functions with temporary calculations and return ----
# calculate automatically CPUE values in kg / h: 

calc_cpue <- function(catches_in_lb, effort_in_h){
  catch_in_kg <- catches_in_lb * 0.453592 # temporary calculations, not part of the output
  
  cpue <- catch_in_kg / effort_in_h
  
  return(cpue) # not necessary, but makes the output explicit and improves readibility
  # alternatively you can simply call cpue:
  # cpue
}

calc_cpue(catches_in_lb = 10, effort_in_h = 0.5)
x <- calc_cpue(catches_in_lb = 10, effort_in_h = 0.5)
x


# say you also want to have the effort in the output: 
calc_cpue <- function(catches_in_lb, effort_in_h){
  catch_in_kg <- catches_in_lb * 0.453592 
  
  cpue <- catch_in_kg / effort_in_h
  
  return(list(effort_hours = effort_in_h, # output of the function becomes a list object
              cpue_kg_h = cpue))
}

calc_cpue(10, 0.5) # output is now a list with two names entries
x <- calc_cpue(catches_in_lb = 10, effort_in_h = 0.5)         


# If - else conditions and early exit ----
# test whether effort is reasonable: 
calc_cpue <- function(catches_in_lb, effort_in_h){
  if(effort_in_h <= 0){
    return(NA)
  } 
  # if the if condition returns FALSEm the function simply continues here:
  catch_in_kg <- catches_in_lb * 0.453592 
  
  cpue <- catch_in_kg / effort_in_h
  
  return(list(effort_hours = effort_in_h, 
              cpue_kg_h = cpue))
}

test <- calc_cpue(catches_in_lb = 10, effort_in_h = -0.5) # returns now an NA

# add an if - else condition, e.g. a flag for high values:
calc_cpue <- function(catches_in_lb, effort_in_h){
  if(effort_in_h <= 0){
    return(NA)
  } 
  # if the if condition returns FALSEm the function simply continues here:
  catch_in_kg <- catches_in_lb * 0.453592 
  
  cpue <- catch_in_kg / effort_in_h
  
  # indicate whether status of cpue is low or high
  if(cpue > 50){
    status <- "high"
  } else{
    status <- "low"
  }
  
  return(list(effort_hours = effort_in_h, 
              cpue_kg_h = cpue, 
              status = status))
}

# add values: 
calc_cpue(catches_in_lb = 10000, effort_in_h = 1)

# Loops ----
# Why do you think this doesn´t work? What does the error tell you? 
calc_cpue(catches_in_lb = c(20, 30, 40), effort_in_h = 0.5)


# Suppose we have several catch values --> we have to repeat the same calculation. 
# Instead of calling the function several times, we want that R repeats the same 
# task again and again until it reaches a limit, that we set. 

y <- 1
for(value in c("hello", "i-MARSYS-05", "course")){
  y <- y + 1
}
# For each value in the vector c("hello", "i-MARSYS-05", "course") y will be 
# increased by 1. Value is here similar to an argument in a function. It is changed
# while the loop is running, starting with the first element of your vector and 
# ending with the last one, so you can also treat at like a counter, counting through
# all the elements in the vector. We can see this by explicitly printing the current
# value of the counter to the console: 
y <- 1 # reset y to 1
for(value in c("hello", "i-MARSYS-05", "course")){
  y <- y + 1
  print(value)
}

# the value can be named however you want to and it can take all the values you want. 
# It is more common that it easier is a numeric counter or takes several character values.

# Lets add this knowledge now to our function. We want to ensure that every catch
# that we have is evaluated and save in our output list.
# Step 1 is to identify again which parts must be changeable and which ones shall
# be saved from the loop. 
# Step 2 is to identify how long your for loop should be running
# Step 3 is to create vectors, which are filled up by the for loop
# Step 4 is to put the code that should be repreated into the for loop and add access to each element that shall be evaluated

# Developed in course: 
calc_cpue_advanced <- function(catches_in_lb, effort_in_h){
  if(effort_in_h <= 0){
    return(NA)
  } 
  
  cpue <- rep(0, length(catches_in_lb))
  status <- rep(0, length(catches_in_lb))
  
  for(i in 1:length(catches_in_lb)){
    # if the if condition returns FALSE the function simply continues here:
    catch_in_kg <- catches_in_lb[i] * 0.453592 
    
    cpue[i] <- catch_in_kg / effort_in_h
    
    # indicate whether status of cpue is low or high
    if(cpue[i] > 50){
      status[i] <- "high"
    } else{
      status[i] <- "low"
    }
  }
  
  return(list(effort_hours = effort_in_h, 
              cpue_kg_h = cpue, 
              status = status))
}
calc_cpue_advanced(catches_in_lb = c(20, 30, 40), effort_in_h = 0.5)


# Debugging ----
first <- function() second()
second <- function() third()
third <- function() fourth()
fourth <- function() fifth()
fifth <- function() bug()

first()

# When clicking on traceback R gives you the order in which it called the functions
# until running into an error. The top one is the one causing the error.

new_vector <- c(1, 2, 3, 4, 5, "cats", 6, 7, 8)
y <- 1
for(i in 1:length(new_vector)){
  y <- y + new_vector[i]
}
# add a print statement:
for(i in 1:length(new_vector)){
  print(i)
  y <- y + new_vector[i]
}
# Already at the first element --> Inspect this
new_vector[1]
# its a character, which cannot be added to a numeric value like y
# change type of new_vector
new_vector <- as.numeric(new_vector)
new_vector
for(i in 1:length(new_vector)){
  print(i)
  y <- y + new_vector[i]
}
y # now its NA and not a number anymore...


# Exercises ----
# Suppose you have sample during a survey the length of fishes in cm and now you 
# want to convert them to biomass values. 
# Here are three example values to try out:
# 5, 7, 3
# you can convert the length to biomass values using the following equation:
# biomass = 0.01 *length^3

# 1. Write a function to calculate biomass from length values 

# 2. Your colleague gave you his fish length data. While inspecting the data you 
# saw that some entries are negative, ensure that the function returns NA, whenever
# it hits a negative number. 
# Use 9, 16, -2 as example values-

# 3. add to the output of your function a character value indicating whether the 
# measured fish is small (<60) or big (>= 60)
# try your own data fish_length_cm and the colleague´s data

# 4. Since your function is working for your data and your colleagues data you
# now want to evaluate all of the survey data: 
survey_length_data <- rpois(100, 10)
# Extend the function so that it can process all fish length at once. 

# 5. You get the data of a second survey, which has sometimes also enter 0, 
# whenever it was not possible to measure a fish. Develop a solution how to 
# handle the 0s in the data set. 
survey_length_data_2 <- rpois(100, 10)
survey_length_data_2[survey_length_data_2 == 8] <- 0
survey_length_data_2


# Solutions ----
# 1. Write a function to calculate biomass from length values 
calc_biomass <- function(length_in_cm){
  biomass <- 0.01 * length_in_cm ^ 3
  biomass
  # or alternatively: return(biomass)
}
# This function can currently calculate biomass for single values as well as 
# vectors, because R recycles the equation for every entry in the vector.
calc_biomass(length_in_cm = c(5, 7, 3))
calc_biomass(5)

# 2. Your colleague gave you his fish length data. While inspecting the data you 
# saw that some entries are negative, ensure that the function returns NA, whenever
# it hits a negative number. 
# Use 9, 16, -2 as example values-
calc_biomass <- function(length_in_cm){
  # test for negative length values
  if(length_in_cm < 0){
    return(NA)
  }
  
  biomass <- 0.01 * length_in_cm ^ 3
  biomass
  # or alternatively: return(biomass)
}
# No this function can handle only one value per time, because the if test, does
# not know which of the values in the vector it should test.
calc_biomass(9)
calc_biomass(16)
calc_biomass(-2)

# 3. add to the output of your function a character value indicating whether the 
# measured fish is small (<60) or big (>= 60)
# try your own data fish_length_cm and the colleague´s data

calc_biomass <- function(length_in_cm){
  # test for negative length values
  if(length_in_cm < 0){
    return(NA)
  }
  
  biomass <- 0.01 * length_in_cm ^ 3
  if(biomass < 60){
    size <- "small"
  } else{
    size <- "big"
  }
  data.frame(biomass = biomass, 
             size = size)
}
calc_biomass(5)
calc_biomass(7)
calc_biomass(3)
calc_biomass(9)
calc_biomass(16)
calc_biomass(-2)

# 4. Since your function is working for your data and your colleagues data you
# now want to evaluate all of the survey data: 
survey_length_data <- rpois(100, 10)
# Extend the function so that it can process all fish length at once. 
survey_length_data < 0 
# all values are positive, so we can skip the test for negative values.
calc_biomass <- function(length_in_cm){
  biomass <- rep(0, length(length_in_cm))
  size <- rep(0, length(length_in_cm))
  
  for(i in 1:length(length_in_cm)){
    biomass[i] <- 0.01 * length_in_cm[i] ^ 3
    if(biomass[i] < 60){
      size[i] <- "small"
    } else{
      size[i] <- "big"
    }
  }

  data.frame(biomass = biomass, 
             size = size)
}

calc_biomass(length_in_cm = survey_length_data)

# 5. You get the data of a second survey, which has sometimes also enter 0, 
# whenever it was not possible to measure a fish. Develop a solution how to 
# handle the 0s in the data set. 
survey_length_data_2 <- rpois(100, 10)
survey_length_data_2[survey_length_data_2 == 8] <- 0
survey_length_data_2
calc_biomass <- function(length_in_cm){
  biomass <- rep(0, length(length_in_cm))
  size <- rep(0, length(length_in_cm))
  
  for(i in 1:length(length_in_cm)){
    if(length_in_cm[i] <= 0){
      biomass[i] <- NA
      size[i] <- NA
      next
    }
    biomass[i] <- 0.01 * length_in_cm[i] ^ 3
    if(biomass[i] < 60){
      size[i] <- "small"
    } else{
      size[i] <- "big"
    }
  }
  
  data.frame(biomass = biomass, 
             size = size)
}

calc_biomass(survey_length_data_2)

# Appendix ----
# For loop with explanations as reminder: 
calc_cpue <- function(catches_in_lb, effort_in_h){
  if(effort_in_h <= 0){
    return(NA)
  } 
  
  # create empty vectors for the cpue and status with the length of the 
  # catches that we have
  cpue <- rep(0, times = length(catches_in_lb))
  status <- rep("none", times = length(catches_in_lb))
  
  # Start the for loop here, since we only have several values for catch, not for effort: 
  for(i in 1:length(catches_in_lb)){
    catch_in_kg <- catches_in_lb[i] * 0.453592 # take the i-th element of the catches and convert to kg
    cpue[i] <- catch_in_kg / effort_in_h # calculate cpue and save to the i-th element of the cpue vector
    # evalute the i-th element of cpue for its status and svae to the i-th element of the vector status:
    if(cpue[i] > 50){
      status[i] <- "high"
    } else{
      status[i] <- "low"
    }
    print(paste("done with:", i)) 
  }
  
  return(list(effort_hours = rep(effort_in_h, times = length(catches_in_lb)), 
              cpue_kg_h = cpue, 
              status = status))
}

calc_cpue(catches_in_lb = c(20, 30, 40), effort_in_h = 0.5)




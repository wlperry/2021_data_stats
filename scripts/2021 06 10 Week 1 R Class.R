## Install packages 

## Install these packages only one time and load libraries only once----
install.packages("devtools") # install new things
install.packages("ggThemeAssist") # helps reformat code - only run library one time
install.packages("styler")

## Run these libraries only one time----
library(devtools) # some behind the scenes stuff
library(ggThemeAssist) # modifies ggplots appearance
library(styler) # reformats code to make pretty

# # Packages I use on a regular basis-----
install.packages("tidyverse") # dplyr and piping and ggplot etc
install.packages("lubridate") # dates and times
install.packages("scales") # scales on ggplot ases
install.packages("readxl") # read in excel files
install.packages("skimr") # quick summary stats
install.packages("janitor") # clean up excel imports
install.packages("patchwork") # arrange multiple plots per page
install.packages("plotly") # cool ggplot things
install.packages("colorRamps") # adds cool color templates
install.packages("car") # for anovas
install.packages("emmeans") # adds post f tests

# load libraries each time you run a script ----
## Load the libraries ----
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)
library(plotly)
library(colorRamps)
# special things
library(car)
library(emmeans)


# Operators ---- 
# the key is the assingment operator =   <-   - takes what is on right and puts it in left
# this is difernet than the pipe operatore =   %>%    - which takes stuff on the left 
                                                      # and feeds it to the next command on the rights


# now to load a file ----


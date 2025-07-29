FROM rstudio/plumber
RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev libpng-dev pandoc
RUN R -e "install.packages(c('plumber','tidyverse,'rpart'))"
COPY API.R API.R
EXPOSE
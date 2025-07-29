FROM rstudio/plumber
RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev libpng-dev pandoc
RUN R -e "install.packages(c('plumber','tidyverse','rpart'))"
COPY API.R API.R
EXPOSE 8000
ENTRYPOINT ["R", "-e",\
"pr <-plumber::plumn('API.R');pr$run(host='0.0.0.0',port=8000)"]
# To build an image:
# docker build -t lmsgrowth2 .

# To run, specify directory on host to persist user uploads
# docker run --publish 3838:3838 -v <host dir for uploads>:/srv/shiny-server/LMSgrowth2/inst/uploads lmsgrowth2

FROM rocker/shiny:4.0.4

RUN apt-get update && apt-get -y install libcurl4-gnutls-dev libgit2-dev libssh2-1-dev libssl-dev libv8-dev libxml2-dev

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

RUN R -e "install.packages(c('devtools', 'dplyr', 'shiny', 'shinyjs', 'purrr', 'stringr', 'plotly', 'DT', 'formattable', 'shinyWidgets', 'sitar', 'uuid', 'rio', 'V8', 'shinyhelper', 'tibble'))"

COPY . /srv/shiny-server/LMSgrowth2

# For Azure release, uncomment the two lines below to persist uploads
# RUN rm -rf /srv/shiny-server/LMSgrowth2/inst/uploads
# RUN ln -s /home/shiny/ /srv/shiny-server/LMSgrowth2/inst/uploads

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

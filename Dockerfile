FROM r-base:latest
ARG USER_ID
COPY example.Renviron /.Renviron
RUN apt-get update -y &&\
    apt-get install -y libnode-dev libcurl4-openssl-dev libssl-dev libjq-dev libprotobuf-dev protobuf-compiler make libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev pandoc libfontconfig1-dev libharfbuzz-dev libfribidi-dev &&\
    useradd -l -u ${USER_ID} -g sudo jenkins &&\
    mkdir -m 0755 /home/jenkins && chown jenkins /home/jenkins &&\
    mv /.Renviron /home/jenkins &&\
    echo "jenkins ALL=(root) NOPASSWD: /usr/sbin/service mariadb start" >> /etc/sudoers
RUN R -e "install.packages('remotes')" &&\
    R -e 'remotes::install_github("rspatial/terra")' &&\
    R -e 'install.packages("leaflet")' &&\
    R -e 'install.packages("leaflet.extras")' &&\
    R -e 'install.packages("tidyverse")' &&\
    R -e 'install.packages("htmltools")' &&\
    R -e 'install.packages("readr")' &&\
    R -e 'install.packages("zipcodeR")' &&\
    R -e 'install.packages("stringr")' &&\
    R -e 'install.packages("htmlwidgets")' &&\
    R -e 'install.packages("geojsonio")'
USER jenkins

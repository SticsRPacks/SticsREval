FROM rocker/r-ver:4

COPY . /usr/local/src/SticsREval
WORKDIR /usr/local/src/SticsREval

RUN apt update -y
RUN apt install -y libcurl4-openssl-dev libssl-dev libxslt1.1 libxslt1-dev libxml2-dev

RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')"

RUN R -e "remotes::install_local('.', dependencies = TRUE)"

CMD ["R"]
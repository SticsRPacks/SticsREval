FROM rocker/r-ver:4

COPY . /usr/local/src/SticsREval
WORKDIR /usr/local/src/SticsREval
RUN R -e "install.packages('/usr/local/src/SticsREval', deps = TRUE)"



RUN R -e "library(SticsREval)"
CMD ["R"]
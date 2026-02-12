# syntax=docker/dockerfile:1.4
FROM rocker/r-ver:4

RUN apt update -y && \
    apt install -y \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libxslt1-dev

WORKDIR /stics-r-eval

COPY renv.lock renv.lock
COPY renv/ renv/

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

RUN --mount=type=secret,id=GITHUB_TOKEN,required=false \
    if [ -f /run/secrets/GITHUB_TOKEN ]; then \
      export GITHUB_PAT=$(cat /run/secrets/GITHUB_TOKEN); \
    fi && \
    R -e "renv::restore(prompt = FALSE)"

COPY . .

RUN R CMD INSTALL .

CMD ["/bin/bash"]


# syntax=docker/dockerfile:1.6
FROM rocker/r-ver:4

RUN apt update -y && \
    apt install -y \
      libcurl4-openssl-dev libssl-dev libxml2-dev \
      libxslt1-dev cmake libx11-dev pandoc

WORKDIR /stics-r-eval

ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/latest

COPY renv.lock renv.lock
COPY renv/ renv/

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" 

RUN --mount=type=secret,id=GITHUB_TOKEN,required=false \
    if [ -f /run/secrets/GITHUB_TOKEN ]; then \
      export GITHUB_PAT=$(cat /run/secrets/GITHUB_TOKEN); \
    fi && \
    R -e "renv::restore(prompt = FALSE)"

COPY . .

RUN R CMD INSTALL .

CMD ["/bin/bash"]
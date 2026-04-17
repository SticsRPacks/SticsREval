FROM rocker/r-ver:4

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
      curl ca-certificates git \
      build-essential cmake pkg-config \
      libcurl4-openssl-dev libssl-dev libxml2-dev libxslt1-dev \
      libx11-dev pandoc \
      zlib1g-dev libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /stics-r-eval

ENV R_REPOSITORIES=https://cloud.r-project.org

COPY renv.lock renv.lock
COPY renv/ renv/

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

RUN --mount=type=secret,id=GITHUB_TOKEN,required=false \
    if [ -f /run/secrets/GITHUB_TOKEN ]; then \
      export GITHUB_PAT=$(cat /run/secrets/GITHUB_TOKEN); \
    fi && \
    R -e "options(repos = c(CRAN='https://cloud.r-project.org')); renv::restore(prompt = FALSE)"

COPY . .

RUN R CMD INSTALL .

CMD ["/bin/bash"]
# --- Stage 1 : build ---
FROM rocker/r-ver:4 AS builder

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
      curl ca-certificates git \
      build-essential cmake pkg-config \
      libuv1-dev libcurl4-openssl-dev libssl-dev libxml2-dev libxslt1-dev \
      libx11-dev pandoc \
      zlib1g-dev libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /stics-r-eval

ENV R_REPOSITORIES=https://cloud.r-project.org
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE

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

RUN R -e "\
    lib <- renv::paths\$library(); \
    pkgs <- list.files(lib, full.names = TRUE); \
    file.copy(pkgs, '/usr/local/lib/R/site-library', recursive = TRUE, overwrite = TRUE)"

# --- Stage 2 : runtime ---
FROM rocker/r-ver:4

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
      libcurl4 libssl3 libxml2 libxslt1.1 \
      libx11-6 pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /stics-r-eval

COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

CMD ["/bin/bash"]
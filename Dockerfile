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

ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE

COPY renv.lock renv.lock
COPY renv/ renv/

RUN R -e "install.packages('renv')"

# System libraries required by the R packages of the lockfile
# (P3M binaries need them at load time, source builds need the -dev headers)
RUN Rscript -e "\
      pkgs <- names(renv::lockfile_read('renv.lock')\$Packages); \
      reqs <- unique(unlist(renv::sysreqs(pkgs, report = FALSE))); \
      writeLines(reqs, '/tmp/sysreqs.txt')" \
    && apt-get update -y \
    && xargs -a /tmp/sysreqs.txt apt-get install -y --no-install-recommends \
    && rm -rf /var/lib/apt/lists/* /tmp/sysreqs.txt

RUN --mount=type=secret,id=GITHUB_TOKEN,required=false \
    if [ -f /run/secrets/GITHUB_TOKEN ]; then \
      export GITHUB_PAT=$(cat /run/secrets/GITHUB_TOKEN); \
    fi && \
    R -e "renv::restore(repos = getOption('repos'), prompt = FALSE)"

COPY . .
RUN R CMD INSTALL .

RUN R -e "\
    lib <- renv::paths\$library(); \
    pkgs <- list.files(lib, full.names = TRUE); \
    file.copy(pkgs, '/usr/local/lib/R/site-library', recursive = TRUE, overwrite = TRUE)"

# List the apt packages providing the shared libraries actually linked by the
# installed R packages, so the runtime stage installs only what is needed
RUN find /usr/local/lib/R/site-library -name '*.so' -exec ldd {} + 2>/dev/null \
    | awk '$2 == "=>" && $3 ~ /^\// {print $3}' \
    | sort -u | xargs -r realpath \
    | xargs -r dpkg -S 2>/dev/null \
    | cut -d: -f1 | sort -u > /runtime-deps.txt \
    && cat /runtime-deps.txt

# --- Stage 2 : runtime ---
FROM rocker/r-ver:4

ARG QUARTO_VERSION=1.10.18

COPY --from=builder /runtime-deps.txt /tmp/runtime-deps.txt

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
      curl ca-certificates gdebi-core pandoc \
    && xargs -a /tmp/runtime-deps.txt apt-get install -y --no-install-recommends \
    && rm /tmp/runtime-deps.txt \
    && curl -Lo /tmp/quarto.deb \
      "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-$(dpkg --print-architecture).deb" \
    && gdebi -n /tmp/quarto.deb \
    && rm /tmp/quarto.deb \
    && apt-get purge -y --auto-remove gdebi-core \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /stics-r-eval

COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

CMD ["/bin/bash"]
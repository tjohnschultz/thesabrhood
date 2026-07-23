FROM rocker/r-ver:4.4.1

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      ca-certificates \
      libpq-dev \
      libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY infrastructure/cloud-run/install.R /tmp/install.R
RUN Rscript /tmp/install.R \
    && rm /tmp/install.R

COPY infrastructure/cloud-run /app/infrastructure/cloud-run

ENTRYPOINT ["Rscript", "/app/infrastructure/cloud-run/entrypoint.R"]
CMD ["help"]

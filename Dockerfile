FROM r-base:4.3.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'DT', \
    'rflights', \
    'geodist' \
    ), repos = 'https://cloud.r-project.org')"

WORKDIR /app
COPY . /app

EXPOSE 8080

CMD ["Rscript", "gui.R"]

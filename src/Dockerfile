FROM r-base:4.3.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install remotes first
RUN R -e "install.packages('remotes', repos = 'https://cloud.r-project.org')"

# Install CRAN packages
RUN R -e "install.packages(c( \
    'shiny', \
    'DT', \
    'geodist' \
    ), repos = 'https://cloud.r-project.org')"

# Install rflights from GitHub
RUN R -e "remotes::install_github('jcrodriguez1989/rflights')"

WORKDIR /app
COPY . /app

EXPOSE 8080

CMD ["Rscript", "gui.R"]

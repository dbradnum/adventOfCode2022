FROM ghcr.io/rocker-org/devcontainer/tidyverse:4

RUN install2.r --error --skipinstalled --ncpus -1 \
    skimr \
    && rm -rf /tmp/downloaded_packages \
    && strip /usr/local/lib/R/site-library/*/libs/*.so
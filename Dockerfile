FROM rocker/verse
RUN apt-get update && apt-get install -y locales && \
    locale-gen is_IS.UTF-8 && \
    update-locale LANG=is_IS.UTF-8
ENV LANG is_IS.UTF-8
ENV LANGUAGE is_IS.UTF-8
ENV LC_ALL is_IS.UTF-8
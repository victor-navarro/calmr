FROM rocker/r-ver:4.3.0

Label MAINTAINER Victor Navarro

RUN install2.r remotes progress
RUN installGithub.r victor-navarro/calmr
FROM ubuntu:focal

RUN ln -fs /usr/share/zoneinfo/Europe/Berlin /etc/localtime

RUN DEBIAN_FRONTEND="noninteractive" apt-get update && apt-get -y install \
  youtube-dl \
  pandoc \
  w3m \
  lynx \
  html2text \
  poppler-utils \
  openjdk-16-jdk-headless \
  leiningen \
  wget \
  yarnpkg \
  git \
  diction \
  locales

RUN yarnpkg global add --cwd /tmp @postlight/mercury-parser

RUN wget -O /tmp/pdf2htmlex.deb https://github.com/pdf2htmlEX/pdf2htmlEX/releases/download/v0.18.8.rc1/pdf2htmlEX-0.18.8.rc1-master-20200630-Ubuntu-focal-x86_64.deb && apt-get -y install /tmp/pdf2htmlex.deb && rm -f /tmp/pdf2htmlex.deb

RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

RUN mkdir -p /opt/u1f596
COPY target/u1f596-0.1.0-SNAPSHOT-standalone.jar /opt/u1f596/standalone.jar
CMD ["java", "-jar", "/opt/u1f596/standalone.jar"]

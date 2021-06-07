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
  diction

RUN yarnpkg global add --cwd /tmp @postlight/mercury-parser

RUN wget -O /tmp/pdf2htmlex.deb https://github.com/pdf2htmlEX/pdf2htmlEX/releases/download/v0.18.8.rc1/pdf2htmlEX-0.18.8.rc1-master-20200630-Ubuntu-focal-x86_64.deb && apt-get -y install /tmp/pdf2htmlex.deb && rm -f /tmp/pdf2htmlex.deb

RUN mkdir -p /opt/infowarss
COPY target/infowarss-0.1.0-SNAPSHOT-standalone.jar /opt/infowarss/standalone.jar
CMD ["java", "-jar", "/opt/infowarss/standalone.jar"]

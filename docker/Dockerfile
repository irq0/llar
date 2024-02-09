FROM ubuntu:jammy

LABEL org.opencontainers.image.source=https://github.com/irq0/llar
LABEL org.opencontainers.image.description="LLAR - Live Long and Read: Container Image"
LABEL org.opencontainers.image.licenses=GPL

RUN ln -fs /usr/share/zoneinfo/Europe/Berlin /etc/localtime

RUN DEBIAN_FRONTEND="noninteractive" apt-get update && apt-get -y --no-install-recommends install \
  yt-dlp \
  pandoc \
  w3m \
  lynx \
  html2text \
  poppler-utils \
  openjdk-21-jdk-headless \
  leiningen \
  wget \
  yarnpkg \
  git \
  diction \
  exiftool \
  locales \
  && rm -rf /var/lib/apt/lists/*


RUN yarnpkg global add --cwd /tmp @postlight/parser

RUN wget --no-verbose -O /tmp/pdf2htmlex.deb https://github.com/pdf2htmlEX/pdf2htmlEX/releases/download/v0.18.8.rc1/pdf2htmlEX-0.18.8.rc1-master-20200630-Ubuntu-focal-x86_64.deb && apt-get -y --no-install-recommends install /tmp/pdf2htmlex.deb && rm -f /tmp/pdf2htmlex.deb && rm -rf /var/lib/apt/lists/*

RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

RUN mkdir -p /opt/llar
COPY target/llar-0.1.0-SNAPSHOT-standalone.jar /opt/llar/standalone.jar

ENV LLAR_CONFIG /var/lib/llar/config.edn
ENV JDK_JAVA_OPTIONS "-XshowSettings:system -XX:+UseContainerSupport -XX:MaxRAMPercentage=60 -Dclojure.tools.logging.factory=clojure.tools.logging.impl/log4j2-factory -Dtika.config=tika-config.xml"

EXPOSE 8023
EXPOSE 9999
EXPOSE 42000

ENTRYPOINT ["java", "-jar", "/opt/llar/standalone.jar"]

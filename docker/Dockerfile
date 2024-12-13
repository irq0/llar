FROM ubuntu:24.04

ARG VERSION

LABEL org.opencontainers.image.source=https://github.com/irq0/llar
LABEL org.opencontainers.image.url=https://llar.dev
LABEL org.opencontainers.image.title="llar"
LABEL org.opencontainers.image.description="Live Long and Read: Container Image"
LABEL org.opencontainers.image.authors="llar@irq0.org"
LABEL org.opencontainers.image.licenses=AGPLv3
LABEL org.opencontainers.image.version=$VERSION

RUN ln -fs /usr/share/zoneinfo/Europe/Berlin /etc/localtime

RUN DEBIAN_FRONTEND="noninteractive" apt-get update && apt-get -y --no-install-recommends install \
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

RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

RUN mkdir -p /opt/llar /llar
COPY target/llar-$VERSION-standalone.jar /opt/llar/standalone.jar

ENV LLAR_CONFIG /llar/config.edn
ENV WEKA_HOME /llar/wekafiles
ENV JDK_JAVA_OPTIONS "-XshowSettings:system -XX:+UseContainerSupport -XX:MaxRAMPercentage=60 -Dclojure.tools.logging.factory=clojure.tools.logging.impl/log4j2-factory -Dtika.config=tika-config.xml"

RUN groupadd -g 42000 llar && \
   useradd -u 42000 -g llar llar \
   && chown -R llar:llar /llar
USER llar:llar

EXPOSE 8023
EXPOSE 9999
EXPOSE 42000

ENTRYPOINT ["java", "-jar", "/opt/llar/standalone.jar"]

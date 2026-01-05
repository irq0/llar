FROM ubuntu:24.04

ARG VERSION
ARG UBUNTU_MIRROR=http://archive.ubuntu.com/ubuntu
ENV DEBIAN_FRONTEND="noninteractive"

LABEL org.opencontainers.image.source=https://github.com/irq0/llar
LABEL org.opencontainers.image.url=https://llar.dev
LABEL org.opencontainers.image.title="llar"
LABEL org.opencontainers.image.description="Live Long and Read: Container Image"
LABEL org.opencontainers.image.authors="llar@irq0.org"
LABEL org.opencontainers.image.licenses=AGPLv3
LABEL org.opencontainers.image.version=$VERSION

RUN ln -fs /usr/share/zoneinfo/Europe/Berlin /etc/localtime

RUN sed -i "s|http://archive.ubuntu.com/ubuntu|${UBUNTU_MIRROR}|g" /etc/apt/sources.list && \
  apt-get update && apt-get -y --no-install-recommends install \
  pandoc \
  w3m \
  lynx \
  html2text \
  poppler-utils \
  openjdk-25-jdk-headless \
  leiningen \
  wget \
  npm \
  nodejs \
  git \
  diction \
  exiftool \
  locales \
  build-essential \
  software-properties-common \
  && rm -rf /var/lib/apt/lists/*

RUN add-apt-repository ppa:tomtomtom/yt-dlp && \
  apt-get update && apt-get -y --no-install-recommends install \
  yt-dlp

RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen

ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8

RUN mkdir -p /opt/llar /llar
COPY --chown=0:0 target/llar-$VERSION-standalone.jar /opt/llar/standalone.jar

ENV LLAR_CONFIG=/llar/config.edn
ENV WEKA_HOME=/llar/wekafiles
ENV JDK_JAVA_OPTIONS="-XshowSettings:system -XX:+UseContainerSupport -XX:MaxRAMPercentage=60 -Dclojure.tools.logging.factory=clojure.tools.logging.impl/log4j2-factory -Dtika.config=tika-config.xml"

COPY --chown=0:0 tools/ /llar/tools
COPY --chown=0:0 package.json /llar
COPY --chown=0:0 package-lock.json /llar

RUN groupadd -g 42000 llar && \
   useradd -u 42000 -g llar llar \
   && chown -R llar:llar /llar

RUN cd /llar && npm install && chown -R 0:0 /llar

USER llar:llar
WORKDIR /llar
EXPOSE 8023
EXPOSE 9999
EXPOSE 42000

ENTRYPOINT ["java", "-jar", "/opt/llar/standalone.jar"]

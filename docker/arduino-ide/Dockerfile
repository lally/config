FROM ubuntu:14.04
MAINTAINER Mael Auzias <docker@mael.auzias.net>

ENV HOME /home/arduino
ENV USER arduino

RUN apt-get update && apt-get install -y \
        libx11-6 libxext-dev libxrender-dev libxtst-dev \
        x11-utils suckless-tools \
        --no-install-recommends \
        && rm -rf /var/lib/apt/lists/* \
        && useradd --create-home --home-dir $HOME $USER \
        && chown -R $USER:$USER $HOME

ADD arduino-1.6.9-linux64.tar.xz $HOME
RUN sed -i 's/"-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"//g' /home/arduino/arduino-1.6.9/arduino
RUN sed -i 's/env bash/env bash\nwmname LG3D\n\n/' /home/arduino/arduino-1.6.9/arduino

WORKDIR $HOME/arduino-1.6.9
USER $USER

ENTRYPOINT ["/bin/bash"]

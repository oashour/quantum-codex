# syntax=docker/dockerfile:1

#FROM ubuntu:latest
#
#ENV LC_ALL=C.UTF-8
#ENV LANG=C.UTF-8
#
#RUN \
#    apt-get update              &&  \
#    apt-get upgrade --yes       &&  \
#    apt-get install --yes           \
#    python3-flask               \
#    python3-mysql.connector
#
#ADD app.py /srv/app.py
#ADD templates /srv/templates
#ENV FLASK_APP=/srv/app.py
#CMD ["flask", "run", "--host=0.0.0.0"]
#
#ADD docker-entrypoint.sh /srv/docker-entrypoint.sh
#RUN chmod +x /srv/docker-entrypoint.sh
#ENTRYPOINT ["/srv/docker-entrypoint.sh"]


FROM python:3.11

WORKDIR /dft-codex

COPY . .
RUN pip3 install '.[app]'

ENV FLASK_APP=codex.app

CMD [ "flask", "run", "--host=0.0.0.0"]

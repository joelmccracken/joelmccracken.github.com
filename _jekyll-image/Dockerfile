FROM alpine

ENV LANGUAGE=en_US
ENV LANG=en_US.UTF-8
ENV JEKYLL_VERSION=3.1.3
ENV JEKYLL_ENV=development
ENV TZ=America/New_York
ENV LC_ALL=en_US

RUN apk update
RUN apk add zlib-dev build-base libxml2-dev \
  libxslt-dev readline-dev libffi-dev ruby-dev \
  yaml-dev zlib-dev libffi-dev

RUN apk add zlib libxml2 ruby-io-console \
  readline libxslt ruby yaml libffi ruby-irb \
  ruby-json ruby-rake ruby-rdoc git openssl \
  nodejs tzdata

RUN addgroup -Sg 1000 jekyll
RUN adduser  -Su 1000 -G \
  jekyll jekyll

RUN mkdir -p /usr/share/ruby

RUN mkdir -p /jekyll && chown -R jekyll:jekyll /jekyll
RUN echo 'jekyll ALL=NOPASSWD:ALL' >> /etc/sudoers

RUN yes | gem install --force --no-ri --no-rdoc octopress -v3.0.0
RUN yes | gem install bundler -v=1.15.3  --no-ri --no-rdoc --force
RUN yes | gem install colorator -v=0.1 --no-ri --no-rdoc --force
RUN yes | gem install ffi -v=1.9.18 --no-ri --no-rdoc --force
RUN yes | gem install rb-fsevent -v=0.10.2 --no-ri --no-rdoc --force
RUN yes | gem install kramdown -v=1.14.0 --no-ri --no-rdoc --force
RUN yes | gem install liquid -v=3.0.6 --no-ri --no-rdoc --force
RUN yes | gem install mercenary -v=0.3.6 --no-ri --no-rdoc --force
RUN yes | gem install rouge -v=1.11.1 --no-ri --no-rdoc --force
RUN yes | gem install safe_yaml -v=1.0.4 --no-ri --no-rdoc --force
RUN yes | gem install multi_json -v=1.12.1 --no-ri --no-rdoc --force
RUN yes | gem install redcarpet -v=3.4.0 --no-ri --no-rdoc --force
RUN yes | gem install titlecase -v=0.1.1 --no-ri --no-rdoc --force
RUN yes | gem install stringex -v=2.7.1 --no-ri --no-rdoc --force
RUN yes | gem install octopress-deploy -v=1.3.0 --no-ri --no-rdoc --force
RUN yes | gem install rb-inotify -v=0.9.10 --no-ri --no-rdoc --force
RUN yes | gem install pygments.rb -v=1.1.2 --no-ri --no-rdoc --force
RUN yes | gem install sass-listen -v=4.0.0 --no-ri --no-rdoc --force
RUN yes | gem install listen -v=3.0.8 --no-ri --no-rdoc --force
RUN yes | gem install sass -v=3.5.1 --no-ri --no-rdoc --force
RUN yes | gem install jekyll-watch -v=1.5.0 --no-ri --no-rdoc --force
RUN yes | gem install jekyll-sass-converter -v=1.5.0 --no-ri --no-rdoc --force
RUN yes | gem install jekyll -v=3.1.6 --no-ri --no-rdoc --force
RUN yes | gem install octopress-escape-code -v=2.1.1 --no-ri --no-rdoc --force
RUN yes | gem install octopress-hooks -v=2.6.2 --no-ri --no-rdoc --force
RUN yes | gem install octopress -v=3.0.11 --no-ri --no-rdoc --force

RUN gem clean

WORKDIR /jekyll
CMD ["jekyll", "serve", "--drafts"]
VOLUME  /jekyll
EXPOSE 35729 4000
FROM silex/emacs:24.5

COPY org-convert.el /org-convert.el
COPY org-8.2.6 /org-8.2.6
COPY start-server.sh /start-server.sh

EXPOSE 9876

WORKDIR /
ENTRYPOINT ["/bin/sh"]
CMD ["/start-server.sh"]

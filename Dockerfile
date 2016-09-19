FROM node:6.6
EXPOSE 3000
ADD . /app
WORKDIR /app
ENV PATH $HOME/.local/bin:$PATH
CMD ./compile.sh
CMD ./start.sh
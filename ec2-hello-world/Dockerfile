FROM node:10.1.0
COPY . /app
WORKDIR /app
RUN npm install
RUN npm install pm2 -g
EXPOSE 3000
CMD ["npm","start"]

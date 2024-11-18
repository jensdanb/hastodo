# syntax=docker/dockerfile:1

# Comments are provided throughout this file to help you get started.
# If you need more help, visit the Dockerfile reference guide at
# https://docs.docker.com/go/dockerfile-reference/

# Want to help us make this template better? Share your feedback here: https://forms.gle/ybq9Krt8jtBL3iCk7

ARG NODE_VERSION=22

FROM node:${NODE_VERSION}-alpine

# Uncomment to use production node environment.
#ENV NODE_ENV production


WORKDIR /app

COPY package.json .
RUN npm i

# Copy the rest of the source files into the image.
COPY . .

RUN npm run build

RUN npm install serve -g

# Expose the port that the application listens on.
EXPOSE 3000

# Run the application.
CMD serve -s dist

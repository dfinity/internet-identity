FROM nginx:latest

ARG TEST_APP_CANISTER_ID
ARG II_CANISTER_ID
COPY ./nginx.conf /etc/nginx/nginx.conf

RUN sed -i "s/II_CANISTER_ID/${II_CANISTER_ID}/g" /etc/nginx/nginx.conf
RUN sed -i "s/TEST_APP_CANISTER_ID/${TEST_APP_CANISTER_ID}/g" /etc/nginx/nginx.conf
COPY ./certs /etc/nginx/certs

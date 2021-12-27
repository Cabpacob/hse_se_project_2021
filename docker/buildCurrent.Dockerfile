FROM haskell:9.0.1
WORKDIR /home/se-project-fall-2021
ADD . .
RUN stack setup
RUN stack build
RUN stack test

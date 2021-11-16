FROM haskell
WORKDIR /home/se-project-fall-2021
ADD . .
RUN stack setup
RUN stack run
FROM haskell
WORKDIR /home/se-project-fall-2021
RUN git clone --recursive https://github.com/Cabpacob/hse_se_project_2021.git .
RUN stack setup
RUN stack build
RUN stack test
# 451 Fahrenheit

[Project presentation](https://docs.google.com/presentation/d/1Ve6xeb4-FLGNJpRn_BId_PoTfhUm-ez01O-_th2Kiw4/edit?usp=sharing)


An ultimate application for creating tasks and automatically checking them.

## Installation (on your machine)

```
git clone https://github.com/Cabpacob/hse_se_project_2021
cd hse_se_project_2021
stack build
```

## Installation (under Docker)

_Note_: Following command should be executed 
from the project root.

There are two options avaiable.

- If you want to run your local version of project, then you should
use `docker/buildCurrent.Dockerfile`. Then all local local sources
will be build in the container.
    ```
    docker build -t build-f451 -f docker/buildCurrent.Dockerfile
    ```

- But if you want to try out
the latest avaiable version of project, then you may want to
use `docker/buildLatest.Dockerfile`. Then sources will be taken from
the `master` branch on this GitHub repository.
    ```
    docker build -t build-f451 -f docker/buildLatest.Dockerfile
    ```

## Execution (under Docker)

_Note_: Following command should be executed from the project root.

There are also two ways to execute demo application. The idea is the same as in the _Installation_ paragraph: there are `Current` and `Latest` versions. But this time execution
files have format `docker/run*.Dockerfile`.

-
    ```
    docker build -t exec-f451 -f docker/runCurrent.Dockerfile
    ```
-
    ```
    docker build -t exec-f451 -f docker/runLatest.Dockerfile
    ```

## Acknowledgments

We sincerely thank our families for their support on this difficult path, as well as Mike Mirzayanov for the excellent polygon and codeforces platforms.

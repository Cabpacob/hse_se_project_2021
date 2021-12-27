# 451 Fahrenheit

[Project presentation](https://docs.google.com/presentation/d/1Ve6xeb4-FLGNJpRn_BId_PoTfhUm-ez01O-_th2Kiw4/edit?usp=sharing)


An ultimate application for creating tasks and automatically checking them.

## Installation (on your machine)
Install stack:
```
apt install haskell-stack
```

Install F451:
```
git clone https://github.com/Cabpacob/hse_se_project_2021
cd hse_se_project_2021
stack build
```

## Installation (under Docker)

_Note_: Following command should be executed 
from the project root.

```
docker build -t build-f451 -f docker/buildCurrent.Dockerfile
```

## Execution (under Docker)

```
docker build -t exec-f451 -f docker/runCurrent.Dockerfile
```

## Acknowledgments

We sincerely thank our families for their support on this difficult path, as well as Mike Mirzayanov for the excellent polygon and codeforces platforms.


## Examples
### The largest ocean in the world?

`taskQuiz "The largest ocean in the world?" (OneOf ["Pacific", "Atlantic", "Other"]) 0 oneZeroConstraints`

Answers:
* `Just 4 -> Score 0`
* `Just 0 -> Score 1`
* `Just 1 -> Score 0`
* `Just 2 -> Score 0`
* `Nothing -> Score 0`

### Name all directions of the world
`taskMultipleRequired "Name all directions of the world" (AllOf [("North", 1), ("East", 1), ("West", 1), ("South", 1)]) oneZeroConstraints`

Answers:
* `Just ["North", "East", "West", "South"] -> Score 1`
* `Just ["South", "West", "East", "North"] -> Score 1`
* `Just ["South", "wEst", "eAst  "] -> Score 0.25`
* `Just ["  norTh", "wEst", "   eAst  "] -> Score 0`
* `Just [""] -> Score 0`
* `Just ["Three whales", "Giant turtle"] -> Score 0`
* `Just ["Three whales", "Giant turtle", "South", "Park"] -> Score 0.25`
* `Nothing -> Score 0`

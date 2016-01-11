# fsm-bar

## how to run in mathematica

there are two main files: `basic.rkt` which is used for the grand simulation.
`side.rkt` is for the side project (or spinoff).

before running, it's necessary to change the directory for the output.

- open `side.rkt`, the CONFIGURATION is at the beginning:

- change the directory to smt like: "/Users/linhchi.nguyen/Dropbox/fsm-bar/run1/mean.png"

- then change the variables accordingly, for example: N 100, CYCLES 100000...

- then run the file `run-in-matha.nb` in mathematica

in general (and inside mathematica), the command to run is

```
racket -tm side.rkt
```

with -tm and no further specification, racket will evaluate the function `main defined inside the file side.rkt

### how to run in lab 1 computer:

- change the directory for the output into a dropbox folder
(ie smt like: "/Users/linhchi.nguyen/Dropbox/fsm-bar/run1/mean.png")

```
(require (file "/Users/linhchi.nguyen/Documents/fsm-bar/main.rkt"))

```

### how to run in disa lab computer:
- change directory for output:
"C:/Documents and Settings/linhchi.nguyen/My Documents/Dropbox/fsm-bar/run6/mean"

```
(require (file "R:/fsm-bar/main.rkt"))
```

#### extra

```
raco test -s five main.rkt 
```

## To do

| To do         | Date          | By    | Note |
| ------------- |:-------------:| ----- |:----:|
| spin off from sample-fsm2      | 7 Dec | chi | |
| change "automata.rkt" to fit bargaining game | 7 Dec | ||
| add classic automata | 8 Dec |||
| plot the number of types in each cycle | x | |
| pass test 1 and test 2 of RD | x | ||
| fix out-rank |x|||
| run across delta | 11 dec |||
| write a montecarlo |||hm|
| optimize code, now time is spent in match-up and rank | |||
| write acknowledgement | x |||
| test 3 passed | 28 dec | ||
| output to dropbox |x|||
| draw the new automata to check |x|||
| run in matha | x |||
| restructure code | 30 dec|||
| print simulation settings as title | x |||
| able to run across N, rounds, deltas | x |||
| two different methods of deltas | 9 jan | ||
| set states# = 100 | x |||
| fix a bug: in deltas, in the for loop, build-random-population in each i | x || see if it solves the flat simulations |
| merge module c & d, from now on, run raco test for each module test | x |||
| ls the decision tree of an auto | x |||
| now contingent on the other action, ... (active states) |  |||


 
# Acknowledgment

This is a customised version built upon the base code of Matthias Felleisen [here](https://github.com/mfelleisen/sample-fsm)

The initial code of this project received a lot of critical contribution by Hoang Minh Thang.

Along the way of the development of this project, the code benefits tremendously from discussions on racket mailing list [here](https://groups.google.com/forum/?hl=en-GB#!topic/racket-users/4o1goSwrLHA), IRC #racket [here](http://pastebin.com/sxrCnwRV) and StackExchange.

The file "csv.rkt" has external copyright condition which can be found in its own file.

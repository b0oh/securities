# securuties
securities is a program to show some statistic from securities market

## Directories and files

### Directories
* `deps`: contain all dependencies, now it only yaws
* `ebin`: compiled erlang files
* `include`: contain header files
* `priv`: contain configs and misc files
* `priv/www`: contain static web files
* `src`: source files
* `tests`: tests for application

### Files
* `priv/securities.config`: settings for yaws server

## Install and Use

### Install
_tested on Ubuntu Server 10.04_

This command install all you need for running application

`sudo aptitude install git-core erlang build-essential autoconf libpam0g-dev`

Installing:

* `git clone git://github.com/b0oh/securities`
* `cd securities`
* `make build-deps` — it downloaded and cocmpiled yaws server
* `make test` for sure all works

Voila. Application installed.

### Use
I put some helpful scripts in Makefile

* `make build-deps`: call `make get-deps` that do git clone yaws repository;<br>
  check if there no configure file, it run autoconf, and do `./configure && make`
* `make test`: running tests
* `make run-dev`: running application in development environment
* `make run`: running application and detaching process;<br>
  to stop process you can use `ps aux | grep beam` to find pid and `kill -INT <pid>`

## API

### Types
* **Datatime** - should be string with format YYYY-MM-DD HH:II or YYYY-MM-DD HH:II:SS<br>
  MM ­— month, two digit<br>
  II — minutes, two digit

### HTTP
All request return JSON response on success.

HTTP Server handling 4 requests:

1. `GET /api/scales` — return list of scale names, downcase, strings
2. `GET /api/papers` — return list of exist papers in application, strings
3. `GET /api/entries?name=echo&from=2012-04-11%2000%3A00&to=2012-04-12%2000%3A00&scale=hour`<br>
   `name` and `scale` should be existing values, `from` and `to` should be _Datatime_<br>
   return list of entry, entry example:<br>
`{"startTime": "2012-04-11 00:00:00",<br>
  "startPrice": 1.21,<br>
  "endPrice": 1.26,<br>
  "minPrice": 1.21,<br>
  "maxPrice": 1.26,<br>
  "amount": 1200.0}`<br>
   `startPrice`, `endPrice`, `minPrice`, `maxPrice`, `amount` ­— float values, `startTime` — string
4. `POST /api/operation` — it require valid post data:
   * `name`
   * `time` — Datetime
   * `price` — float or integer
   * `amount` — float or integer

   **AHTUNG!** name is case sensitive: echo, Echo, ECHO it three different names 
# Wrecker-UI

This tool contains a small HTTP server and javascript application that can be used to store the results of different
`wrecker` runs and plot them.

It is meant to be used as a way to track progress between various runs of `wrecker` so it can be said confidently
that the performance of a particular work flow was affected in between code changes.

## Installation

The easiest way to use this tool is to download a binary from [the releases page](https://github.com/seatgeek/wrecker-ui/releases)


## Usage

You first need to start the HTTP server. It will automatically create a `wrecker.db` sqlite3 database in
the current directory:

```sh
wrecker-ui
```

If you want to run from a folder where the `assets` directory is not present. Then set the `WRECKER_ASSETS` env variable


```sh
WRECKER_ASSETS="/path/to/assets" wrecker-ui
```

The server will start in port `3000`. You can then visit [http://localhost:300/](http://localhost:300/) to start using the UI.
It wil not contain any data the first time. So you need to populate it yourself:


### Storing runs in the database

Use the `--output-path` option when executing `wrecker` to produce a `JSON` file. This file will be parsed and stored by this
application. Then, create the first run with the options you provided to `wrecker`:

```sh
curl -X POST 'http://localhost:3000/runs' \
	-d title="My Title" \
	-d groupName="Optional Group Name" \
	-d concurrency=100
```

The `groupName` param is used to display runs under the same group with the same color in the UI.
This is handy when you are calling `wrecker` in a batch script in order to figure out the application
behavior under different concurrency loads.

This will return an `id` you can use for future requests:

```json
{"success": true, id: 1}
```

Now, use the id to store the JSON result from the `wrecker` run:


```sh
curl -X POST 'http://localhost:3000/runs/1' \
	--data-binary @/path/to/file.json
```

You can now see you results in the web interface.


## Building from source

Dependencies:

- [Haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- [Elm 0.18](https://guide.elm-lang.org/install.html)

Install the `stack` tool from the link above. Then `cd` to the root folder of this repo and execute:

```sh
stack setup
stack install
```

If it is the first time, it will take *a lot* of time. Don't worry, it's only once you need to pay this price.

The UI is built using the `elm-make` command

```sh
elm-make UI.elm --output=assets/app.js
```

## Development

During development, it is recommended that you use `elm-live` for a quick feedback loop after changing the
Elm files:

```sh
npm install -g elm-live
```

Then you can run

```sh
elm-live UI.elm --dir=assets/ --output=assets/app.js --open
```

This will open the browser at an address wht will reload automatically on each code change.

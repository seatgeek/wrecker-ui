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

Typically, you would use `wrecker-ui` paired with a `wrecker` executable CLI command, containing your own tests. By default,
`wrecker-ui` looks for `wrecker` command present in PATH, but you can customize it by providing the `WRECKER_EXECUTABLE`
environment variable. The value of the variable needs to have the name of the executable in it:

```sh
WRECKER_EXECUTABLE="/path/to/wrecker-cli" wrecker-ui
```

If you want to run from a folder where the `assets` directory is not present. Then set the `WRECKER_ASSETS` env variable


```sh
WRECKER_ASSETS="/path/to/assets" wrecker-ui
```

The server will start in port `3000`. You can then visit [http://localhost:3000/](http://localhost:3000/) to start using the UI.
It wil not contain any data the first time. So you need to populate it yourself:

Alternatively you can change the default port using the `WRECKER_UI_PORT` env variable

```sh
WRECKER_UI_PORT=8000 wrecker-ui
```

### Using PostgreSQL

Wrecker-UI uses SQLite by default for storing the runs results. You can alternatively use PostgreSQL for this purpose when passing
a connection string via the `WRECKER_DB` env variable:


```sh
WRECKER_DB="postgres://user:pass@host/db_name" wrecker-ui
```

### Executing load testing runs

The Web interface is able to execute runs using wrecker. For this purpose it is required that your wrecker to provide test groups,
that is, the web interface is only usefule when you have used wrecker as a library to creat your own test suite.

Click on the `Schedule Test` link to see a list of tests available to wrecker and set the options for the run. Results should
shortly appear in the plot for the given test name.

### Manually storing runs in the database

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
{"success": true, "id": 1}
```

Now, use the id to store the JSON result from the `wrecker` run:


```sh
curl -X POST 'http://localhost:3000/runs/1' \
	--data-binary @/path/to/file.json
```

You can now see you results in the web interface.


## Distributing load testing accross nodes

Wrecker is limited by the amount of memory and CPU available to a single machine. For slow pages (triple digit milliseconds)
it is usually not a problem, as Haskell can very intelligently save memory while it is waiting for results to be transmitted
over the wire. But for a big concurrency number, specially in the presence of fast responding pages,
it is easy to exhaust the available resources.

Wrecker-UI allows you to distribute the testing load across nodes in the network, once the concurrency level for a test exceeds
2000 threads.

In order to distribute the load you need to first create the worker nodes, by starting wrecker-ui in slave mode in another machine:

```sh
WRECKER_ROLE=slave WRECKER_HOST=192.168.1.18 WRECKER_PORT=10501 wrecker-ui
```

The `WRECKER_HOST` env var is the IP or hostname that the worker will be bound to. If no variable is provided,
it will bind itself to `127.0.0.1` which is generally not very useful unless you are testing in a single machine.

You can start as many slaves as you need. Generally it makes no sense to start more than one node per physical machine.
After starting the worker nodes, you can start the master node. The master node is also the node that will display the
web interface.

You start the master node as described in the previous sections. Upon start, it should show something like this:

```
Found the following slave servers: [nid://192.168.1.18:15001:0]
```

Note: All nodes where wrecker-ui is running, will also need `wrecker` to be in the `$PATH`

### Static list of workers

Wrecker-UI uses a unicast broadcast to discover the workers in the network. This is sometimes not possible or desirable,
for example when deploying it in AWS EC2 servers. The workaround is to specify a static list of worker servers when
starting the master node:

```sh
WRECKER_SLAVES=192.168.1.18:15005,192.168.1.20:15001 wrecker-ui
```

Wrecker-UI does not currently verify that the slaves can actaully respond when specifying a static list. This is oftentimes
convenient as it does not impose any restrictions in the order the nodes need to be started.

## Building from source

Dependencies:

- [Haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- [Elm 0.18](https://guide.elm-lang.org/install.html)
- [Elm Install](https://github.com/gdotdesign/elm-github-install)

Install the `stack` tool from the link above. Then `cd` to the root folder of this repo and execute:

```sh
stack setup
stack install
```

If it is the first time, it will take *a lot* of time. Don't worry, it's only once you need to pay this price.

Currently, we require a fork of an official Elm package. Therefore, we are unable to use `elm-package` to install
dependencies. Instead, use `elm-install`. just executed it to the the Elm dependencies:

```sh
elm-install
```

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
elm-live UI/UI.elm --dir=assets/ --output=assets/app.js
wrecker-ui
```

Now you can use wrecker-ui as usual, but it will automatically reload the browser on any change done to the .elm files.

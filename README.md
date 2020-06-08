# Distant Socialing

The app is divided in a [client](https://github.com/nilehmann/binah-apps/tree/master/covid/client) written with [vue.js](https://vuejs.org/) and a [server](https://github.com/nilehmann/binah-apps/tree/master/covid/server) written with Binah.

## Development

### Get the code

Clone the repo including submodules

```bash
$ git clone --recursive https://github.com/nilehmann/binah-apps/
```

### Run the code

For local development you need to run both the [client](https://github.com/nilehmann/binah-apps/tree/master/covid/client#readme) and [server](https://github.com/nilehmann/binah-apps/tree/master/covid/server#readme). Se each README for further instruction.

## Production

### Build the code

To build just run:

```bash
$ ./build.sh
```

This will create a bundle files for the client app and compile the haskell code. It'll then copy everything to `dist/`.

### Run the code

In production the server is setted up to serve all the the static files and to respond with `index.html` for any other unknown route (routes are handled in the client by vue). To run just execute the haskell binary.

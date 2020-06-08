# client

## Project setup
Run yarn install 
```
yarn install
```

### Compiles and hot-reloads for development
This will start a development server for the client with hot reloading. The development client communicates with the local api server that needs to be running in 127.0.0.1:3000.

```
yarn serve
```

### Mock API
It is possible to run the client without running the backend by mocking api requests. The app will not be fully functional but in some cases it may be more comfortable to run in this mode if you are only working in client code. Check [api.mock.ts](https://github.com/nilehmann/binah-apps/blob/master/covid/client/src/services/api.mock.ts) to see what api calls have a mock implementation.

```
yarn serve --mode mock
```

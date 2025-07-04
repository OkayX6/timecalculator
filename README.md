# Feliz Template

This template gets you up and running with a simple web app using [Fable](http://fable.io/) and [Feliz](https://github.com/Zaid-Ajaj/Feliz).

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) v7.0 or higher
* [node.js](https://nodejs.org) v18+ LTS


## Editor

To write and edit your code, you can use either VS Code + [Ionide](http://ionide.io/), Emacs with [fsharp-mode](https://github.com/fsharp/emacs-fsharp-mode), [Rider](https://www.jetbrains.com/rider/) or Visual Studio.


## Development

Before doing anything, start with installing npm dependencies using `npm install`.

Then to start development mode with hot module reloading, run:
```bash
npm start
```
This will start the development server after compiling the project, once it is finished, navigate to http://localhost:8080 to view the application .

To build the application and make ready for production:
```
npm run build
```
This command builds the application and puts the generated files into the `deploy` directory (can be overwritten in webpack.config.js).

### Tests

The template includes a test project that ready to go which you can either run in the browser in watch mode or run in the console using node.js and mocha. To run the tests in watch mode:
```
npm run test:live
```
This command starts a development server for the test application and makes it available at http://localhost:8085.

To run the tests using the command line and of course in your CI server, you have to use the mocha test runner which doesn't use the browser but instead runs the code using node.js:
```
npm test
```

### TODO

- [ ] Sum all data on multiple lines
- [ ] Unit tests simple model
- [ ] favicon
- [ ] Optimise audio (lower latency)
- [ ] Calculation font: adjust line spacing and also, character spacing for large digits
- [ ] Explore other design & color palettes
  - [ ] Minimalist Highcontrast Monochrome / typographic palette: https://dribbble.com/shots/25837736-Fluentio-Brand-Identity-Campaign-Design-Visual-Language
  - [ ] Brutalist: https://dribbble.com/shots/12770179-Brutalism-Dashboard-UI-Data-Visualisation
- [x] Render simple model
- [x] Render multiple models
- [x] Page title 


#### QA
On ne devrait pas pouvoir écrire

P2
```
- [ ] 00002h
- [ ] 7h00000
- [ ] 28h
```

Incorrect transformations:
- [ ] 7h0 -> should be 7h00 when not focused instead of 7h

P1
Helpers do not work:

```
if model.LastLine.IsDisplayResult then...
```
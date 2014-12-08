This directory contains build tools for Shiny.


## Grunt

Grunt is a built tool that runs on node.js. Once node.js is installed, you can install grunt (run from this directory):

### Installing Grunt

```
sudo npm install -g grunt-cli
npm install grunt
```

You will also need to install some modules to run the grunt tasks:

```
npm install grunt-contrib-uglify
npm install grunt-contrib-jshint
npm install grunt-contrib-watch
npm install grunt-newer
```

### Using Grunt

To run all default grunt tasks (minification and jshint), simply go into the `tools` directory and run:

```
grunt
```

It's also useful to run `grunt` so that it monitors files for changes and run tasks as necessary. This is done with:

```
grunt watch
```

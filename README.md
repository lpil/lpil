# MM Russell Skin

## Setup

This skin has offline compile steps that are required before deploying it to
the site. Why? So we can write in SCSS, rather than CSS, and so we have one
Javascript file HTTP request, rather than many. Here's how to get started:

1. Clone this repository.
2. Open the project directory in the terminal.
3. Ensure that you have [node.js](http://nodejs.org/) installed.
4. Ensure that you have the `grunt` task runner CLI installed with `npm install
   -g grunt-cli`. If that doesn't work due to needing administrator privileges,
   try `sudo npm install -g grunt-cli`
5. Install the project dependencies with `npm install` from the project
   directory.
6. Install an [editorconfig plugin](http://editorconfig.org/#download) for your
   text editor, if one is available.

## Usage

From the project directory run `grunt` to start the task runner. Grunt will
then compile the SCSS into CSS, and concatenate the Javascript files into one
file. Each time you save a file in the SCSS or JS directory the appropriate
file will be recompiled.

The compiled CSS and JS can be found in the `output/` directory.

Pay close attention to the output of the output of the grunt program, as it
will alert you to errors in your Javascript and SCSS.

## Style guide

* Use spaces for indentation, not tabs.
* Use 2 space indentation.

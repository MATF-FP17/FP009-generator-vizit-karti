# VKGen

Simple, reausable, modular and template based business card generator.

## Design

A JSON object is provided as a template file to the form generator, which
generates the user input form.

After the user inputs the desired data - text, images, formatting info etc.,
the app outputs a JSON object consumed by the business card generator,
outputting an image containing the generated card.

## Feature checklist

Implemented features are marked with - _DONE_

### Major

* Load and parse template files
* Generate input form from parsed template object _DONE_
* Generate business card data _DONE_
* Generate image from business card data

### Nice to have

* WYSIWYG editing
* WYSIWYG template editing
* Printer friendly output

## Setup

### Install Gtk2HS

`
sudo apt-get install libghc-gtk-dev
`

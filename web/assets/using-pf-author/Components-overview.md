## Introduction

This page will give an overview of how we run Pipefish code in a book and make it interact with other components. We will go into the details of the components and their APIs in subsequent pages.

There are two kinds of components, Pipefish-centric components which begin with `pf-`, and widgets, which begin with `wj-`.

What we mean by Pipefish-centric components is that they have to know how to run or at least correctly render Pipefish code.

Widgets, on the other hand, interact with a Pipefish service but don't really know what Pipefish is, as will be explained in a later page.

## Pipefish-centric components

There are currently 11 of these which in principle someone *might* want to use as modules in a larger structure. These are all documented in [the main Pipefish documentation](****).

However, for your purposes you will probably want only three, which will be documented in this book.

* `pf-ide`. This gives access to an IDE in which you can edit a Pipefish project, with a TUI and a file-handler.
* `pf-service`. This is a non-visual component which you can use to power widgets when you don't want to expose any Pipefish code, but just want to present data in the widgets in an interactive way.
* `pf-tui`. This exposes the TUI of a service but not the code.

## Widgets

A widget is associated with the Pipefish service declared above it in the page, in a `pf-` component.

Widgets interact with Pipefish like this. Typically a widget will be pointed at a file in the virtual file system of the book it's in. It will automatically update itself when the file is updated, and will know how to render the file, e.g. a `wj-image` knows how to display a `.bmp` or `.jpg` or `.png` file; a `wj-sound` knows how to play a `.wav` or `.mp3` file.

The widget is automatically set up to send appropriate commands to the associated service in the event of (for example) clicking on a `wj-image`, or pressing enter in a `pf-query`. The service is under no obligation to implement these commands: error messages and `OK` alike are discarded.

The widgets cuttently available are as follows:

* `wj-image`: displays a `.bmp` or `.jpg` or `.png` file.
* `wj-sound`: plays a `.wav` or `.mp3` file.
* `wj-text` : inserts text into the book.

The will again be dealt with in detail on their own page.

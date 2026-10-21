## The file structure of a book

A well-formed Pipefish book consists of a folder of markdown files, the pages of the book, plus other assets which may be dumped in with the markdown but more sensibly occupy their own subfolders.

One of the pages must be `Contents.md`. You may not be aware that every valid Pipefish book has a `Contents.md` file, because there is no obligation to make it visible. However, it has to be there as a menifest of what the chapters of the book are, and what order the pages come in, and inded the title of the book. If you look in the sidebar you will find the "Contents" page listed under the last chapter heading, "Unused pages", and you may find it instructive to look at it and its raw markdown.

## The structure of the Contents file

### The header

The header of the `Contents.md` file looks schematically like this:

```
## Title of the book.

- [Icon](link to icon)
- [Logo](link to logo)
- [Description of link in top nav](link)
- [Description of other link in top nav](other link)
- [Description of last link in top nav](otherlast link)
```
It is not of course necessary to have any top nav. The `Icon` and `Logo` links specify the icon of the book in browser tabs, and the logo of the book in the header: if they are omitted then the icon will be the nazar emoji , and the logo will be Rene-the-Pipefish.

## The other sections

These are just chapter headings, with pages listed in them. So for example the `Contents.md` file of this book you're reading begins like this:

```
**********
```

Each page name will be converted to the name of an `.md` file by stripping out punctuation, replacing spaces with `-`, and adding the `.md` suffix: if there is no `.md` file in the book corresponding to a page name in `Contents.md`, it will be highlighted in the sidebar to alert you to this fact.

## The landing page

A direct link to a Pipefish book without specifying the page will always take you to the first page of the first chapter.

## Unused pages

The "Unused pages" chapter heading doesn't appear in the `Contents.md` file nor in the book as published: it's where Pipefish Author displays markdown files in a book which aren't listed in `Contents.md`, so that you can find and navigate to them.

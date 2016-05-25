####Build Status:
[![Build Status](https://travis-ci.org/DeveauP/RDevGraphlab.svg?branch=master)](https://travis-ci.org/DeveauP/RDevGraphlab)

# DevGRaph

DevGRaph is a project to allow easy visualization of package development.
It shows interactions between functions and statuses of functions.

## Interaction
Interactions are defined by a "who calls who" rule.
The goal is to list all functions that are called by a function inside of a package, and in non-base packages.
This should come in handy when keeping track of the imported function, see dependencies and look for broken links.

## Status
The status is extracted from the comments of the functions, thus is user-defined.
It should allow to check milestones in the development of a package, with statuses being:
 - unknown
 - ongoing
 - undocumented
 - complete

It should help in keeping track of the ongoing work.

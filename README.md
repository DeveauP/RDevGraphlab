# RDevGraphlab

RDevGraphLab is a project to simply visualize the development of a package.
It shows interactions between functions and statuses of functions.

## Interaction
Interactions are defined by a "who calls who" rule.
The goal is to list all functions that are called by a function inside of a package, and in non-base packages.
This should come in handy when keeping track of the imported function, see dependencies and look for broken links.

## Status
The status is extracted from the comments of the functions, thus is user-defined.
It should allow to check milestones in the development of a package, with statuses being:
 - unknow
 - ongoing
 - undocumented
 - complete

It should help in keeping track of the ongoing work.

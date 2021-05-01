===========
 Changelog
===========

0.3.0 (2021-05-01)
==================

* Fixed rendering of the blocks with only one
  children which is a tight container.
* Fixed rendering of workflows with multiple jobs in case
  if some jobs have workflows and some don't.

0.2.0 (2021-02-09)
==================

* Now it is possible to specify a branch.
  Just add as a parameter to URL: ``matrix.svg?branch=release``.
* Fixed rendering of a title for containers having only one item inside.
  Now badge should be more compact.

0.1.1 (2021-02-09)
==================

* When there is no runs of workflow, we will show a gray in-progress plate,
  to make it obvious, why there is no any results.

0.1.0 (2021-01-29)
==================

* Initial MVP.

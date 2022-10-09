===========
 Changelog
===========

0.4.1 (2022-10-09)
==================

* Fixed bug with trying to reuse closed SSL connection inside Dexador:
  https://github.com/fukamachi/dexador/issues/137

0.4.0 (2022-08-03)
==================

* Switched to Hunchentoot, because it is able to process parallel requests.

0.3.2 (2021-05-03)
==================

* Algorithm fixed to make badge more compact when there are
  nested containers having a single child.

0.3.1 (2021-05-03)
==================

* Fixed redering in case there are two different workflows with the same name
  one of which was disabled at the GitHub.

0.3.0 (2021-05-01)
==================

* Fixed rendering of the blocks with only one
  children which is a tight container.
* Fixed rendering of workflows with multiple jobs in case
  if some jobs have workflows and some don't.
* Fixed container's height calculation. And now all containers
  in a row are aligned to the bottom.
* Now all color boxes have the same font size.
* Added ability to select a subset of workflows/jobs to render.
  Use parameter ``only``. It should be a comma-separated list
  of paths, where each path consist of a workflow name, job name
  and job parameters, separated with dots::

    only=ci.run-tests.ubuntu-latest.ultralisp,ci.linter

* Footer with text "Rendered by github-actions.40ants.com" now disappears
  if only a single compact badge is rendered because of ``only`` argument
  effect. This feature should resolve issue
  https://github.com/40ants/github-matrix/issues/2

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
